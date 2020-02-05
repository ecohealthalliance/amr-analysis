library(tidyverse)
library(brms)
library(bayesplot)
h <- here::here

color_scheme_set("viridis") 

# Model results
fit_combined <- read_rds(h("model/fit_combined.rds"))

# Data
amr_mice <- read_rds(h("model/mice-imputation.rds")) 

amr_with_imputes <- amr_mice %>% 
  mice::complete(.) %>% 
  select(-ln_ab_import_per_capita, -ln_livestock_pcu) %>%
  mutate(country = countrycode::countrycode(sourcevar = iso3c,
                                            origin = "iso3c",
                                            destination = "country.name")  )
# Generate posterior predictions
y <- amr_with_imputes$n_amr_events
yrep <- posterior_predict(fit_combined, nsamples = 1000) 
yord <- order(y)
yrep <- yrep[,yord]
y <- y[yord]

# Trace Plots
params <- mcmc_trace_data(fit_combined)$parameter %>% as.character() %>% unique()
params_abbr <- str_remove_all(params, "_per_capita|_perc|_kg|_ddd")
names(params_abbr) <- params
global_labeller <- labeller(
  parameter = params_abbr
)
trace_plot <- mcmc_trace(fit_combined, facet_args = list(labeller = global_labeller)) + facet_text(size = 8) 
cowplot::plot_grid(plotlist = list(trace_plot), labels = c("A"))
ggsave(filename = h("plots/diagnostics/trace.png"), width = 15, height = 15)

# Histograms...doesn't work
test = as.array(fit_combined)[,1,1:2]
dimnames(test)
hist_plot <- mcmc_rank_hist(test, n_bins = 40)
ggsave(hist_plot, filename = h("plots/diagnostics/histogram.png"), width = 15, height = 15)

# Density overlay plots
dens_plot <- ppc_dens_overlay(y, yrep) + 
  scale_y_continuous(limits = c(0, 1)) +
  scale_x_continuous(limits = c(0, 50))
ggsave(dens_plot, filename = h("plots/diagnostics/density.png"))


# Proportion zero plots
prop_zero <- function(x) mean(x == 0)
zero_plot <- ppc_stat(y, yrep, stat = "prop_zero", binwidth = 0.005)
ggsave(zero_plot, filename = h("plots/diagnostics/prop_zero.png"))

# Interval plots
int_dat <- ppc_intervals_data(y, yrep) 
int_dat <- int_dat %>%
  filter(y_obs>0) %>%
  mutate(in_50 = y_obs >= l & y_obs <= h,
         in_90 = y_obs >= ll & y_obs <= hh)

p50 <- round(100*sum(int_dat$in_50)/nrow(int_dat))
p90 <- round(100*sum(int_dat$in_90)/nrow(int_dat))

interval_plot <- ppc_intervals(y, yrep)# + labs(title = "Observations versus Predictions (individual observations)", caption = paste0("dark line = 50% probability\nfaded line = 90% probability\n", p50, "% non-zeros in 50% prob\n", p90,  "% non-zeros in 90% prob"))
ggsave(interval_plot, filename = h("plots/diagnostics/intervals.png"), width = 6)

y_grped <- tibble(y) %>%
  mutate(grp = cut(y, breaks = c(0,  1, 10, 20, 50, Inf), right = FALSE)) %>%
  group_by(grp) %>%
  mutate(n = n()) %>%
  ungroup() %>%
  mutate(grp = paste0(grp, " (n = ", n, ")")) %>%
  dplyr::select(-n)

y_grped_sum <- y_grped %>%
  group_by(grp) %>%
  summarize(med = median(y)) %>%
  mutate(ytype = "y")

colnames(yrep) <- y_grped$grp

yrep_grped <- map_df(unique(colnames(yrep)), function(x){
  
  grp <- yrep[, x]
  med <- median(grp)
  prob_50_upper <- quantile(grp, 0.75)
  prob_50_lower <- quantile(grp, 0.25)
  prob_90_upper <- quantile(grp, 0.95)
  prob_90_lower <- quantile(grp, 0.05)
  
  tibble(grp = x, med, prob_50_upper, prob_50_lower, prob_90_upper, prob_90_lower, ytype = "yrep")
  
})

yall <- bind_rows(y_grped_sum, yrep_grped)

ggplot(data = yall, aes(x = grp)) + 
  geom_errorbar(aes(ymin = prob_50_lower, ymax = prob_50_upper), color = color_scheme_get()[2], width=0, size = 1.5) +
  geom_errorbar(aes(ymin = prob_90_lower, ymax = prob_90_upper), color = color_scheme_get()[2], width=0, alpha = 0.3, size = 1.5) +
  geom_point(aes(x = grp, y = med, color = ytype, fill = ytype), pch=21, size = 3) +
  scale_fill_manual(values = c("y" = unname(color_scheme_get()[6]), "yrep" = unname(color_scheme_get()[1]))) +
  scale_color_manual(values = c("y" = unname(color_scheme_get()[5]), "yrep" = unname(color_scheme_get()[2]))) +
  labs(title = "Observations versus Predictions(grouped by intervals)", caption = "dark line = 50% probability; faded line = 90% probability; points are medians", x = "", y = "", color = "", fill = "")

ggsave(filename = h("plots/diagnostics/intervals_grouped.png"), width = 6)

# all plots
all_plots <- list(dens_plot, interval_plot, zero_plot)
cowplot::plot_grid(plotlist=all_plots, labels = c("B", "C", "D"), nrow = 1)
ggsave(filename = h("plots/diagnostics/all.png"), width = 12, height= 4)
