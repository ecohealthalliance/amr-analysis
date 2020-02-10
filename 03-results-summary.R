library(tidyverse)
library(brms)
library(ggthemes)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(sf)
library(leaflet)
library(leaflet.extras)
library(httr)
library(sjPlot)
library(cowplot)
set.seed(101)

h <- here::here
source(h("R/functions.R"))

# Setup -------------------------------------------------------------------

# model results
fit_combined <- read_rds(h("model/fit_combined.rds"))
marg_effects <- read_rds(h("model/fit_all_marginal_effects.rds"))

# data - raw and imputed
amr_mice <- read_rds(h("model/mice-imputation.rds")) 
amr_with_imputes <- amr_mice %>% 
  mice::complete(.) %>% 
  select(-ln_ab_import_per_capita, -ln_livestock_pcu) %>%
  mutate(country = countrycode::countrycode(sourcevar = iso3c,
                                            origin = "iso3c",
                                            destination = "country.name"))
amr_raw <- amr_mice$data %>%
  select(-iso3c) %>%
  gather(key ="var", value = "x") %>%
  drop_na() %>%
  mutate(x_backtrans = ifelse(str_detect(var, "ln_"), exp(x), x))

# get posterior samples of data
beta_samples <- posterior_samples(fit_combined, subset = sample(nsamples(fit_combined), 500, replace = F)) 

# get zi vars (from logisitic regression)
zi_vars <- beta_samples %>%
  select(matches("zi_"), -b_zi_Intercept) %>%
  colnames(.) %>%
  gsub("^b_zi_", "", .)

# get pois vars
pois_vars <- beta_samples %>%
  select(-matches("zi_"), -b_Intercept, -lp__) %>%
  colnames(.)%>%
  gsub("^b_", "", .)

# event info
url_events <- "https://raw.githubusercontent.com/ecohealthalliance/amr-db/master/events-db.csv"
events <- GET(url_events, authenticate(Sys.getenv("GITHUB_USERNAME"), Sys.getenv("GITHUB_PAT")))
events <- read_csv(content(events, "text"))  %>%
  mutate(start_year = as.integer(substr(start_date, 1, 4))) %>%
  filter(start_year >= 2006) # removes promed mentions prior to 2006
n_distinct(events$study_id)

url_locs <- "https://raw.githubusercontent.com/ecohealthalliance/amr-db/master/data-processed/locations.csv"
locs <- GET(url_locs, authenticate(Sys.getenv("GITHUB_USERNAME"), Sys.getenv("GITHUB_PAT")))
locs <- read_csv(content(locs, "text")) %>%
  filter(study_id %in% events$study_id) %>%
  filter(!is.na(study_location)) 

# Summary and Coefficients ------------------------------------------------------------

summary(fit_combined)
brms::bayes_R2(fit_combined, summary = TRUE)

# odds ratio data
coefs <- get_model_data(fit_combined, type = "est") %>%
  distinct() %>%
  mutate(term_clean = as.character(term)) %>%
  mutate(term_clean = lookup_vars[term_clean]) %>%
  mutate(term_clean = fct_reorder(term_clean, estimate)) %>%
  mutate(est = round(estimate, 2)) %>%
  mutate(predictor = !(1 >= conf.low & 1 <= conf.high)) %>%
  mutate(lab = ifelse(predictor, paste0(est, "*"), est))

# which terms are consistent predictors
predictor_terms <- coefs %>%
  filter(predictor) %>%
  mutate(lab = "*") %>%
  select(term, lab)

# coefficient dot plots
# coefficient dot plots
ggplot(coefs, aes(x = term_clean, y = estimate)) + 
  geom_hline(yintercept = 1, color = "gray60") +
  geom_segment(aes(y = conf.low, yend = conf.high, xend = term_clean), color = "cornflowerblue") +
  geom_point(aes(color = group), show.legend = FALSE) +
  geom_text(aes(label = lab), nudge_x = 0.25) +
  scale_y_continuous(limits = c(0.25, 2)) +
  scale_color_manual(values = c("neg" = "cornflowerblue", "pos" = "cornflowerblue")) +
  labs(x = "", y = "Odds Ratio", title = "Model Coefficients - United States Removed") +
  coord_flip() +
  theme_foundation(base_size = 10, base_family =  "sans") + 
  theme(rect = element_rect(fill = "white", linetype = 0, colour = NA),
        title = element_text(size = rel(1), face = "bold"), 
        axis.text = element_text( size = rel(1)), 
        axis.ticks = element_blank(),
        axis.line = element_blank(), 
        plot.title.position = "plot",
        panel.grid.major = element_line(colour = "gray50", linetype = 3), 
        panel.grid.minor = element_blank())

ggsave(filename = h("plots/dot_plot.png"), width = 6, height = 4)

# Marginal effects --------------------------------------------------------
marg_effects_data <- imap_dfr(marg_effects, function(x, y){
  get_me_dat(x) %>%
    mutate(iteration = y) 
}) %>%
  mutate(value_backtrans = ifelse(str_detect(var, "ln_"), exp(value), value)) %>%
  mutate(var = factor(var, levels = names(lookup_vars))) 

marg_effects_avg <- marg_effects_data %>%
  group_by(value_backtrans, var) %>%
  summarize(mean = mean(estimate__)) %>%
  ungroup() 

predictors <- marg_effects_avg %>%
  group_by(var) %>%
  summarize(xval = 0.9 * max(value_backtrans),
            yval = 0.9 * max(mean)) %>%
  ungroup() %>%
  right_join(predictor_terms, by = c("var" = "term")) %>%
  mutate(var = factor(var, levels = names(lookup_vars)))

me_plots <- map(names(lookup_vars), function(lv){
  p <- ggplot(data = filter(marg_effects_data, var == lv), aes(x = value_backtrans)) + 
    geom_line(aes(y = estimate__, group = iteration), color = "cornflowerblue", size=.5, alpha = 0.4) +
    geom_line(data = filter(marg_effects_avg, var == lv), aes(x = value_backtrans, y = mean), size = 1.25) +
    geom_rug(data = filter(amr_raw, var ==lv), mapping = aes(x = x_backtrans)) +
    scale_y_continuous(limits = c(0, 10), 
                       breaks = c(0, 2.5, 5, 7.5, 10), 
                       labels = c("0", "", "5", "", "10")) +
    labs(title = lookup_vars[lv], y = "", x="") +
    theme_foundation(base_size = 12, base_family =  "sans") + 
    theme(rect = element_rect(fill = "white", linetype = 0, colour = NA),
          title = element_text(size = rel(1.1)), 
          axis.text = element_text( size = rel(1)), 
          axis.ticks = element_blank(),
          axis.line = element_blank(), 
          panel.grid.major = element_line(colour = "gray50", linetype = 3), 
          panel.grid.minor = element_blank()
    )
  if(lv == "ln_population"){
    p <- p + scale_x_log10()
  }
  if(lv %in% c("ln_pubcrawl_per_capita", "ln_gdp_per_capita")){
    p <- p + scale_x_continuous(labels = function(x) format(x, scientific = TRUE))
  }
  if(lv %in% predictors$var){
    p <- p +
      geom_text(data = predictors %>% filter(var == lv), aes(label = lab, x = Inf, y = Inf),
                hjust = 2, vjust = 1.5, size = 14) 
  }
  return(p)
})

plot_grid(plotlist=me_plots, 
          ncol = 2) 
ggsave(filename = h("plots/marginal_effects_multi.png"), width = 10, height = 21)

# Zi logistic model ---------------------------------------------------------------

# Get Zi partial effects
out_zi <- map_dfr(zi_vars, function(var){
  
  minx <- amr_with_imputes %>% pull(var) %>% min(., na.rm = T)
  maxx <- amr_with_imputes %>% pull(var) %>% max(., na.rm = T)
  seqx <- seq(from = minx, to = maxx, length.out = 100)
  
  betas <- as.matrix(beta_samples[,grep("zi", colnames(beta_samples))])
  
  X <- matrix(
    c(1, colMeans(as.matrix(amr_with_imputes[,zi_vars]))),
    nrow = length(zi_vars) + 1, ncol = length(seqx),
  )
  rownames(X) <- c("Intercept", zi_vars)
  
  assertthat::assert_that(all(rownames(X) ==  gsub("^b_zi_", "", colnames(betas))))
  
  X[var, ] <- seqx
  Y <- plogis(betas %*% X)
  out <- as_tibble(Y) %>% 
    mutate(samp = 1:nrow(Y)) %>% 
    gather("x", "y", -samp) %>% 
    mutate(x = rep(seqx, each = nrow(betas)),
           var = var)
  
  return(out)
})

# reverse axis
out_zi <- out_zi %>% mutate(y = 1-y)

# labels for x axis
out_zi <- out_zi %>% 
  mutate(x_backtrans = ifelse(grepl("ln_", var), exp(x), x))

# summarize
out_zi_sum <- out_zi %>% 
  group_by(x, x_backtrans, var) %>% 
  summarise(med = median(y),
            lo = quantile(y, .025),
            hi = quantile(y, .975)) %>%
  ungroup()

zi_plots <- map(zi_vars, function(zv){
  p <-   ggplot() + 
    geom_line(data = filter(out_zi, var==zv), aes(x = x_backtrans, y = y, group = samp), color = "gray60", size=.5, alpha = 0.2) + #For lots of lines
    geom_line(data = filter(out_zi_sum, var==zv), aes(x = x_backtrans, y = med), size = 1) +
    geom_line(data = filter(out_zi_sum, var==zv), aes(x = x_backtrans, y = lo),  size = 0.5, alpha = 0.8) +
    geom_line(data = filter(out_zi_sum, var==zv), aes(x = x_backtrans, y = hi),  size = 0.5, alpha = 0.8) +
    geom_rug(data = filter(amr_raw, var==zv), mapping = aes(x = x_backtrans)) +
    scale_y_continuous(limits = c(0,1)) +
    labs(y = "", x = "", title = lookup_vars[zv]) +
    theme_foundation(base_size = 10, base_family =  "sans") + 
    theme(rect = element_rect(fill = "white", linetype = 0, colour = NA),
          title = element_text(size = rel(1)), 
          axis.text = element_text( size = rel(1)), 
          axis.ticks = element_blank(),
          axis.line = element_blank(), 
          panel.grid.major = element_line(colour = "gray50", linetype = 3), 
          panel.grid.minor = element_blank()
    )
          
  if(zv == "ln_population"){
    p <- p + scale_x_log10()
  }
  if(zv %in% c("ln_pubcrawl_per_capita", "ln_gdp_per_capita")){
    p <- p + scale_x_continuous(labels = function(x) format(x, scientific = TRUE))
  }
  return(p)
})

plot_grid(plotlist=zi_plots, 
          ncol = 3) 
ggsave(filename = h("plots/zi_partial_effects.png"), width = 12, height = 6)

# Poisson model ---------------------------------------------------------------

# Get poisson partial effects
out_pois <- map_dfr(pois_vars, function(var){
  
  minx <- amr_with_imputes %>% pull(var) %>% min(., na.rm = T)
  maxx <- amr_with_imputes %>% pull(var) %>% max(., na.rm = T)
  seqx <- seq(from = minx, to = maxx, length.out = 100)
  
  betas <- cbind(as.matrix(beta_samples[,c(grep("b_[^z]", colnames(beta_samples)))]), 1)
  colnames(betas)[ncol(betas)] <- "offset"
  
  X <- matrix(
    c(1, colMeans(as.matrix(amr_with_imputes[,c(pois_vars, "ln_population")]))),
    nrow = length(pois_vars) + 2, ncol = length(seqx),
  )
  rownames(X) <- c("Intercept", pois_vars, "offset") 
  
  assertthat::assert_that(all(rownames(X) ==  gsub("^b_", "", colnames(betas))))
  
  X[var, ] <- seqx
  Y <- exp(betas %*% X)
  out <- as_tibble(Y) %>% 
    mutate(samp = 1:nrow(Y)) %>% 
    gather("x", "y", -samp) %>% 
    mutate(x = rep(seqx, each = nrow(betas)),
           var = var)
  
  return(out)
})

# labels for x axis
out_pois <- out_pois %>% 
  mutate(x_backtrans = ifelse(grepl("ln_", var), exp(x), x))

# summarize
out_pois_sum <- out_pois %>% 
  group_by(x, x_backtrans, var) %>% 
  summarise(med = median(y),
            lo = quantile(y, .025),
            hi = quantile(y, .975))

pois_plots <- map(pois_vars, function(pv){
  p <-   ggplot() + 
    geom_line(data = filter(out_pois, var==pv), aes(x = x_backtrans, y = y, group = samp), color = "gray60", size=.5, alpha = 0.2) + #For lots of lines
    geom_line(data = filter(out_pois_sum, var==pv), aes(x = x_backtrans, y = med), size = 1) +
    geom_line(data = filter(out_pois_sum, var==pv), aes(x = x_backtrans, y = lo),  size = 0.5, alpha = 0.8) +
    geom_line(data = filter(out_pois_sum, var==pv), aes(x = x_backtrans, y = hi),  size = 0.5, alpha = 0.8) +
    geom_rug(data = filter(amr_raw, var==pv), mapping = aes(x = x_backtrans)) +
    # scale_y_continuous(limits = c(0, 10), 
    #                    breaks = c(0, 2.5, 5, 7.5, 10), 
    #                    labels = c("0", "", "5", "", "10")) +
    labs(y = "", x = "", title = lookup_vars[pv]) +
    theme_foundation(base_size = 10, base_family =  "sans") + 
    theme(rect = element_rect(fill = "white", linetype = 0, colour = NA),
          title = element_text(size = rel(1)), 
          axis.text = element_text( size = rel(1)), 
          axis.ticks = element_blank(),
          axis.line = element_blank(), 
          panel.grid.major = element_line(colour = "gray50", linetype = 3), 
          panel.grid.minor = element_blank()
    )
  
  if(pv == "ln_population"){
    p <- p + scale_x_log10()
  }
  if(pv %in% c("ln_pubcrawl_per_capita", "ln_gdp_per_capita")){
    p <- p + scale_x_continuous(labels = function(x) format(x, scientific = TRUE))
  }
  return(p)
})

plot_grid(plotlist=pois_plots, 
          ncol = 3) 
ggsave(filename = h("plots/pois_partial_effects.png"), width = 12, height = 12)


# Generate predictions --------------------------------------------
amr_predict <- amr_with_imputes %>%
  mutate(mod_predict = predict(fit_combined)[,1]) %>%
  select(iso3c, mod_predict, n_amr_events)

# get means and predict doubling for each vars
amr_means <- colMeans(as.matrix(amr_with_imputes[,unique(c(zi_vars, pois_vars))])) %>%
  enframe() %>%
  pivot_wider(names_from = name, values_from = value)
mean_estimate <- predict(fit_combined, newdata = amr_means, summary = TRUE)

imap(amr_means, function(x, y){
  dbl_estimate <- amr_means %>%
    mutate(!!y := ifelse(str_detect(y, "ln_"), log(exp(x) * 2), x * 2)) %>%
    predict(fit_combined, newdata = .)
  dbl_estimate[1]/mean_estimate[1]
})

# matrix of beta samples 
betas <- cbind(as.matrix(beta_samples[,c(grep("b_[^z]", colnames(beta_samples)))]), 1)
colnames(betas)[ncol(betas)] <- "b_ln_population"

# matrix of data by country
X <- cbind("Intercept" = 1 , amr_with_imputes[,c(pois_vars, "ln_population")]) %>%
  as.matrix(.) %>%
  t()
colnames(X) <- amr_with_imputes$iso3c
X2 <- X
X2["ln_population",] <- 0 # set population to zero to calculate rate

# calc poisson predictions for each country for each beta sample
assertthat::assert_that(all(rownames(X) ==  gsub("^b_", "", colnames(betas))))
Y <- exp(betas %*% X)
Y2 <- exp(betas %*% X2)

# post process
pois_predicts <- as_tibble(Y) %>% 
  mutate(samp = 1:nrow(Y)) %>% 
  gather("iso3c", "pois_predict", -samp) %>%
  mutate(v = "mean_pop")

Y2 <- as_tibble(Y2) %>% 
  mutate(samp = 1:nrow(Y2)) %>% 
  gather("iso3c", "pois_predict", -samp) %>%
  mutate(v = "zero_pop")

pois_predicts <- bind_rows(pois_predicts, Y2) %>%
  group_by(iso3c, v) %>%
  summarise(med = median(pois_predict),
            lo = quantile(pois_predict, .025),
            hi = quantile(pois_predict, .975)) %>%
  ungroup() %>%
  left_join(amr_predict) %>% # would need full model equation for distributions of predictions 
  mutate(continent = countrycode(sourcevar = iso3c,
                                 origin = "iso3c",
                                 destination = "continent"),
         country = countrycode(sourcevar = iso3c,
                               origin = "iso3c",
                               destination = "country.name"))

# view largest differences between predicted and acctual
diffs <- pois_predicts %>%
  filter(v == "mean_pop") %>%
  mutate(diff = med - n_amr_events) %>%
  mutate(country_lab = paste0(country, " (", round(lo, 0), " - ", round(hi,0), ")")) %>%
  arrange(-abs(diff))

mean(diffs$diff)
diffs %>% filter(diff > 0) %>% nrow()

diffs_reshape <- diffs %>%
  select(country_lab, n_amr_events, med, diff) %>%
  mutate(increase = diff > 0) %>%
  mutate(top_10 = row_number() <= 10) %>%
  gather(-country_lab, -increase, -top_10, -diff, key = "key", value = "value") %>%
  mutate(key = factor(key, levels = c("n_amr_events", "med"), labels = c("Reported", "Predicted"))) 

diffs_reshape_top10 <- diffs_reshape %>% filter(top_10==TRUE)

# slope graph
slope_graph <- ggplot() +
  geom_line(data = diffs_reshape, aes(x = key, y = value, group = country_lab), color = "gray40", alpha = 0.2) +
  geom_point(data = diffs_reshape, aes(x = key, y = value, group = country_lab), color = "gray40", alpha = 0.2) +
  geom_line(data = diffs_reshape_top10, aes(x = key, y = value, group = country_lab)) +
  geom_point(data = diffs_reshape_top10, aes(x = key, y = value, group = country_lab)) +
  geom_text(data = filter(diffs_reshape_top10, key == "Predicted", ), aes(label = country_lab, x = key, y = value),
            hjust = "outward", nudge_x = 0.02) +
  scale_x_discrete(expand = c(0.1, 0,0 ,0.5))+
  # scale_color_manual(values = c(`TRUE` = "hotpink",
  #                               `FALSE` = "blue")) +
  labs(x = "", y = "", title = "")  +
  theme_foundation(base_size = 12, base_family =  "sans") + 
  theme(rect = element_rect(fill = "white", linetype = 0, colour = NA),
        title = element_text(size = rel(1), face = "bold"), 
        axis.text = element_text( size = rel(1)), 
        axis.ticks = element_blank(),
        axis.line = element_blank(), 
        legend.position ="none",
        panel.grid.major =  element_blank(), 
        panel.grid.minor = element_blank())


ggsave(slope_graph, filename = h("plots/slope_graph.png"), width = 8, height = 8)

# Map predictions ---------------------------------------------------------

# Get adm
admin <- ne_countries(type='countries', scale = 'large') %>%
  st_as_sf() %>%
  mutate(iso3c = countrycode(sourcevar = name,
                             origin = "country.name",
                             destination = "iso3c"),
         iso3c = ifelse(is.na(iso3c), iso_a3_eh, iso3c)) %>%
  select(name, iso3c)

admin <- left_join(admin, pois_predicts, by = "iso3c") %>%
  mutate(med = ifelse(v=="mean_pop", round(med, 0), med*10000)) %>%
  select(name, iso3c, v, "Reported AMR Events" = n_amr_events, "Predicted AMR Events" = med) 

admin_mean <- admin %>%
  filter(is.na(v) | v == "mean_pop") %>% 
  select(-v) %>%
  gather(-name, -iso3c, -geometry, key = "key", value = "value") %>%
  mutate(key = factor(key, levels = c("Reported AMR Events", "Predicted AMR Events"), labels = c("Reported", "Predicted"))) %>%
  filter(name != "Antarctica")

pal1 <- colorNumeric("OrRd", domain = c(admin_mean$`Predicted AMR Events`, admin_mean$`Reported AMR Events`), na.color = "#e9e9f0")

caption <- glue::glue(nrow(events), " AMR emergence events<br/>",
                      n_distinct(events$study_country), " countries<br/>",
                      n_distinct(events$drug), " antimicrobial drugs<br/>",
                      n_distinct(events$bacteria), " resistant bacteria<br/>",
                      str_sub(min(events$start_date), 1, 4), " - ",  str_sub(max(events$start_date), 1, 4))

map_predictions <- ggplot(admin_mean) + 
  geom_sf(aes(fill = value), color = "transparent") +
  facet_wrap(key~., strip.position="top", ncol = 1) +
  scale_fill_viridis_c(option = "plasma", alpha = 0.8) +
  labs(fill = "AMR Emergence Count") +
  theme_foundation(base_size = 14, base_family =  "sans") + 
  coord_sf() +
  theme(strip.background = element_blank(), 
        strip.text = element_text(size = rel(1)), 
        rect = element_rect(fill = "white", linetype = 0, colour = NA),
        #title = element_text(size = rel(1.1), face = "bold"), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.line = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        legend.position = "none")
        # legend.title = element_text(size = rel(0.9)),
        # legend.text = element_text(size = rel(0,8)), 
        # legend.position = "left")

ggsave(map_predictions, filename = h("plots/map_predictions.png"), width = 12, height = 8)

plot_combined <- plot_grid(map_predictions, slope_graph, labels = "AUTO")
ggsave(plot_combined, filename = h("plots/map_and_slope.png"), width = 14, height = 8)


admin_mean2 <- admin %>%
  filter(v == "mean_pop")

lf1 <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addFullscreenControl(position = "topright") %>%
  addPolygons(data = admin_mean2, 
              stroke = TRUE, color = "#46464a", weight = 1,
              fill = TRUE, fillColor = ~pal1(`Predicted AMR Events`), fillOpacity = 0.9,
              label = ~paste0(name, ": ", `Predicted AMR Events`), group = "Predicted") %>%
  addPolygons(data = admin_mean2, 
              stroke = TRUE, color = "#46464a", weight = 1,
              fill = TRUE, fillColor = ~pal1(`Reported AMR Events`), fillOpacity = 0.9,
              label = ~paste0(name, ": ", `Reported AMR Events`), group = "Reported") %>%
  addCircleMarkers(data = locs,  radius = 3,
                   lng = ~jitter(lon_study), lat = ~jitter(lat_study), 
                   stroke = TRUE, color = "#210106", opacity = 1, weight = 1,
                   fill = TRUE, fillColor = "#210106", fillOpacity = 0.5,
                   label = ~study_location, group = "Reported") %>%
  addLegend(data = admin_mean2, pal = pal1, values = ~`Predicted AMR Events`, position = "bottomright", title = "AMR Emergence Events") %>% 
  addControl(caption) %>%
  addLayersControl(baseGroups = c("Reported", "Predicted"), options = layersControlOptions(collapsed = FALSE), position = "bottomleft")  

lf1

htmlwidgets::saveWidget(lf1, h("plots/map_predictions_interactive.html"))
