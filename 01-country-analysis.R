#'---
#' title: "exploratory_analysis"
#' output: github_document
#' always_allow_html: yes
#' ---
#' 
#' 
#+ r setup, include = FALSE
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE, fig.width=10)
library(tidyverse)
library(magrittr)
library(here)
library(broom)
library(mgcv)
library(dbarts)
library(DALEX)
library(ceterisParibus)
library(PerformanceAnalytics)

set.seed(101)

# prep data
country_dat <- read_csv(here::here("country_level_amr.csv")) %>%
  na.omit() %>%
  mutate(wb_gdp_dollars = log10(wb_gdp_dollars/1000000000),
         wb_population = log(wb_population),
         oec_ab_import = log(oec_ab_import),
         oec_ab_import = ifelse(is.infinite(oec_ab_import), 0, oec_ab_import),
         oec_ab_export = log(oec_ab_export),
         oec_ab_export = ifelse(is.infinite(oec_ab_export), 0, oec_ab_export),
         continent = as.factor(continent),
         region = as.factor(region)) %>%
  rename(wb_gdp_billion_log = wb_gdp_dollars,
         wb_population_log = wb_population,
         oec_ab_import_log = oec_ab_import,
         oec_ab_export_log = oec_ab_export)

# create reshape version
country_dat_rs <- country_dat %>% 
  gather(key = "var", value = "val", -n_amr_events, -continent, -region, - country, -iso3c) 
#' -----------------View Data-----------------
#+ r plots
ggplot(data = country_dat_rs %>%
         bind_rows(country_dat_rs %>%
                     select(n_amr_events, iso3c) %>%
                     rename(val = n_amr_events) %>%
                     mutate(var = "n_amr_events") %>% 
                     distinct()),
       mapping = aes(x = val)) +
  geom_histogram() +
  facet_wrap(var ~ ., scales = "free") +
  theme_bw()

chart.Correlation(country_dat %>%
                    select(-continent, -region, - country, -iso3c),
                  histogram=FALSE, pch=19)

#' -----------------Fit GAM-----------------
#+ r mod-gam
# sp = smoothing parameter (higher = smoother, can be fit with method = "REML")
# k  = number of base curves

gam_mod <- gam(data = country_dat, 
               formula = n_amr_events ~  
                 s(wb_gdp_billion_log) + 
                 s(wb_population_log) +
                 #s(wb_livestock_index) +
                 s(wb_ag_land_perc, k = 15) + 
                 #s(wb_health_expend_perc) +
                 #s(oec_ab_import_log) +
                 s(oec_ab_export_log) +
                 s(pubs_sum) +
                 english_spoken,
                 #continent,
               method = "REML",
               family = "quasipoisson")

summary(gam_mod) # higher EDF = more wiggly (1 = linear)
concurvity(gam_mod, full = TRUE) # Concurvity occurs when some smooth term in a model could be approximated by one or more of the other smooth terms in the model. 
gam.check(gam_mod)

# plot(gam_mod, page=1,
#      all.terms = TRUE,
#      residuals = TRUE, pch = 1, cex = 1, shade = TRUE, shade.col = "lightblue",
#      seWithMean = TRUE, shift = coef(gam_mod)[1])

#' -----------------Fit BART-----------------
#+ r mod-bart
bart_mod <- bart(country_dat %>% dplyr::select(-n_amr_events, -iso3c, -country, -continent),
                 country_dat$n_amr_events,
                 ndpost=1000,
                 keeptrees = TRUE)

#' -----------------Compare models-----------------
#+ r mod-comp

# model results
country_dat %<>% mutate(id = row_number())
gt <- tibble(gam = predict(gam_mod, type="response"), id = country_dat %>% filter(!is.na(pubs_sum), !is.na(wb_gdp_billion_log)) %>% pull(id))
bt <- tibble(bart = bart_mod$yhat.train.mean, id = country_dat %>% na.omit() %>% pull(id))

# add in model predictions
country_dat2 <- country_dat %>%
  left_join(gt) %>%
  left_join(bt) %>%
  dplyr::select(-continent, -region, -country, -iso3c, -id) 

# reshape
country_dat_rs2 <- country_dat2  %>%
  gather(key = "var", value = "x", -n_amr_events, -gam, -bart) %>%
  gather(key = "model", value = "predicted", -x, -n_amr_events, -var) %>%
  mutate(residual = n_amr_events - predicted,
         var = factor(var, levels = c("pubs_sum",
                                      "wb_gdp_billion_log",
                                      "wb_population_log",
                                      "wb_health_expend_perc",
                                      "wb_ag_land_perc",
                                      "wb_livestock_index",
                                      "oec_ab_import_log",
                                      "oec_ab_export_log",
                                      "english_spoken"))) %>%
  filter(!(model=="gam" & var %in% c("wb_livestock_index", "wb_health_expend_perc", "oec_ab_import_log")))

# mean residuals
country_dat_rs2 %>%
  group_by(model) %>%
  summarize(mean_residuals = mean(abs(residual), na.rm=TRUE))

# covariance of predictions with n
country_dat2 %>%
  dplyr::select(n_amr_events, gam, bart) %>%
  as.matrix() %>%
  cor() 

#' -----------------Residuals-----------------
#+ r resids

# residual plots (black open circle = predicted, solid = actual)
ggplot(data = country_dat_rs2, aes(x = x, y = n_amr_events)) +
   geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
   geom_point(aes(color = residual)) +
   scale_color_gradient2(low = "blue", mid = "white", high = "red") +
   guides(color = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  labs(title = "", x = "") +
  facet_grid(model ~ var, scales = "free_x") +
  theme_bw()

#' -----------------ICE (Individual Conditional Expectation) Plots-----------------
#+ r ice

# ICE plots
mod_dat_gam <-  country_dat %>%
  select(pubs_sum,
         wb_ag_land_perc,
         wb_gdp_billion_log,
         wb_population_log ,
         oec_ab_export_log,
         english_spoken
         ) %>%
  as.data.frame()

mod_dat_bart <-  country_dat %>%
  select(-iso3c, -country, -continent, -id, -n_amr_events) %>%
  as.data.frame()

# function for bart and gam model predict
bart_predict <- function(model, newdat) {
  apply(predict(object = model, test = newdat), 2, mean)
}

gam_predict <- function(model, newdat) {
  predict.gam(object = model, newdata = newdat, type="response")
}

# explain objects
gamexp <- DALEX::explain(gam_mod, data = mod_dat_gam, y = country_dat$n_amr_events,
                         predict_function = gam_predict, label = "GAM")

bartexp <- DALEX::explain(bart_mod, data = mod_dat_bart, y = country_dat$n_amr_events,
                          predict_function = bart_predict, label = "BART")

# calculating ceteris paribus profiles, cpm is a ceteris_paribus_explainer and data frame
gamcpm <- ceteris_paribus(gamexp, observations = mod_dat_gam, y = country_dat$n_amr_events)
bartcpm <- ceteris_paribus(bartexp, observations = mod_dat_bart, y = country_dat$n_amr_events)

# ice plot (individual conditional expectation) - shows what the model would predict if all values for variable were observed x,y
plot(bartcpm) +
  labs(title = "BART", x = "", y = "n") +
  scale_color_manual(values  = "black") +
  stat_summary(aes(group = 1),
               geom = "line", fun.y = mean, size = 1.5, color = "green") +
  theme_bw()

plot(gamcpm) +
  labs(title = "GAM", x = "", y = "n") +
  scale_color_manual(values  = "black") +
  stat_summary(aes(group = 1),
               geom = "line", fun.y = mean, size = 1.5, color = "green") +
  theme_bw()
