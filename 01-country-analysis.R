#'---
#' title: "exploratory_analysis"
#' output: github_document
#' always_allow_html: yes
#' ---
#' 
#' 
#+ r setup, include = FALSE
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE,
                      fig.width = 10, dpi = 300)
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
  #select(-livestock_ab_sales_kg) %>% # temp remove until NA handling is implemented
  select(-continent, -region) %>%
  #na.omit() %>%
  mutate(gdp_dollars = log10(gdp_dollars/1000000000),
         population = log10(population)) %>%
  rename(gdp_billion_log = gdp_dollars,
         population_log = population) 
# create reshape version
country_dat_rs <- country_dat %>% 
  gather(key = "var", value = "val", -n_amr_events, - country, -iso3c) 
#' -----------------View Data-----------------
#+ r plots
ggplot(data = country_dat_rs %>%
         bind_rows(country_dat_rs %>%
                     select(val = n_amr_events, iso3c) %>%
                     mutate(var = "n_amr_events") %>% 
                     distinct()),
       mapping = aes(x = val)) +
  geom_histogram() +
  facet_wrap(var ~ ., scales = "free") +
  theme_bw()

chart.Correlation(country_dat %>%
                    select(- country, -iso3c),
                  histogram=FALSE, pch=19)

#' -----------------Fit GAM-----------------
#' using ziplss family- The ziplss family implements a zero inflated Poisson model in which one linear predictor controls the probability of presence and the other controls the mean given presence. 
#+ r mod-gam
# sp = smoothing parameter (higher = smoother, can be fit with method = "REML")
# k  = number of base curves

# handling NAs
## process data.frame producing binary indicators of missingness,
## mx0, mx1 etc. For each missing value create a level of a factor
## idx0, idx1, etc. So idx0 has as many levels as x0 has missing 
## values. Replace the NA's in each variable by the mean of the 
## non missing for that variable...

dname <- country_dat %>%
  select_if(~any(is.na(.))) %>%
  select(-english_spoken) %>%
  colnames()
dat1 <- country_dat
for (i in 1:length(dname)) {
  n <- nrow(country_dat)
  by.name <- paste("m",dname[i],sep="")
  dat1[[by.name]] <- is.na(dat1[[dname[i]]])
  dat1[[dname[i]]][dat1[[by.name]]] <- mean(dat1[[dname[i]]],na.rm=TRUE)
  lev <- rep(1,n);lev[dat1[[by.name]]] <- 1:sum(dat1[[by.name]])
  id.name <- paste("id",dname[i],sep="")
  dat1[[id.name]] <- factor(lev)
  dat1[[by.name]] <- as.numeric(dat1[[by.name]])
}

## Fit a gam, in which any missing value contributes zero 
## to the linear predictor from its smooth, but each 
## missing has its own random effect, with the random effect 
## variances being specific to the variable. e.g.
## for s(x0,by=ordered(!mx0)), declaring the `by' as an ordered
## factor ensures that the smooth is centred, but multiplied
## by zero when mx0 is one (indicating a missing x0). This means
## that any value (within range) can be put in place of the 
## NA for x0.  s(idx0,bs="re",by=mx0) produces a separate Gaussian 
## random effect for each missing value of x0 (in place of s(x0),
## effectively). The `by' variable simply sets the random effect to 
## zero when x0 is non-missing, so that we can set idx0 to any 
## existing level for these cases.   
k_tune <- 5
gam_mod <- gam(data = dat1, 
               formula = list(
                 # first specifies the response on the left hand side and the structure of the linear predictor for the Poisson parameter on the right hand side
                 n_amr_events ~ 
                   s(pubs_sum, by = ordered(!mpubs_sum), k = k_tune) + 
                   s(idpubs_sum, bs="re", by=mpubs_sum, k = k_tune) +
                   s(gdp_billion_log, by = ordered(!mgdp_billion_log), k = k_tune) + 
                   s(idgdp_billion_log, bs="re", by=mgdp_billion_log, k = k_tune) +
                   s(population_log, by = ordered(!mpopulation_log), k = k_tune) + 
                   s(idpopulation_log, bs="re", by=mpopulation_log, k = k_tune) +
                   english_spoken,
                 # specifying the linear predictor for the probability of presence on the right hand side
                 ~  
                   s(ab_consumption_ddd, by = ordered(!mab_consumption_ddd), k = k_tune) +
                   s(idab_consumption_ddd, bs="re", by=mab_consumption_ddd, k = k_tune) +
                   s(health_expend_perc, by = ordered(!mhealth_expend_perc), k = k_tune) + 
                   s(idhealth_expend_perc, bs="re", by=mhealth_expend_perc, k = k_tune) #+
                   # s(livestock_ab_sales_kg, by = ordered(!mlivestock_ab_sales_kg), k = k_tune) +
                   # s(idlivestock_ab_sales_kg, bs="re", by=mlivestock_ab_sales_kg, k = k_tune)
                   # s(ag_land_perc, by = ordered(!mag_land_perc), k = k_tune) +
                   # s(idag_land_perc, bs="re", by=mag_land_perc, k = k_tune)
                   # s(ab_export_perc, by = ordered(!mab_export_perc), k = k_tune) +
                   # s(idab_export_perc, bs="re", by=mab_export_perc, k = k_tune) +
                   # s(manure_soils_kg_per_km2) +
                   # s(migrant_pop_perc) +
               ),
               method = "REML", # to set sp
               family = ziplss())
# Notes for next steps
# brms - neg binomial - or hurdle - brm(formula, data), bf(count ~ ..., theta, formula) stick this into brm( with family = hurdle.poisson) - 4 chains on egypti
# bayes plot, sjplot::plotmodel() - missing data in brms - mice multiple imputation and then fit - haas option of fitting missing data.

gam_mod$outer.info$conv
summary(gam_mod) # higher EDF = more wiggly (1 = linear)
#concurvity(gam_mod, full = TRUE) # Concurvity occurs when some smooth term in a model could be approximated by one or more of the other smooth terms in the model. 
gam.check(gam_mod)

plot(gam_mod,
     all.terms = FALSE,
     residuals = TRUE, pch = 1, cex = 1, shade = TRUE, shade.col = "lightblue",
     seWithMean = TRUE, shift = coef(gam_mod)[1])

## more checking...
## 1. If the zero inflation rate becomes decoupled from the linear predictor, 
## it is possible for the linear predictor to be almost unbounded in regions
## containing many zeroes. So examine if the range of predicted values 
## is sane for the zero cases? 
range(predict(gam_mod,type="response")[gam_mod$y==0]) # range is reasonable

#' -----------------Fit BART-----------------
#+ r mod-bart
bart_mod <- bart(country_dat %>% dplyr::select(-n_amr_events, -iso3c, -country),
                 country_dat$n_amr_events,
                 ndpost=1000,
                 keeptrees = TRUE)

#' -----------------Compare models-----------------
#+ r mod-comp

# model results
# country_dat %<>% mutate(id = row_number())
# gt <- tibble(gam = predict(gam_mod, type="response"), id = country_dat %>% filter(!is.na(pubs_sum), !is.na(gdp_billion_log)) %>% pull(id))
# bt <- tibble(bart = bart_mod$yhat.train.mean, id = country_dat %>% na.omit() %>% pull(id))
# 
# # add in model predictions
# country_dat2 <- country_dat %>%
#   left_join(gt) %>%
#   left_join(bt) %>%
#   dplyr::select(-country, -iso3c, -id) 
# 
# # reshape
# country_dat_rs2 <- country_dat2  %>%
#   gather(key = "var", value = "x", -n_amr_events, -gam, -bart) %>%
#   gather(key = "model", value = "predicted", -x, -n_amr_events, -var) %>%
#   mutate(residual = n_amr_events - predicted,
#          var = factor(var, levels = c("pubs_sum",
#                                       "gdp_billion_log",
#                                       "population_log",
#                                       "migrant_pop_perc",
#                                       "health_expend_perc",
#                                       "ab_export_perc",
#                                       "ab_consumption_ddd",
#                                       "ag_land_perc",
#                                       #"manure_soils_kg_per_km2",
#                                       "english_spoken"),
#                       labels = c("Pubcrawler sum",
#                                  "GDP ($ billion; log scale)",
#                                  "Population (log scale)",
#                                  "Migrant Population (% total population)",
#                                  "Health Expenditure (% GDP)",
#                                  "Antibiotic Exports ($ billion; log scale)",
#                                  "Antibiotic Consumption (DDD per capita)",
#                                  "Agricultural Land Coverage (%)",
#                                  #"Manure applied to soils (N content; kg)",
#                                  "English Spoken"))) 
# 
# # mean residuals
# country_dat_rs2 %>%
#   group_by(model) %>%
#   summarize(mean_residuals = mean(abs(residual), na.rm=TRUE))
# 
# # covariance of predictions with n
# country_dat2 %>%
#   dplyr::select(n_amr_events, gam, bart) %>%
#   as.matrix() %>%
#   cor() 

#' -----------------Residuals-----------------
#+ r resids

# residual plots (black open circle = predicted, solid = actual)
# ggplot(data = country_dat_rs2, aes(x = x, y = n_amr_events)) +
#    geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
#    geom_point(aes(color = residual)) +
#    scale_color_gradient2(low = "blue", mid = "white", high = "red") +
#    guides(color = FALSE) +
#   geom_point(aes(y = predicted), shape = 1) +
#   labs(title = "", x = "") +
#   facet_grid(model ~ var, scales = "free_x") +
#   theme_bw()

#' -----------------ICE (Individual Conditional Expectation) Plots-----------------
#+ r ice

# ICE plots
mod_dat <-  country_dat %>%
  select(-iso3c, -country, #-id, 
         -n_amr_events) %>%
  as.data.frame()

# function for bart and gam model predict
bart_predict <- function(model, newdat) {
  apply(predict(object = model, test = newdat), 2, mean)
}

gam_predict <- function(model, newdat) {
  predict.gam(object = model, newdata = newdat, type="response")
}

# explain objects
gamexp <- DALEX::explain(gam_mod, data = mod_dat, y = country_dat$n_amr_events,
                         predict_function = gam_predict, label = "GAM")

bartexp <- DALEX::explain(bart_mod, data = mod_dat, y = country_dat$n_amr_events,
                          predict_function = bart_predict, label = "BART")

# calculating ceteris paribus profiles, cpm is a ceteris_paribus_explainer and data frame
gamcpm <- ceteris_paribus(gamexp, observations = mod_dat, y = country_dat$n_amr_events)
bartcpm <- ceteris_paribus(bartexp, observations = mod_dat, y = country_dat$n_amr_events)

# ice plot (individual conditional expectation) 
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
  theme_bw() +
  facet_wrap(~ `_vname_`, scales = "free", ncol = 3)
