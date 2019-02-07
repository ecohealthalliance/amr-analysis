#'---
#' title: "exploratory_analysis"
#' output: github_document
#' always_allow_html: yes
#' ---
#' 
#' 
#+ r setup, include = FALSE
knitr::opts_chunk$set(echo = FALSE, message = FALSE, warning = FALSE)
library(tidyverse)
library(here)
library(broom)
library(mgcv)
library(dbarts)
library(DALEX)
library(ceterisParibus)
library(PerformanceAnalytics)

set.seed(101)

# prep data
country_dat <- read_csv(here::here("country_level.csv")) %>%
  na.omit() %>%
  mutate(NY.GDP.MKTP.CD.Billion = NY.GDP.MKTP.CD/1000000000) %>%
  mutate(NY.GDP.MKTP.CD.Billion.log = log(NY.GDP.MKTP.CD.Billion)) %>%
  mutate(SP.POP.TOTL.log = log(SP.POP.TOTL)) %>%
  mutate(continent = as.factor(continent)) %>%
  dplyr::select(-NY.GDP.MKTP.CD, -date, -country, -NY.GDP.MKTP.CD.Billion, -SP.POP.TOTL)

# create reshape version
country_dat_rs <- country_dat %>% 
  gather(key = "var", value = "val", -n, -continent) %>%
  mutate(var = as_factor(var, levels = c(NY.GDP.MKTP.CD.Billion.log, SP.POP.TOTL.log, pubs_sum)))

#' -----------------View Data-----------------
#+ r plots

# plot population + gdp
ggplot(data = country_dat_rs,
       mapping = aes(x = val, y = n)) +
  geom_point(alpha = 0.8, aes(color = continent)) +
  facet_wrap(var ~ ., scales = "free_x") +
  theme_bw() +
  theme(legend.position = "top", legend.title = element_blank())

chart.Correlation(country_dat %>% 
                    select(-continent, -n), 
                  histogram=TRUE, pch=19)
#' -----------------Fit GAM-----------------
#+ r mod-gam
# sp = smoothing parameter (higher = smoother, can be fit with method = "REML")
# k  = number of base curves

gam_mod <- gam(data = country_dat, 
               formula = n ~  s(NY.GDP.MKTP.CD.Billion.log) + continent + s(SP.POP.TOTL.log) + s(pubs_sum),
               #weight = pubs_sum, 
               method = "REML",
               family = "quasipoisson")

# gam_mod <- gam(data = country2_dat,
#                formula = n ~ s(log(NY.GDP.MKTP.CD.Billion)) +  s(log(SP.POP.TOTL)) + ti(log(NY.GDP.MKTP.CD.Billion), log(SP.POP.TOTL)) + continent,
#                method = "REML",
#                family = "quasipoisson")

summary(gam_mod) # higher EDF = more wiggly (1 = linear)
concurvity(gam_mod, full = TRUE)
gam.check(gam_mod)

# plot(gam_mod, page=1,
#      all.terms = TRUE,
#      residuals = TRUE, pch = 1, cex = 1, shade = TRUE, shade.col = "lightblue",
#      seWithMean = TRUE, shift = coef(gam_mod)[1])

#' -----------------Fit BART-----------------
#+ r mod-bart

bart_mod <- bart(country_dat %>% dplyr::select(-n),
                 country_dat$n,
                 ndpost=1000,
                 keeptrees = TRUE)

summary(bart_mod)

#' -----------------Compare models-----------------
#+ r mod-comp

# add in model predictions
country_dat2 <- country_dat %>%
  mutate(gam = predict(gam_mod, type="response"),
         bart = bart_mod$yhat.train.mean) %>%
  dplyr::select(-continent) 

# reshape
country_dat_rs2 <- country_dat2  %>%
  gather(key = "var", value = "x", -n, -gam, -bart) %>%
  gather(key = "model", value = "predicted", -x, -n, -var) %>%
  mutate(residual = n - predicted,
         var = as_factor(var, levels = c(NY.GDP.MKTP.CD.Billion.log, SP.POP.TOTL.log, pubs_sum)))


# mean residuals
country_dat_rs2 %>%
  group_by(model) %>%
  summarize(mean_residuals = mean(abs(residual)))

# covariance of predictions with n
country_dat2 %>%
  dplyr::select(n, gam, bart) %>%
  as.matrix() %>%
  cor()

# residual plots
ggplot(data = country_dat_rs2, aes(x = x, y = n)) +
  geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
  geom_point(aes(color = residual)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  guides(color = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  labs(title = "", x = "") + 
  facet_grid(model ~ var, scales = "free_x") +
  theme_bw()

# ICE plots
mod_dat <- as.data.frame(country_dat)

# function for bart and gam model predict
bart_predict <- function(model, newdat) {
  apply(predict(object = model, test = newdat), 2, mean)
}

gam_predict <- function(model, newdat) {
  predict.gam(object = model, newdata = newdat, type="response")
}

# explain objects
gamexp <- DALEX::explain(gam_mod, data = mod_dat %>% dplyr::select(-n), y = mod_dat$n,
                         predict_function = gam_predict, label = "GAM")
bartexp <- DALEX::explain(bart_mod, data = mod_dat %>% dplyr::select(-n), y = mod_dat$n,
                          predict_function = bart_predict, label = "BART")

# calculating ceteris paribus profiles, cpm is a ceteris_paribus_explainer and data frame
gamcpm <- ceteris_paribus(gamexp, observations = mod_dat %>% dplyr::select(-n), y = mod_dat$n)
bartcpm <- ceteris_paribus(bartexp, observations = mod_dat %>% dplyr::select(-n), y = mod_dat$n)

plot(bartcpm) +
  labs(title = "BART", x = "", y = "n") +
  scale_color_manual(values  = "black") +
  stat_summary(aes(group = 1),
               geom = "line", fun.y = mean, size = 1.5, color = "coral") + 
  theme_bw()

plot(gamcpm) +
  labs(title = "GAM", x = "", y = "n") +
  scale_color_manual(values  = "black") +
  stat_summary(aes(group = 1),
               geom = "line", fun.y = mean, size = 1.5, color = "coral") + 
  theme_bw()
