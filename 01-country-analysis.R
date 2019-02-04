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
library(MASS)
library(mgcv)
library(caret)
source(here("multiplot.R"))

set.seed(101)

# prep data
country_dat <- read_csv(here("country_level.csv")) %>%
  na.omit() %>%
  mutate(NY.GDP.MKTP.CD.Billion = NY.GDP.MKTP.CD/1000000000) %>%
  dplyr::select(-NY.GDP.MKTP.CD, -date, -country) %>%
  mutate(continent = as.factor(continent))

# create reshape version
country_dat_rs <- country_dat %>% 
  gather(key = "var", value = "val", -n, -continent)

#' -----------------Visualize-----------------
#+ r plots

# look at data - eval distribution
# country_dat %>% arrange(-n)
hist(country_dat$n)
# lamda <- mean(country_dat$n)
# var <- var(country_dat$n)

# confirm data are overdispersed
# od_mod <- glm(data = country_dat, formula = n ~ NY.GDP.MKTP.CD.Billion:continent + SP.POP.TOTL:continent , family = "poisson")
# AER::dispersiontest(od_mod)

# plot population + gdp
p0 <- ggplot(data = country_dat_rs,
             mapping = aes(x = val, y = n)) +
  geom_point(alpha = 0.8, aes(color = continent)) +
  scale_x_log10() +
  facet_grid(~ var, scales = "free_x")

p0
# p0 +
#   geom_smooth(method="glm", method.args = list(family = "quasipoisson")) +
#   labs(title = "Quasipoisson")
# 
# p0 +
#   geom_smooth(method = "glm.nb") +
#   labs(title = "Neg. Binomial")  
# 
# p0 +
#   geom_smooth(method = "gam", formula = y ~ s(x)) +
#   labs(title = "GAM")  

# -----------------Fit Quasipoisson-----------------
#+ r mod-quasipoisson
# mod <- glm(data = country_dat,
#            formula = n ~ log10(NY.GDP.MKTP.CD.Billion):continent+ log10(SP.POP.TOTL):continent,
#            family="quasipoisson")
# tidy(mod)
# glance(mod)
# 
# country_dat$predicted <- predict(mod, type="response")
# country_dat$residuals <- residuals(mod, type="response")

# model_qp <- train(n ~ NY.GDP.MKTP.CD.Billion + SP.POP.TOTL + continent,
#                   #data = training,
#                   data = country_dat,
#                   method = "glm",
#                   family = "quasipoisson",
#                   trControl = trainControl(
#                     method = "cv", number = 10, verboseIter = TRUE
#                   )
# )
# # test data
# pred <- predict(model_qp, newdata = testing, type="raw") 
# actual <- testing$n
# mae <- sum(abs(pred - actual))
# 
# # full data
# country_dat$predicted <- predict(model_qp, newdata = country_dat, type="raw")   
# country_dat$residuals <- country_dat$n - country_dat$predicted

# country_dat %>% 
#   dplyr::select(-continent) %>%
#   gather(key = "var", value = "x", -n, -predicted, -residuals) %>%
#   ggplot(aes(x = x, y = n)) +
#   geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
#   geom_point(aes(color = residuals)) +
#   scale_color_gradient2(low = "blue", mid = "white", high = "red") +
#   scale_x_log10() +
#   guides(color = FALSE) +
#   geom_point(aes(y = predicted), shape = 1) +
#   facet_grid(~ var, scales = "free_x") +
#   theme_bw()

# -----------------Fit Negative Binomial-----------------
#+ r mod-neg-binom
# mod <- glm.nb(data = country_dat,
#               formula = n ~ NY.GDP.MKTP.CD.Billion + SP.POP.TOTL + continent,
#               control=glm.control(maxit=1000))
# tidy(mod)
# glance(mod)
# 
# country_dat$predicted <- predict(mod, type="response")   
# country_dat$residuals <- residuals(mod, type="response")
# 
# model_np <- train(n ~ NY.GDP.MKTP.CD.Billion + SP.POP.TOTL + continent,
#                   data = country_dat,
#                   method = "glm.nb",
#                   link = "log",
#                   #control=glm.control(maxit=1000),
#                   trControl = trainControl(
#                     method = "cv", number = 1, verboseIter = TRUE
#                   )
# )
# 
# country_dat$predicted <- predict(model_qp, type="raw")   
# country_dat$residuals <- residuals(model_qp, type="response")
# 
# country_dat %>% 
#   dplyr::select(-continent) %>%
#   gather(key = "var", value = "x", -n, -predicted, -residuals) %>%
#   #mutate(x = log(x)) %>%
#   ggplot(aes(x = x, y = n)) +
#   geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
#   geom_point(aes(color = residuals)) +
#   scale_color_gradient2(low = "blue", mid = "white", high = "red") +
#   scale_x_log10() +
#   guides(color = FALSE) +
#   geom_point(aes(y = predicted), shape = 1) +
#   facet_grid(~ var, scales = "free_x") +
#   theme_bw()
#' -----------------Fit GAM-----------------
#+ r mod-gam
#sp = smoothing parameter (higher = smoother, can be fit with method = "REML")
#k = number of base curves
# mod <- gam(data = country_dat, 
#            formula = n ~ + s(log(NY.GDP.MKTP.CD.Billion)) + continent + offset(SP.POP.TOTL), method = "REML")
mod <- gam(data = country_dat, 
           formula = n ~ + s(log(NY.GDP.MKTP.CD.Billion)) + continent + s(log(SP.POP.TOTL)), method = "REML")

summary(mod)
# plot(mod, page=1, 
#      all.terms = TRUE, 
#      residuals = TRUE, pch = 1, cex = 1, shade = TRUE, shade.col = "lightblue",
#      seWithMean = TRUE, shift = coef(mod)[1])
# higher EDF = more wiggly (1 = linear)
# assumes average of other vars
concurvity(mod, full = TRUE)

country_dat %>% 
  mutate(predicted = predict(mod, type="response"),
         residuals = residuals(mod, type="response")) %>%
  dplyr::select(-continent) %>%
  gather(key = "var", value = "x", -n, -predicted, -residuals) %>%
  ggplot(aes(x = x, y = n)) +
  geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
  geom_point(aes(color = residuals)) +
  scale_color_gradient2(low = "blue", mid = "white", high = "red") +
  scale_x_log10() +
  guides(color = FALSE) +
  geom_point(aes(y = predicted), shape = 1) +
  facet_grid(~ var, scales = "free_x") +
  theme_bw()

gam.check(mod)

#' -----------------Fit BART-----------------
#+ r mod-bart
library(dbarts)
bart_fit <- bart(country_dat %>% dplyr::select(-n),
               country_dat$n,
               ndpost=1000) 
plot(bartFit) # plot bart fit

fitmat = cbind(y,Ey,lmFit$fitted,bartFit$yhat.train.mean)
colnames(fitmat) = c('y','Ey','lm','bart')
print(cor(fitmat))
