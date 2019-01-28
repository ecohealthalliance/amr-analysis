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
source(here("multiplot.R"))

country_dat <- read_csv(here("country_level.csv")) %>%
  na.omit() %>%
  mutate(NY.GDP.MKTP.CD.Billion = NY.GDP.MKTP.CD/1000000000) %>%
  dplyr::select(-NY.GDP.MKTP.CD)

#' -----------------Visualize-----------------
#+ r plots
country_dat %>% arrange(-n)
hist(country_dat$n)

model_colors <- RColorBrewer::brewer.pal(3, "Set1")

# plot population
p0 <- ggplot(data = country_dat,
             mapping = aes(x = SP.POP.TOTL, y = n)) +
  geom_point(alpha = 0.8, aes(color = continent)) +
  scale_x_log10() 

p_glm <- p0 +
  geom_smooth(method="glm", method.args = list(family = "quasipoisson")) +
  labs(title = "Quasipoisson")
  
p_nb <- p0 +
  geom_smooth(method = "glm.nb") +
  labs(title = "Neg. Binomial")  

p_gam <- p0 +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 3)) +
  labs(title = "GAM")  

p_lo <- p0 +
  geom_smooth(method = "loess") +
  labs(title = "LOESS")  

multiplot(p_glm, p_nb, p_gam, p_lo, cols=2) 

# plot gdp
p0 <- ggplot(data = country_dat,
             mapping = aes(x = NY.GDP.MKTP.CD.Billion, y = n)) +
  geom_point(alpha = 0.8, aes(color = continent)) +
  scale_x_log10()

p_glm <- p0 +
  geom_smooth(method="glm", method.args = list(family = "quasipoisson")) +
  labs(title = "Quasipoisson")

p_nb <- p0 +
  geom_smooth(method = "glm.nb") +
  labs(title = "Neg. Binomial")

p_gam <- p0 +
  geom_smooth(method = "gam", formula = y ~ s(x, k = 3)) +
  labs(title = "GAM")

p_lo <- p0 +
  geom_smooth(method = "loess") +
  labs(title = "LOESS")

multiplot(p_glm, p_nb, p_gam, p_lo, cols=2)

#' -----------------Fit quasipoisson-----------------
#+ r mod
mod <- glm(data = country_dat, formula = n ~ NY.GDP.MKTP.CD.Billion * SP.POP.TOTL , family = "quasipoisson")
tidy(mod)
glance(mod)
