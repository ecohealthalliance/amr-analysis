library(tidyverse)
library(brms)
library(magrittr)
library(here)
library(mice)
library(future)

set.seed(101)
source(here::here("R/functions.R"))

# Read in data
country_raw <- read_csv(here::here("country_level_amr.csv")) %>%
  dplyr::select(-continent, -region, -country, -english_spoken) %>%
  drop_na(population, gdp_dollars) %>% # remove if population or gdp data is unavailable (usually territories)
  mutate_at(vars(pubs_sum, ab_export_perc, ab_import_perc), ~replace_na(., 0)) #%>% # assume 0 for pubs_sum and ab_export NAs
#mutate_at(vars(gdp_dollars, population), ~log10(.))

# View correlation matrix
country_raw %>%
  dplyr::select(-iso3c) %>%
  PerformanceAnalytics::chart.Correlation(., histogram = TRUE, pch = 19)

# Which parameters have NAs
map_int(country_raw, ~sum(is.na(.)))

# Specify predictors for imputation
# Note that it might be best to include all variables: 
# https://stefvanbuuren.name/fimd/sec-modelform.html#sec:predictors
# Conditioning on all other data is often reasonable for small to medium datasets, containing up to, say, 20–30 variables, without derived variables, interactions effects and other complexities. As a general rule, using every bit of available information yields multiple imputations that have minimal bias and maximal efficiency (Meng 1994; Collins, Schafer, and Kam 2001). It is often beneficial to choose as large a number of predictors as possible. Including as many predictors as possible tends to make the MAR assumption more plausible, thus reducing the need to make special adjustments for MNAR mechanisms (Schafer 1997).
pred_matrix <- matrix(nrow = 4, ncol = ncol(country_raw), 
                      dimnames = list(c("health_expend_perc", "human_consumption_ddd", "livestock_consumption_kg_per_pcu", "livestock_pcu"),
                                      colnames(country_raw)), data = 0)
pred_matrix["health_expend_perc", c("gdp_dollars", "population")] <- 1
pred_matrix["human_consumption_ddd", c("gdp_dollars", "population", "ab_export_perc", "ab_import_perc")] <- 1
pred_matrix["livestock_consumption_kg_per_pcu", c("gdp_dollars", "population", "ab_export_perc", "ab_import_perc", "livestock_pcu", "human_consumption_ddd")] <- 1
pred_matrix["livestock_pcu", c("gdp_dollars", "population")] <- 1

# Impute + Diagnostics
# https://stefvanbuuren.name/fimd/sec-algoptions.html#sec:convergence
# https://stefvanbuuren.name/fimd/sec-diagnostics.html

m <- 4

## with specified predictors
country_mice <- mice(country_raw, m=m, maxit=35, method='cart', seed=500, blocks = c("health_expend_perc", "human_consumption_ddd", "livestock_consumption_kg_per_pcu", "livestock_pcu"), predictorMatrix = pred_matrix) 
plot(country_mice) # On convergence, the different streams should be freely intermingled with one another, without showing any definite trends. Convergence is diagnosed when the variance between different sequences is no larger than the variance within each individual sequence.
densityplot(country_mice)
stripplot(country_mice) # not working?
show_imputes(country_mice, m = m, raw = country_raw)

## using full predictors
country_mice_full <- mice(country_raw, m=m, maxit=35, method='cart', seed=500)
plot(country_mice_full) # On convergence, the different streams should be freely intermingled with one another, without showing any definite trends. Convergence is diagnosed when the variance between different sequences is no larger than the variance within each individual sequence.
densityplot(country_mice_full)
stripplot(country_mice_full) # not working?
show_imputes(country_mice_full, m = m, raw = country_raw)


# Model Runs
par(mfrow=c(2,2))

## simple run without impute
mod1 <- brm(bf(n_amr_events ~  livestock_consumption_kg_per_pcu + offset(log(population)),
              zi ~ pubs_sum ),
           data = country_raw,
           inits = "0",
           iter = 2000,
           family = zero_inflated_poisson(),
           cores = getOption("mc.cores", 4L))

write_rds(mod1, "mod1.rds")
mod1 <- read_rds("mod1.rds")
summary(mod1)
plot(marginal_effects(mod1), ask = FALSE)
# ^ works

## simple run with impute
plan(multiprocess)
mod2 <- brm_multiple(bf(n_amr_events ~ livestock_consumption_kg_per_pcu + offset(log(population)),
                       zi ~ pubs_sum ),
                    data = country_mice,
                    inits = "0",
                    iter = 2000,
                    family = zero_inflated_poisson(),
                    cores = getOption("mc.cores", 4L))

write_rds(mod2, "mod2.rds")
mod2 <- read_rds("mod2.rds")
summary(mod2)
plot(marginal_effects(mod2), ask = FALSE)

## full run with impute
plan(multiprocess)
mod3 <- brm_multiple(bf(n_amr_events ~  log(livestock_consumption_kg_per_pcu) + log(livestock_pcu) + 
                           migrant_pop_perc + ab_export_perc+ health_expend_perc + 
                          human_consumption_ddd + 
                          offset(log(population)) ,
                        zi ~ pubs_sum + gdp_dollars),
                     data = country_mice,
                     inits = "0",
                     iter = 4000,
                     family = zero_inflated_poisson(),
                     cores = getOption("mc.cores", 4L))
write_rds(mod3, "mod3.rds")
mod3 <- read_rds("mod3.rds")
summary(mod3)
plot(mod3)
plot(marginal_effects(mod3), ask = FALSE)
# ^ effects not interpretable

## full run with impute but no offset
plan(multiprocess)
mod4 <- brm_multiple(bf(n_amr_events ~  log(livestock_consumption_kg_per_pcu) + log(livestock_pcu) + 
                          migrant_pop_perc + ab_export_perc+ health_expend_perc + 
                          human_consumption_ddd + 
                          log(population),
                        zi ~ pubs_sum + log(gdp_dollars)),
                     data = country_mice,
                     inits = "0", 
                     iter = 4000,
                     control = list(adapt_delta = 0.9),
                     family = zero_inflated_poisson(),
                     cores = getOption("mc.cores", 4L))
write_rds(mod4, "mod4.rds")
mod4 <- read_rds("mod4.rds")
summary(mod4)
plot(mod4)
plot(marginal_effects(mod4), ask = FALSE)
# ^ looks pretty good!

## full run with impute but no offset - logging more vars - export and pubs sum have true 0s
plan(multiprocess)
mod5 <- brm_multiple(bf(n_amr_events ~  log(livestock_consumption_kg_per_pcu) + log(livestock_pcu) + 
                          log(migrant_pop_perc) + ab_export_perc+ log(health_expend_perc) + 
                          log(human_consumption_ddd) + 
                          log(population),
                        zi ~ pubs_sum + log(gdp_dollars)),
                     data = country_mice,
                     inits = "0", 
                     iter = 4000,
                     control = list(adapt_delta = 0.9),
                     family = zero_inflated_poisson(),
                     cores = getOption("mc.cores", 4L))
write_rds(mod5, "mod5.rds")
mod5 <- read_rds("mod5.rds")
summary(mod5)
plot(mod5)
plot(marginal_effects(mod5), ask = FALSE)
# ^ looks pretty good!

# is there a difference between mod4 and mod5?
fixef(mod4)
fixef(mod5)
marginal_effects(mod4, "migrant_pop_perc") 
marginal_effects(mod5, "migrant_pop_perc") 
marginal_effects(mod4, "health_expend_perc") 
marginal_effects(mod5, "health_expend_perc") 
marginal_effects(mod4, "human_consumption_ddd") 
marginal_effects(mod5, "human_consumption_ddd") 

# next: no offset, add population and gdp to both sides of equation

# then: treat population as an offset

