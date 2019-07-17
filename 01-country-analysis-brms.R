library(tidyverse)
library(brms)
library(magrittr)
library(here)
library(mice)
library(future)

set.seed(101)

# read in data
country_raw <- read_csv(here::here("country_level_amr.csv")) %>%
  dplyr::select(-continent, -region, -country) %>%
  drop_na(population, gdp_dollars) %>% # remove if population or gdp data is unavailable (usually territories)
  mutate_at(vars(pubs_sum, ab_export_perc, ab_import_perc), ~replace_na(., 0)) %>% # assume 0 for pubs_sum and ab_export NAs
  mutate(livestock_consumption_kg = ifelse(livestock_consumption_kg==0, 1, livestock_consumption_kg)) %>%
  mutate_at(vars(gdp_dollars, population, livestock_consumption_kg), ~log10(.))

country_scale <- country_raw %>%
  mutate_at(vars(-iso3c, -n_amr_events, -pubs_sum, -english_spoken), ~BBmisc::normalize(., method = "standardize"))
# https://www.rdocumentation.org/packages/BBmisc/versions/1.10/topics/normalize
# https://www.theanalysisfactor.com/when-not-to-center-a-predictor-variable-in-regression/

# specify predictors for imputation
pred_matrix <- matrix(nrow = 2, ncol = ncol(country_raw), 
                      dimnames = list(c("health_expend_perc", "human_consumption_ddd"),
                                      colnames(country_raw)), data = 0)
pred_matrix["health_expend_perc", c("gdp_dollars", "population")] <- 1
pred_matrix["human_consumption_ddd", c("gdp_dollars", "population", "ab_export_perc", "ab_import_perc")] <- 1

# impute
country_raw_mice <- mice(country_raw, m=4, maxit=35, method='cart', seed=500, blocks = c("health_expend_perc", "human_consumption_ddd"), predictorMatrix = pred_matrix) 
country_scale_mice <- mice(country_scale, m=4, maxit=35, method='cart', seed=500,   blocks = c("health_expend_perc", "human_consumption_ddd"), predictorMatrix = pred_matrix) 

# run models

## simple run without impute
### raw
mod <- brm(bf(n_amr_events ~  livestock_consumption_kg,
              zi ~ pubs_sum ),
           data = country_raw,
           inits = "0",
           iter = 2000,
           family = zero_inflated_poisson(),
           cores = getOption("mc.cores", 4L))

write_rds(mod, "mod1.rds")
mod <- read_rds("mod1.rds")
summary(mod)
# ^ works

### scale
mod <- brm(bf(n_amr_events ~  livestock_consumption_kg,
              zi ~ pubs_sum ),
           data = country_scale,
           inits = "0",
           iter = 2000,
           family = zero_inflated_poisson(),
           cores = getOption("mc.cores", 4L))

write_rds(mod, "mod2.rds")
mod <- read_rds("mod2.rds")
summary(mod)
# ^ works


### raw
mod <- brm(bf(n_amr_events ~  livestock_consumption_kg + migrant_pop_perc +
                pubs_sum + gdp_dollars + population,
              zi ~ pubs_sum + gdp_dollars + population),
           data = country_raw,
           inits = "0",
           iter = 4000,
           family = zero_inflated_poisson(),
           cores = getOption("mc.cores", 4L))

write_rds(mod, "mod3.rds")
# ^ did not converge, but rhats were fairly low

### scale
mod <- brm(bf(n_amr_events ~  livestock_consumption_kg + migrant_pop_perc +
              pubs_sum + gdp_dollars + population,
              zi ~ pubs_sum + gdp_dollars + population ),
           data = country_scale,
           inits = "0",
           iter = 4000,
           family = zero_inflated_poisson(),
           cores = getOption("mc.cores", 4L))

write_rds(mod, "mod4.rds")

## now run with impute
### raw
plan(multiprocess)
mod <- brm_multiple(bf(n_amr_events ~  livestock_consumption_kg + migrant_pop_perc + health_expend_perc + human_consumption_ddd +
              pubs_sum + gdp_dollars + population,
              zi ~ pubs_sum + gdp_dollars + population),
           data = country_raw_mice,
           inits = "0",
           iter = 2000,
           family = zero_inflated_poisson(),
           cores = getOption("mc.cores", 4L))
write_rds(mod, "mod5.rds")

### scale
plan(multiprocess)
mod <- brm_multiple(bf(n_amr_events ~  livestock_consumption_kg + migrant_pop_perc + health_expend_perc + human_consumption_ddd +
                       pubs_sum + gdp_dollars + population ,
                       zi ~ pubs_sum + gdp_dollars + population ),
                    data = country_scale_mice,
                    inits = "0",
                    iter = 2000,
                    family = zero_inflated_poisson(),
                    cores = getOption("mc.cores", 4L))
write_rds(mod, "mod6.rds")
summary(mod)
