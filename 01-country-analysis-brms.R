library(tidyverse)
library(brms)
library(magrittr)
library(here)
library(broom)
library(mgcv)
library(dbarts)
library(DALEX)
library(ceterisParibus)
library(PerformanceAnalytics)
library(mice)
library(future)

set.seed(101)

country_raw <- read_csv(here::here("country_level_amr.csv")) %>%
  select(-continent, -region, -english_spoken, -country) %>%
  drop_na(population, gdp_dollars) %>% # remove if population or gdp data is unavailable (usually territories)
  mutate_at(vars(pubs_sum, ab_export_perc), ~replace_na(., 0)) # assume 0 for pubs_sum and ab_export NAs

country_mean_imp <- country_raw %>%
  mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .)) 

# country_center <- country_raw %>%
#   mutate_at(vars(-iso3c, -n_amr_events, -pubs_sum), ~as.numeric(scale(., scale=F)) )

# # prep data
# country_dat <- read_csv(here::here("country_level_amr.csv")) %>%
#   select(-continent, -region) %>%
#   #tmp NA handling with means
#   mutate_if(is.numeric, ~ifelse(is.na(.), rnorm(sum(is.na(.)), mean(., na.rm = T), 1), .))
#
#
# mod <- brm(bf(n_amr_events ~  ab_consumption_ddd,
#               zi ~ pubs_sum ),
#            data = country_dat,
#            family = zero_inflated_poisson(),
#            cores = getOption("mc.cores", 4L))
# # ^ works
#
# mod <- brm(bf(n_amr_events ~  ab_consumption_ddd + pubs_sum,
#               zi ~ pubs_sum ),
#            data = country_dat,
#            family = zero_inflated_poisson(),
#            cores = getOption("mc.cores", 4L))
# # ^
# # Warning messages:
# # 1: There were 1807 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
# # http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
# # 2: There were 3 chains where the estimated Bayesian Fraction of Missing Information was low. See
# # http://mc-stan.org/misc/warnings.html#bfmi-low
# # 3: Examine the pairs() plot to diagnose sampling problems
#
# mod <- brm(bf(n_amr_events ~  ab_consumption_ddd + pubs_sum,
#               zi ~ pubs_sum ),
#            data = country_dat,
#            family = zero_inflated_poisson(),
#            cores = getOption("mc.cores", 4L),
#            inits = "0",
#            control = list(max_treedepth = 15))
# # ^ works
#
# mod <- brm(bf(n_amr_events ~ pubs_sum + gdp_dollars + population + health_expend_perc +
#                 migrant_pop_perc + ab_consumption_ddd + manure_soils_kg_per_km2 + ag_land_perc,
#               zi ~ pubs_sum + gdp_dollars + population + health_expend_perc),
#            data = country_dat,
#            family = zero_inflated_poisson(),
#            cores = getOption("mc.cores", 4L),
#            inits = "0",
#            iter = 2000,
#            control = list(max_treedepth = 15))
# # ^
# # Warning messages:
# # 1: There were 3998 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 15. See
# # http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
# # 2: Examine the pairs() plot to diagnose sampling problems
#
# mod <- brm(bf(n_amr_events ~ pubs_sum + gdp_dollars + population + health_expend_perc +
#                 migrant_pop_perc + ab_consumption_ddd + manure_soils_kg_per_km2 + ag_land_perc,
#               zi ~ pubs_sum),
#            data = country_dat,
#            family = zero_inflated_poisson(),
#            cores = getOption("mc.cores", 4L),
#            inits = "0",
#            iter = 500,
#            control = list(max_treedepth = 15))
# # ^
# # Warning messages:
# # 1: There were 1000 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 15. See
# # http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
# # 2: Examine the pairs() plot to diagnose sampling problems
#
# mod <- brm(bf(n_amr_events ~ pubs_sum + gdp_dollars + population +
#                 ab_consumption_ddd + ag_land_perc,
#               zi ~ pubs_sum),
#            data = country_dat,
#            family = zero_inflated_poisson(),
#            cores = getOption("mc.cores", 4L),
#            inits = "0",
#            iter = 500,
#            control = list(max_treedepth = 15))
# # ^
# # Warning messages:
# # 1: There were 994 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 15. See
# # http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
# # 2: Examine the pairs() plot to diagnose sampling problems
#
# # can decrease step size, adapt_delta = 0.99
# traceplot(mod$fit)
# summary(mod)
# plot(marginal_effects(mod), ask = FALSE)
#
# # Pause to work on missing data handling
# # https://cran.r-project.org/web/packages/brms/vignettes/brms_missings.html
# # Imputation before model fitting
# # Each missing value is not imputed once but times leading to a total of fully imputed data sets. The model can then be fitted to each of those data sets separetely and results are pooled across models, afterwards.
# library(mice)
#
# country_raw <- read_csv(here::here("country_level_amr.csv")) %>%
#   select(-continent, -region, -english_spoken, -country) %>%
#   filter_at(.vars =  vars(pubs_sum:manure_soils_kg_per_km2), .vars_predicate = any_vars(!is.na(.))) %>% # remove all NA rows

# comparing imputed to true distribution
# country_mice <- mice(country_raw, m=1, maxit=500, method='cart', seed=500) #https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/24586
# imputed <- imap(country_mice$imp,
#                 ~rename_all(.x, ~paste0(!!.y, ".val")) %>%
#                   rownames_to_column() %>%
#                   mutate(!!paste0(.y, ".qual") := 1))
#
# raw <- imap(country_raw,
#             ~select(country_raw, .y) %>%
#               rownames_to_column() %>%
#               drop_na() %>%
#               mutate(!!paste0(colnames(.)[2], ".qual") := 0) %>%
#               rename(!!paste0(colnames(.)[2], ".val") := 2)
# )
#
# country_tmp <- map2(imputed, raw, ~bind_rows(.x, .y)) %>%
#   reduce(left_join) %>%
#   select(-starts_with("iso3c"), -starts_with("country"))
#
# country_vars <- country_tmp %>%
#   select(rowname, ends_with(".val")) %>%
#   gather("key", "value", -rowname) %>%
#   mutate(key = str_remove(key, ".val"))
#
# country_quals <- country_tmp %>%
#   select(rowname, ends_with(".qual")) %>%
#   gather("key", "qual", -rowname) %>%
#   mutate(key = str_remove(key, ".qual"))
#
# country_all <- full_join(country_vars, country_quals)
#
# country_all %>%
#   ggplot(., aes(x = value)) +
#   geom_histogram() +
#   geom_histogram(data = country_all %>% filter(qual==0), aes(x = value), color = "red") +
#   #scale_x_log10() +
#   facet_wrap(key~., scales = "free") +
#   labs(caption = "red is non-imputed values")
#
# chart.Correlation(country_raw %>%
#                     select(-iso3c),
#                   histogram=FALSE, pch=19)
#
# country_raw %>%
#   drop_na(livestock_ab_sales_kg) %>%
#   ggplot(aes(x = livestock_ab_sales_kg, y = n_amr_events)) +
#   geom_point() +
#   geom_smooth(method='lm',formula=y~x) +
#   scale_x_log10()
#
#
# chart.Correlation(country_tmp %>%
#                     select(-rowname, -ends_with(".qual")),
#                   histogram=FALSE, pch=19)
#
# country_tmp %>%
#   mutate(livestock_ab_sales_kg.qual = factor(livestock_ab_sales_kg.qual, levels = c(0, 1), labels = c("raw", "imputed"))) %>%
#   ggplot(aes(x = livestock_ab_sales_kg.val, y = n_amr_events.val, color = livestock_ab_sales_kg.qual)) +
#   geom_point() +
#   scale_x_log10()
#
# # now run with multiple imputations
# # helpful for setting parameters: https://stats.stackexchange.com/questions/219013/how-do-the-number-of-imputations-the-maximum-iterations-affect-accuracy-in-mul/219049
# # m is the number of imputations, generally speaking, the more the better. Originally (following Rubin, 1987) 5 was considered to be enough (hence the default). So from an accuracy point of view, 5 may be sufficient. However, this was based on an efficiency argument only. In order to achieve better estimates of standard errors, more imputations are needed. These days there is a rule of thumb to use whatever the average percentage rate of missingness is - so if there is 30% missing data on average in a dataset, use 30 imputations - see Bodner (2008) and White et al (2011) for further details.
# map_dbl(country_raw, ~sum(is.na(.))/nrow(country_raw)) %>% mean()
# # maxit is the number of iterations for each imputation. mice uses an iterative algorithm. It is important that the imputations for all variables reach convergence, otherwise they will be inaccurate. By inspecting the trace plots generated by plot() this can be visually determined. Unlike other Gibbs sampling methods, far fewer iterations are needed - generally in the region of 20-30 or less as a rule of thumb. When the trace lines reach a value and fluctuate slightly around it, convergence has been achieved.
#
# country_mice <- mice(country_raw, m=20, maxit=35, method='cart', seed=500) #https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/24586
# write_rds(country_mice, "country_mice.rds")
# plot(country_mice)
#
# # fit simple model
# mod <- brm_multiple(bf(n_amr_events ~ ab_consumption_ddd + ag_land_perc,
#                        zi ~ pubs_sum),
#                     data = country_mice,
#                     family = zero_inflated_poisson(),
#                     cores = getOption("mc.cores", 4L),
#                     inits = "0",
#                     iter = 500,
#                     control = list(max_treedepth = 15))
# # ^
# # startup message: "the number of chains is less than 1; sampling not done"
# # Warning messages:
# # 1: Imputed model 3 did not converge.
# # 2: Imputed model 14 did not converge. '
# # Warning message:
# # The model has not converged (some Rhats are > 1.1). Do not analyse the results!
# # We recommend running more iterations and/or setting stronger priors.
#
# rstan::traceplot(mod$fit)
# summary(mod)
# plot(marginal_effects(mod), ask = FALSE)

# fit full model - need to reduce the number of m to improve speed
# country_mice <- mice(country_raw, m=4, maxit=35, method='cart', seed=500) #https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/24586
# write_rds(country_mice, "country_mice.rds")
#
# mod <- brm_multiple(bf(n_amr_events ~ pubs_sum + gdp_dollars + population + health_expend_perc +
#                          migrant_pop_perc + ab_consumption_ddd + manure_soils_kg_per_km2 + ag_land_perc,
#                        zi ~ pubs_sum + gdp_dollars + population + health_expend_perc),
#                     data = country_mice,
#                     family = zero_inflated_poisson(),
#                     cores = getOption("mc.cores", 4L),
#                     inits = "0",
#                     iter = 500,
#                     control = list(max_treedepth = 15))
# write_rds(mod, "lastest_mod.rds")

# ^
# Warning messages:
#   1: In grid.Call.graphics(C_upviewport, as.integer(n)) :a
#   cannot pop the top-level viewport ('grid' and 'graphics' output mixed?)
# 2: There were 999 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 15. See
# http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
# 3: Examine the pairs() plot to diagnose sampling problems
#
# 4: Imputed model 1 did not converge.
# 5: There were 999 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 15. See
# http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
# 6: Examine the pairs() plot to diagnose sampling problems
#
# 7: Imputed model 2 did not converge.
# 8: There were 1000 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 15. See
# http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
# 9: Examine the pairs() plot to diagnose sampling problems
#
# 10: Imputed model 3 did not converge.
# 11: There were 1000 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 15. See
# http://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded
# 12: Examine the pairs() plot to diagnose sampling problems
#
# 13: Imputed model 4 did not converge.

# country_mice <- mice(country_raw, m=15, maxit=35, method='cart', seed=500) #https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/24586
# write_rds(country_mice, "country_mice.rds")
#
# mod <- brm_multiple(bf(n_amr_events ~ pubs_sum + gdp_dollars + population, #+ health_expend_perc +
#                          #migrant_pop_perc + ab_consumption_ddd + manure_soils_kg_per_km2 + ag_land_perc,
#                        zi ~ pubs_sum + gdp_dollars + population #+ health_expend_perc
#                        ),
#                     data = country_mice,
#                     family = zero_inflated_poisson(),
#                     cores = getOption("mc.cores", 4L),
#                     inits = "0",
#                     iter = 2000,
#                     control = list(max_treedepth = 15))
# write_rds(mod, "lastest_mod_2000iter_m15.rds")
# summary(mod)
# ^ no convergence

# simple model with un-centered data
#country_mice <- mice(country_raw, m=4, maxit=35, method='cart', seed=500) #https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/24586
# 'cart'(classification and regression trees)
# https://arxiv.org/pdf/1603.01631.pdf - CART is nonparametric, tends to reduce error, not hindered by multicollinearity

# mod <- brm_multiple(bf(n_amr_events ~ pubs_sum + population + ab_consumption_ddd + livestock_ab_sales_kg,
#                        zi ~  pubs_sum + population),
#                     data = country_mice,
#                     family = zero_inflated_poisson(),
#                     cores = getOption("mc.cores", 4L),
#                     inits = "0",
#                     iter = 500,
#                     control = list(max_treedepth = 15))
# ^ did not converge
# plan(multiprocess)
# country_mice <- mice(country_center, m=4, maxit=35, method='cart', seed=500) #https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/24586
# mod <- brm_multiple(bf(n_amr_events ~ pubs_sum + population + ab_consumption_ddd + livestock_ab_sales_kg,
#                        zi ~  pubs_sum + population),
#                     data = country_mice,
#                     family = zero_inflated_poisson(),
#                     cores = getOption("mc.cores", 4L),
#                     inits = "0",
#                     iter = 2000,
#                     control = list(max_treedepth = 15))
# write_rds(mod, "lastest_mod.rds")
# mod <- read_rds("lastest_mod.rds")
# summary(mod)
# ^ did not converge

# country_mice <- mice(country_raw, m=4, maxit=35, method='cart', seed=500) #https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/24586
# plan(multiprocess)
# mod <- brm_multiple(bf(n_amr_events ~ pubs_sum + population,
#                        zi ~  pubs_sum ),
#                     data = country_mice,
#                     family = zero_inflated_poisson(),
#                     cores = getOption("mc.cores", 4L),
#                     inits = "0",
#                     iter = 2000,
#                     control = list(max_treedepth = 15))
# write_rds(mod, "latest_mod.rds")
# 
# country_mice <- mice(country_raw, m=4, maxit=35, method='cart', seed=500) #https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/24586
# plan(multiprocess)
# mod <- brm_multiple(bf(n_amr_events ~ pubs_sum + gdp_dollars + population + health_expend_perc +
#                          migrant_pop_perc + ab_consumption_ddd + manure_soils_kg_per_km2 + ag_land_perc,
#                        zi ~ pubs_sum + gdp_dollars + population + health_expend_perc
#                        ),
#                     data = country_mice,
#                     family = zero_inflated_poisson(),
#                     cores = getOption("mc.cores", 4L),
#                     inits = "0",
#                     iter = 2000,
#                     control = list(max_treedepth = 15))
# write_rds(mod, "latest_full_mod.rds")
# 
# mod_simple <- read_rds("latest_mod.rds")
# mod_full <- read_rds("latest_full_mod.rds")
#  
# country_raw %>%
#  gather(key = "key", value="value", pubs_sum:manure_soils_kg_per_km2) %>%
#   ggplot(aes(x = value)) +
#   geom_histogram() +
#   facet_wrap(key~., scale = "free")

# 7/12/19
# back to simple
# mod <- brm(bf(n_amr_events ~  gdp_dollars,
#               zi ~ pubs_sum ),
#            data = country_raw,
#            inits = "0",
#            iter = 2000,
#            control = list(max_treedepth = 15),
#            family = zero_inflated_poisson(),
#            cores = getOption("mc.cores", 4L))
# 
# write_rds(mod, "latest_mod.rds")
# mod <- read_rds( "latest_mod.rds")
# ^ did not converge

# evaluate variation in imputations
# country_mice <- mice(country_raw, m=4, maxit=35, method='cart', seed=500) 
# 
# imp <- country_mice$imp
# 
# imp2 <- imap_dfr(imp, function(x, y){
#   x %>%
#     as_tibble() %>%
#     rownames_to_column(var = "country_id") %>%
#     gather(key = "m", value = "value", `1`:`4`) %>%
#     mutate(field = y)
# })
# 
# raw_means <- country_raw %>%
#   select_if(~any(is.na(.))) %>%
#   gather(key = "field", value = "value") %>%
#   group_by(field) %>%
#   summarize(mean = mean(value, na.rm = T), min = min(value, na.rm = T), max = max(value, na.rm = T)) 
# 
# ggplot(data = imp2, aes(x = country_id,  y = value, color = m)) +
#   geom_point() +
#   geom_hline(data = raw_means, aes(yintercept = mean)) +
#  # geom_hline(data = raw_means, aes(yintercept = min), lty= 2) +
#  # geom_hline(data = raw_means, aes(yintercept = max), lty = 2) +
#   facet_wrap(field ~., scales = "free")

# try with mean imputations 
# country_mean_imp <- country_raw %>%
#   mutate_all(~ifelse(is.na(.), mean(., na.rm = TRUE), .)) 

# simple models with mean imputations
# mod <- brm(bf(n_amr_events ~  ab_consumption_ddd,
#               zi ~ pubs_sum),
#            data = country_mean_imp,
#            family = zero_inflated_poisson(),
#            cores = getOption("mc.cores", 4L),
#            inits = "0",
#            iter = 2000,
#            control = list(max_treedepth = 15))
# write_rds(mod, "latest_mod_simple.rds")
# summary(mod)
# rstan::traceplot(mod$fit)
# # ^ works
# 
# mod <- brm(bf(n_amr_events ~  pubs_sum + ab_consumption_ddd + livestock_ab_sales_kg,
#               zi ~ pubs_sum),
#            data = country_mean_imp,
#            family = zero_inflated_poisson(),
#            cores = getOption("mc.cores", 4L),
#            inits = "0",
#            iter = 2000,
#            control = list(max_treedepth = 15))
# write_rds(mod, "latest_mod_simple.rds")
# summary(mod)
# rstan::traceplot(mod$fit)
# ^ not converged

# ^ not converged :(

# full model with mean imputations
# mod <- brm(bf(n_amr_events ~ pubs_sum + gdp_dollars + population + health_expend_perc +
#                              migrant_pop_perc + livestock_ab_sales_kg + ab_consumption_ddd +
#                              manure_soils_kg_per_km2 + ag_land_perc + ab_export_perc,
#               zi ~ pubs_sum + gdp_dollars + population + health_expend_perc),
#            data = country_mean_imp,
#            family = zero_inflated_poisson(),
#            cores = getOption("mc.cores", 4L),
#            inits = "0",
#            iter = 2000,
#            control = list(max_treedepth = 15))
# 
# write_rds(mod, "latest_mod_full.rds")
# ^ not converged

# Now try imputing in the model
# not ZIP
# bform <- bf(n_amr_events ~ population + mi(ab_consumption_ddd))  + 
#   bf(ab_consumption_ddd | mi() ~ population) +  set_rescor(FALSE)
# mod <- brm(bform, data = country_raw)
# # ^ not converged
# 
# # ZIP
bform <- bf(n_amr_events ~ population + mi(ab_consumption_ddd), zi ~ pubs_sum)  +
  bf(ab_consumption_ddd | mi() ~ population) +  set_rescor(FALSE)
mod <- brm(bform,
           data = country_raw,
           family = zero_inflated_poisson(),
           cores = getOption("mc.cores", 4L),
           inits = "0",
           iter = 2000,
           control = list(max_treedepth = 15))
# ^ not compatible 

country_mice <- mice(country_raw, m=4, maxit=35, method='cart', seed=500) #https://www.kaggle.com/c/house-prices-advanced-regression-techniques/discussion/24586

plot(country_raw$ab_consumption_ddd, country_raw$n_amr_events)

plan(multiprocess)
mod <- brm_multiple(bf(n_amr_events ~ ab_consumption_ddd  + population,
                       zi ~  population ),
                    data = country_mice,
                    family = zero_inflated_poisson(),
                    cores = getOption("mc.cores", 4L),
                    inits = "0",
                    iter = 2000,
                    control = list(max_treedepth = 15))
write_rds(mod, "latest_mod.rds")
mod <- read_rds("latest_mod.rds")

imp <- country_mice$imp

imp2 <- imap_dfr(imp, function(x, y){
  x %>%
    as_tibble() %>%
    rownames_to_column(var = "country_id") %>%
    gather(key = "m", value = "value", `1`:`4`) %>%
    mutate(field = y)
})

raw_means <- country_raw %>%
  select_if(~any(is.na(.))) %>%
  gather(key = "field", value = "value") %>%
  group_by(field) %>%
  summarize(mean = mean(value, na.rm = T), min = min(value, na.rm = T), max = max(value, na.rm = T))

ggplot(data = imp2, aes(x = country_id,  y = value, color = m)) +
  geom_point() +
  geom_hline(data = raw_means, aes(yintercept = mean)) +
  # geom_hline(data = raw_means, aes(yintercept = min), lty= 2) +
  # geom_hline(data = raw_means, aes(yintercept = max), lty = 2) +
  facet_wrap(field ~., scales = "free")

