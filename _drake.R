# This file serves the r_*() functions (e.g. r_make()) documented at
# https://books.ropensci.org/drake/projects.html#safer-interactivity # nolint
# and
# https://docs.ropensci.org/drake/reference/r_make.html

# Load all your packages before calling make().

suppressPackageStartupMessages({
library(drake)
library(tidyverse)
library(brms)
library(here)
library(mice)
library(future)
library(furrr)
library(rmarkdown)
library(rstan)
library(StanHeaders)
})
h <- here::here

for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(file)

# Set analysis parameters
imputed_sets <- 30
impute_iterations <- 40
seed <- 500

plan <- drake_plan(
  model_data_with_na = generate_model_data_with_na(file_in(!!h("data/country-level-amr.csv"))),
  model_data = generate_model_data(model_data_with_na),
  imputed_data = mice(model_data, m = imputed_sets, maxit = impute_iterations, method = 'cart', seed = seed),
  quantities_doc = render(knitr_in(!!h("quantities.Rmd")), output_file = file_out("quantities.html")),
  brm_models = fit_brm_models(
    imputed_data, seed = seed,
    formula = bf(n_amr_events ~  ln_livestock_consumption_kg_per_capita + 
                   ln_migrant_pop_per_capita + ln_tourism_inbound_per_capita + ln_tourism_outbound_per_capita +
                   ln_ab_export_per_capita + ab_export_bin + health_expend_perc + 
                   human_consumption_ddd + english_spoken + 
                   ln_pubcrawl_per_capita + ln_promed_mentions_per_capita + ln_gdp_per_capita + offset(ln_population),
                 zi ~ ln_pubcrawl_per_capita + ln_promed_mentions_per_capita  + ln_gdp_per_capita + ln_population + english_spoken)),
  brm_model_combined = combine_models(mlist = brm_models, check_data = FALSE),
  all_marginal_effects = future_map(brm_models, ~marginal_effects(.))
)

future::plan(multiprocess, workers = floor(parallel::detectCores()/4))


# _drake.R must end with a call to drake_config().
# The arguments to drake_config() are basically the same as those to make().
config <- drake_config(plan, lock_envir = FALSE, # lock_envir=F needed for Stan
                       cache_log_file = "drake_cache_log.csv",)
config