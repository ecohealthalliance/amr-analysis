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
  library(bayesplot)
  library(sjPlot)
  library(cowplot)
  library(ggthemes)
})
h <- here::here

for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(file)

# Set analysis parameters
imputed_sets <- 30
impute_iterations <- 40
seed <- 500

plan <- drake_plan(
  # pull country amr data
  model_data_with_na = generate_model_data_with_na(file_in(!!h("data/country-level-amr.csv"))),
  # use hueristics to replace some NAs
  model_data = generate_model_data(model_data_with_na),
  # use MICE to replace other NAs
  imputed_data = mice(model_data, m = imputed_sets, maxit = impute_iterations, method = 'cart', seed = seed),
  # data exploration markdown
  quantities_doc = render(knitr_in(!!h("quantities.Rmd")), output_file = file_out("quantities.html")),
  # fit brm
  brm_models = fit_brm_models(
    imputed_data, seed = seed,
    formula = bf(n_amr_events ~  ln_livestock_consumption_kg_per_capita + 
                   ln_migrant_pop_per_capita + ln_tourism_inbound_per_capita + ln_tourism_outbound_per_capita +
                   ln_ab_export_per_capita + ab_export_bin + health_expend_perc + 
                   human_consumption_ddd + english_spoken + 
                   ln_pubcrawl_per_capita + ln_promed_mentions_per_capita + ln_gdp_per_capita + offset(ln_population),
                 zi ~ ln_pubcrawl_per_capita + ln_promed_mentions_per_capita  + ln_gdp_per_capita + ln_population + english_spoken)),
  # get marginal effects on all model iterations
  all_marginal_effects = future_map(brm_models, ~marginal_effects(.)),
  # aggregate brm model
  brm_model_combined = combine_models(mlist = brm_models, check_data = FALSE), 
  # sample posterior y
  posterior_y = posterior_predict(brm_model_combined, nsamples = 1000),
  # sample posterior beta
  beta_samples = posterior_samples(brm_model_combined, subset = sample(nsamples(brm_model_combined), 500, replace = F)),
  # generate trace plots
  trace_plots = ggsave(plot_trace(brm_model_combined), 
                       filename = file_out(!!h("plots/diagnostics/trace.png")), 
                       width = 15, height = 15),
  # generate plots comparing posteriors to actual events
  posterior_plots = ggsave(plot_posteriors(y = model_data$n_amr_events, yrep = posterior_y), 
                           filename = file_out(!!h("plots/diagnostics/posterior_all.png")), 
                           width = 12, height= 4),
  # get model coefficients
  model_coefficients = get_coefficients(brm_model_combined),
  # which variables are consistent predictors
  predictor_terms = get_consistent_predictors(model_coefficients),
  # generate dot plot
  dot_plot = ggsave(coefficient_dot_plots(model_coefficients), 
                    filename = file_out(!!h("plots/dot_plot.png")), 
                    width = 6, height = 4),
  # generate marignal effects plot
  marginal_effects_plot = ggsave(plot_marginal_effects(all_marginal_effects, plot_labels, predictor_terms, model_data),
                    filename = file_out(!!h("plots/marginal_effects_multi.png")), 
                    width = 10, height = 21)
)

vis_drake_graph(plan)

future::plan(multiprocess, workers = floor(parallel::detectCores()/4))

# _drake.R must end with a call to drake_config().
# The arguments to drake_config() are basically the same as those to make().
config <- drake_config(plan, lock_envir = FALSE, # lock_envir=F needed for Stan
                       cache_log_file = "drake_cache_log.csv",)
config

