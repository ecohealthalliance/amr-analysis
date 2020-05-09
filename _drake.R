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
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(countrycode)
  library(sf)
  library(leaflet)
  library(leaflet.extras)
  library(httr)
})
h <- here::here

for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(file)

# Set analysis parameters
imputed_sets <- 30
impute_iterations <- 40
seed <- 101
set.seed(seed)

plan <- drake_plan(
  
  #### Data Preparation ####
  # pull country amr data
  model_data_with_na = generate_model_data_with_na(file_in(!!h("data/country-level-amr.csv"))),
  # use hueristics to replace some NAs
  model_data = generate_model_data(model_data_with_na),
  # gather data (for plotting later)
  model_data_long = rehape_model_data(model_data),
  # use MICE to replace other NAs
  imputed_data = mice(model_data, m = imputed_sets, maxit = impute_iterations, method = 'cart', seed = seed),
  # extract completed MICE data
  imputed_data_complete = mice::complete(imputed_data),
  # data exploration markdown
  quantities_data_doc = render(knitr_in(!!h("doc/quantities_data.Rmd")), output_file = file_out(!!h("doc/quantities_data.html"))),
  
  #### Model Fitting ####
  # fit brm hurdle model
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
  beta_samples = posterior_samples(brm_model_combined, subset = sample(nsamples(brm_model_combined), 500, replace = FALSE)),
  # get zi var names (from logistic regression)
  zi_vars = get_zi_vars(beta_samples),
  # get pois var names
  pois_vars = get_pois_vars(beta_samples),
  
  #### Diagnostics ####
  # generate trace plots
  trace_plots = ggsave(plot_trace(brm_model_combined),
                       filename = file_out(!!h("plots/diagnostics/trace.png")),
                       width = 15, height = 15),
  # generate plots comparing posteriors to actual events
  posterior_plots = ggsave(plot_posteriors(y = model_data$n_amr_events, yrep = posterior_y),
                           filename = file_out(!!h("plots/diagnostics/posterior_all.png")),
                           width = 12, height= 4),
  
  #### Model Summary ####
  # get model coefficients
  model_coefficients = get_coefficients(brm_model_combined),
  # which variables are consistent predictors
  predictor_terms = get_consistent_predictors(model_coefficients),
  # generate dot plot
  dot_plot = ggsave(coefficient_dot_plots(model_coefficients),
                    filename = file_out(!!h("plots/dot_plot.png")),
                    width = 6, height = 4),
  # generate marignal effects plot
  marginal_effects_plot = ggsave(plot_marginal_effects(all_marginal_effects, plot_labels, predictor_terms, model_data_long),
                                 filename = file_out(!!h("plots/marginal_effects_multi.png")),
                                 width = 10, height = 21),
  # generate partial effects plot - logistic vars
  zi_partial_effects_plot = ggsave(plot_zi_partial_effects(beta_samples, zi_vars, imputed_data_complete, model_data_long),
                                   filename =  file_out(!!h("plots/zi_partial_effects.png")),
                                   width = 12, height = 6),
  # generate partial effects plot - poisson vars
  pois_partial_effects_plot = ggsave(plot_pois_partial_effects(beta_samples, pois_vars, imputed_data_complete, model_data_long),
                                     filename =  file_out(!!h("plots/pois_partial_effects.png")),
                                     width = 12, height = 12),
  # get model predictions 
  model_predictions = get_model_predictions(imputed_data_complete, beta_samples, pois_vars),
  # get map data
  map_data = get_map_data(model_predictions),
  # generate slope plot
  predict_slope_plot = plot_slope(model_predictions),
  # generate map
  predict_map = plot_map(map_data),
  # combine slope and map into single figure
  map_and_slope = ggsave(plot_grid(predict_map, predict_slope_plot, labels = "AUTO"),
                         filename = file_out(!!h("plots/map_and_slope.png")), 
                         width = 14, height = 8),
  # for interactive map: get dataframe of AMR events
  events = get_events(),
  # for interactive map: get coordinate locations of AMR events
  locations = get_locations(events),
  # generate interactive map
  predict_map_interactive = htmlwidgets::saveWidget(interactive_map(events, locations, map_data),
                                                    file = file_out(!!h("plots/map_predictions_interactive.html"))),
  # model exploration markdown
  quantities_model_doc = render(knitr_in(!!h("doc/quantities_model.Rmd")), output_file = file_out(!!h("doc/quantities_model.html")))
  
)

vis_drake_graph(plan)

future::plan(multiprocess, workers = floor(parallel::detectCores()/4))

# _drake.R must end with a call to drake_config().
# The arguments to drake_config() are basically the same as those to make().
config <- drake_config(plan, lock_envir = FALSE, # lock_envir=F needed for Stan
                       cache_log_file = "drake_cache_log.csv",)
config

