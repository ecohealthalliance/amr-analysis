# This file serves the r_*() functions (e.g. r_make()) documented at
# https://books.ropensci.org/drake/projects.html#safer-interactivity # nolint
# and
# https://docs.ropensci.org/drake/reference/r_make.html

# Load all your packages before calling make().

suppressPackageStartupMessages({
  library(drake)
  library(tidyverse)
  library(brms) # 2.16.0
  library(here)
  library(mice)
  library(future)
  library(furrr)
  library(rmarkdown)
  library(rstan) # 2.21.1
  library(StanHeaders)
  library(bayesplot)
  library(sjPlot)
  library(cowplot)
  library(ggthemes)
  library(rnaturalearth)
  library(rnaturalearthdata)
  library(rnaturalearthhires) # ropensci/rnaturalearthhires
  library(countrycode)
  library(sf)
  library(leaflet)
  library(leaflet.extras)
  library(httr)
  library(PerformanceAnalytics)
  library(visNetwork)
})
h <- here::here
pkgconfig::set_config("drake::strings_in_dots" = "literals")

for (file in list.files(h("R"), pattern = "\\.R$", full.names = TRUE)) source(file)

# Set analysis parameters
imputed_sets <- 30
impute_iterations <- 40
seed <- 101
set.seed(seed)

#TODO add labels to plots

labs <- c("v1_complete_only", 
          "v2_human_or_animal", 
          "v3_countries_in_range_gdp", 
          "v3.1_us_removed", 
          "v3.2_livestock_biomass_included", 
          "v3.3_first_global_emergence",
          "v4_full_impute"
)

plan <- drake_plan(
  
  #### Data Preparation ####
  # pull country amr data & split into scenarios
  data = target(
    init_data(file_in(!!h("data/country-level-amr.csv")))
  ),
  # check correlations on raw data
  cor_matrix_data = target(
    data %>%
      dplyr::select(-iso3c, # character
                    -n_amr_events, -n_amr_first_events, # outputs
                    -livestock_pcu, -livestock_consumption_kg_per_pcu, -ab_import_per_capita, # not in final model
                    -english_spoken # binary
      ) %>%
      cor(., method = "spearman", use = "pairwise.complete.obs")
  ),  
  # 1
  data_v1_complete_only = target(
    split_data(data, "v1_complete_only")
  ),
  # 2
  data_v2_human_or_animal = target(
    split_data(data, "v2_human_or_animal")
  ),
  # 3
  data_v3_countries_in_range_gdp = target(
    split_data(data, "v3_countries_in_range_gdp")
  ),
  plot_data_v3_countries_in_range_gdp = target(
    ggsave(plot_gdp(data_v3_countries_in_range_gdp),
           filename = file_out(!!h(paste0("plots/gdp_by_impute_status.png"))), 
           width = 8, height = 4)
  ),
  #3.1
  data_v3.1_us_removed = target(
    data_v3_countries_in_range_gdp %>% filter(iso3c != "USA")
  ),
  # 3.2
  data_v3.2_livestock_biomass_included = target(
    split_data(data, "v3.2_livestock_biomass_included")
  ),
  # 3.3
  data_v3.3_first_global_emergence = target(
    split_data(data, "v3.3_first_global_emergence")
  ),
  # 4
  data_v4_full_impute = target(
    split_data(data, "v4_full_impute")
  ),
  # use hueristics to replace some NAs, log transform vars
  data_trans = target(
    transform_data(split_data = input_data),
    transform = cross(input_data = c(data_v1_complete_only, 
                                     data_v2_human_or_animal, 
                                     data_v3_countries_in_range_gdp, 
                                     data_v3.1_us_removed, 
                                     data_v3.2_livestock_biomass_included, 
                                     data_v3.3_first_global_emergence, 
                                     data_v4_full_impute), .id = FALSE)
  ),
  # gather data (for plotting later)
  data_reshape = target(
    rehape_data(data_trans),
    transform = map(data_trans, .id = FALSE)
  ),
  # use MICE to replace other NAs
  data_mice =  target(
    mice::mice(data_trans, m = imputed_sets, maxit = impute_iterations, method = 'cart', seed = seed),
    transform = map(data_trans, .id = FALSE)
  ),
  # extract completed MICE data
  data_mice_compl = target(
    mice::complete(data_mice, action = "long") %>% 
      group_by(.id, iso3c) %>% 
      summarize_all(mean) %>% 
      ungroup()  %>% 
      select(-.id),
    transform = map(data_mice, .id = FALSE)
  ),
  #### Model Fitting ####
  # fit brm hurdle model
  formula = target(
    formula,
    cross(formula = c(!!main_formula, # v1
                      !!main_formula, # v2
                      !!main_formula, # v3
                      !!main_formula, # v3.1
                      !!livestock_biomass_included_formula, # v3.2
                      !!main_formula, # v3.3
                      !!main_formula), #v4
          .id = FALSE)),
  
  mod_fit =  target(
    fit_brm_model(data_mice, 
                  seed = seed, 
                  formula),
    transform = map(data_mice, formula)
  ),
  # get marginal effects on all model iterations
  marg_eff = target(
    furrr::future_map(mod_fit, ~brms::marginal_effects(.)), 
    transform = map(mod_fit, .id = FALSE)
  ),
  # aggregate brm model
  mod_comb =  target(
    brms::combine_models(mlist = mod_fit, check_data = FALSE),
    transform = map(mod_fit, .id = FALSE)
  ),
  # sample posterior y
  post_y = target(
    brms::posterior_predict(mod_comb, nsamples = 1000),
    transform = map(mod_comb, .id = FALSE)
  ),
  # sample posterior beta
  betas = target(
    brms::posterior_samples(mod_comb, subset = sample(nsamples(mod_comb), 500, replace = FALSE)),
    transform = map(mod_comb, .id = FALSE)
  ),
  # get zi var names (from logistic regression)
  zi_vars = target(
    get_zi_vars(betas),
    transform = map(betas, .id = FALSE)
  ),
  # get pois var names (from poisson regression)
  pois_vars = target(
    get_pois_vars(betas),
    transform = map(betas, .id = FALSE)
  ),
  
  #### Diagnostics ####
  # trace plots
  trace_plots = target(
    ggsave(plot_trace(mod_comb),
           filename = file_out(!!h(paste0("plots/diagnostics/trace_", lab, ".png"))), 
           width = 15, height = 15),
    transform = map(mod_comb, lab = !!labs, .id = FALSE)
  ),
  # other posterior plots
  post_plots = target(
    ggsave(plot_posteriors(data_trans, post_y),
           filename = file_out(!!h(paste0("plots/diagnostics/posterior_", lab, ".png"))),
           width = 12, height= 4),
    transform = map(data_trans, post_y, lab = !!labs, .id = FALSE)
  ),
  
  #### Model Summary ####
  # get model coefficients
  coefs = target(
    get_coefficients(mod_comb),
    transform = map(mod_comb, .id = FALSE)
  ),
  # export model coefficients for reporting
  coef_tbl = target(
    write_csv(export_coefficient_table(coefs),
           file = file_out(!!h(paste0("doc/coef_values_", lab, ".csv")))),
    transform = map(coefs, lab = !!labs, .id = FALSE)
    ),
  # coefficient dot plot
  coef_plot = target(
    ggsave(plot_coefficients(coefs, lab),
           filename = file_out(!!h(paste0("plots/dot_plot_", lab, ".png"))),
           width = 8, height = 4),
    transform = map(coefs, lab = !!labs, .id = FALSE)
  ),
  # which variables are consistent predictors?
  consistent_preds = target(
    get_consistent_predictors(coefs),
    transform = map(coefs, .id = FALSE)
  ),
  # marginal effects plot
  marg_eff_plot = target(
    ggsave(plot_marginal_effects(marg_eff, lookup_vars, consistent_preds, data_reshape),
           filename = file_out(!!h(paste0("plots/marginal_effects_multi_", lab, ".png"))),
           width = 10, height = 21),
    transform = map(marg_eff, consistent_preds, data_reshape, lab = !!labs, .id = FALSE)
  ),
  # get model predictions
  predicts = target(
    get_predictions(data_mice_compl, betas, pois_vars),
    transform = map(data_mice_compl, betas, pois_vars, .id = FALSE)
  ),
  # generate partial effects plot - logistic vars
  zi_part_plot = target(
    ggsave(plot_zi_partial_effects(betas, zi_vars, data_mice_compl, data_reshape),
           filename = file_out(!!h(paste0("plots/zi_partial_effects_", lab, ".png"))),
           width = 12, height = 6),
    transform = map(betas, zi_vars, data_mice_compl, data_reshape, lab = !!labs, .id = FALSE)
  ),
  # generate partial effects plot - poisson vars
  pois_part_plot = target(
    ggsave(plot_pois_partial_effects(betas, pois_vars, data_mice_compl, data_reshape),
           filename = file_out(!!h(paste0("plots/pois_partial_effects_", lab, ".png"))),
           width = 14, height = 12),
    transform = map(betas, pois_vars, data_mice_compl, data_reshape, lab = !!labs, .id = FALSE)
  ),
  # get map data
  map_data = target(
    get_map_data(predicts),
    transform = map(predicts, .id = FALSE)
  ),
  # get differences between predicted and actual
  predicted_versus_actual_diff = target(
    get_predicted_versus_actual_diff(predicts),
    transform = map(predicts, .id = FALSE)
  ),
  # combine slope and map into single figure
  ms_plot = target(
    ggsave(plot_grid(plot_map(map_data), plot_slope(predicted_versus_actual_diff), labels = "AUTO"),
           filename = file_out(!!h(paste0("plots/map_and_slope_", lab, ".png"))),
           width = 14, height = 8),
    transform = map(map_data, predicts, lab = !!labs, .id = FALSE)
  ),
  # for interactive map: get dataframe of AMR events
  events = get_events(),
  # for interactive map: get coordinate locations of AMR events
  locations = get_locations(events),
  # generate interactive map
  predict_map_interactive = target(
    htmlwidgets::saveWidget(interactive_map(events, locations, map_data),
                            file = file_out(!!h(paste0("plots/map_predictions_interactive_", lab, ".html")))),
    transform = map(map_data, lab = !!labs, .id = FALSE)
  ),
  # model exploration markdown
  quantities_model_doc = render(knitr_in(!!h("doc/quantities_model.Rmd")), output_file = file_out(!!h("doc/quantities_model.html")))
)


vis_drake_graph(plan, targets_only = TRUE)

future::plan(multisession, workers = floor(parallel::detectCores()/4))

drake::make(plan, lock_envir = FALSE, # lock_envir=F needed for Stan
            cache_log_file = "drake_cache_log.csv")


# _drake.R must end with a call to drake_config().
# The arguments to drake_config() are basically the same as those to make().
config <- drake_config(plan, lock_envir = FALSE, # lock_envir=F needed for Stan
                       cache_log_file = "drake_cache_log.csv")
config
