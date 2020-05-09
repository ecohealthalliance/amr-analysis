generate_model_data_with_na <- function(country_amr_file) {
  # Read in data
  read_csv(country_amr_file) %>%
    # remove rows if population or gdp data is unavailable
    drop_na(population, gdp_per_capita) %>%
    # remove column not using
    dplyr::select(-livestock_consumption_kg_per_pcu)
}


generate_model_data <- function(model_data_with_na) {
  # NA handling
  model_data_with_na %>%
    # amr events - assume 0 for NA
    mutate(n_amr_events = replace_na(n_amr_events, 0)) %>%
    # pubcrawler and promed - replace NAs and 0s with 1/2 min
    mutate_at(vars(pubcrawl_per_capita, promed_mentions_per_capita), 
              ~ifelse(is.na(.)|.==0,
                      0.5*min(.[.>0], na.rm = TRUE),
                      .)) %>% 
    # ab exports - replace NAs and 0s with mean (not doing this to import_per_capita because it only goes into tree impute model)
    mutate_at(vars(ab_export_per_capita), 
              ~ifelse(is.na(.)|.==0,
                      mean(., na.rm = TRUE),
                      .)) %>%
    # log transform values
    mutate_at(vars(gdp_per_capita, migrant_pop_per_capita, population, livestock_consumption_kg_per_capita, tourism_inbound_per_capita, tourism_outbound_per_capita, 
                   promed_mentions_per_capita, pubcrawl_per_capita,  ab_export_per_capita, ab_import_per_capita, livestock_pcu),
              ~log(.)) %>%
    rename_at(vars("livestock_consumption_kg_per_capita", "migrant_pop_per_capita", "promed_mentions_per_capita", "pubcrawl_per_capita",
                   "gdp_per_capita" , "population", "tourism_inbound_per_capita", "tourism_outbound_per_capita", "ab_export_per_capita", "ab_import_per_capita", "livestock_pcu"), ~paste0("ln_", .))
}



rehape_model_data <- function(model_data){
  model_data %>%
    select(-iso3c) %>%
    gather(key ="var", value = "x") %>%
    drop_na() %>%
    mutate(x_backtrans = ifelse(str_detect(var, "ln_"), exp(x), x))
}
