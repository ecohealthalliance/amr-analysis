init_data <- function(country_amr_file) {
  # Read in data
  read_csv(country_amr_file) %>%
    # remove rows if population or gdp data is unavailable
    drop_na(population, gdp_per_capita)
}

split_data <- function(data, model_scenario){
  switch(model_scenario, 
         "baseline" = data %>%
           select(-n_amr_first_events, -livestock_consumption_kg_per_pcu),
         "global" = data %>% 
           select(-n_amr_events, -livestock_consumption_kg_per_pcu) %>% 
           rename(n_amr_events = n_amr_first_events),
         "remove_us" = data %>%
           select(-n_amr_first_events, -livestock_consumption_kg_per_pcu) %>% 
           filter(iso3c != "USA"),
         "remove_outbound_tourism" = data %>%
           select(-n_amr_first_events, -tourism_outbound_per_capita, -livestock_consumption_kg_per_pcu),
         "remove_livestock_biomass" = data %>%
           select(-n_amr_first_events, -livestock_consumption_kg_per_capita) 
  )
}

transform_data <- function(split_data) {
  # NA handling
  split_data %>%
    # amr events - assume 0 for NA
    mutate_at(.vars = c("n_amr_events"), ~replace_na(., 0)) %>% 
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
    mutate(ab_export_bin = replace_na(ab_export_bin, 0)) %>% 
    # log transform values
    mutate_at(vars(one_of("gdp_per_capita", "migrant_pop_per_capita", "population", "livestock_consumption_kg_per_capita", "tourism_inbound_per_capita", "tourism_outbound_per_capita", 
                   "promed_mentions_per_capita", "pubcrawl_per_capita",  "ab_export_per_capita", "ab_import_per_capita", "livestock_pcu", "livestock_consumption_kg_per_pcu")),
              ~log(.)) %>%
    rename_at(vars(one_of("gdp_per_capita", "migrant_pop_per_capita", "population", "livestock_consumption_kg_per_capita", "tourism_inbound_per_capita", "tourism_outbound_per_capita", 
                          "promed_mentions_per_capita", "pubcrawl_per_capita",  "ab_export_per_capita", "ab_import_per_capita", "livestock_pcu", "livestock_consumption_kg_per_pcu")),
              ~paste0("ln_", .))
}


rehape_data <- function(data_trans){
  data_trans %>%
    dplyr::select(-iso3c) %>%
    gather(key ="var", value = "x") %>%
    drop_na() %>%
    mutate(x_backtrans = ifelse(str_detect(var, "ln_"), exp(x), x))
}
