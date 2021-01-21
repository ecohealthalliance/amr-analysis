init_data <- function(country_amr_file) {
  
  # Read in data
  read_csv(country_amr_file) %>%
    # remove rows if population or gdp data is unavailable
    drop_na(population, gdp_per_capita) %>% 
    select(-ab_export_bin, # not needed for convergence
           -tourism_outbound_per_capita) # too sparse
  
  # Remove correlated variables
}

split_data <- function(data, model_scenario){
  switch(model_scenario, 
         "v1_complete_only" = data %>%
           select(-n_amr_first_events, -livestock_consumption_kg_per_pcu) %>% 
           drop_na(human_consumption_ddd, livestock_consumption_kg_per_capita),
         "v2_human_or_animal" = data %>%
           select(-n_amr_first_events, -livestock_consumption_kg_per_pcu) %>% 
           filter(!(is.na(human_consumption_ddd) & is.na(livestock_consumption_kg_per_capita))),
         "v3_full_impute" = data %>%
           select(-n_amr_first_events, -livestock_consumption_kg_per_pcu)
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
    #mutate(ab_export_bin = replace_na(ab_export_bin, 0)) %>% 
    # log transform values
    mutate_at(vars(one_of("gdp_per_capita", "migrant_pop_per_capita", "population", "livestock_consumption_kg_per_capita", "tourism_inbound_per_capita", 
                          "promed_mentions_per_capita", "pubcrawl_per_capita",  "ab_export_per_capita", "ab_import_per_capita", "livestock_pcu", "livestock_consumption_kg_per_pcu")),
              ~log(.)) %>%
    rename_at(vars(one_of("gdp_per_capita", "migrant_pop_per_capita", "population", "livestock_consumption_kg_per_capita", "tourism_inbound_per_capita", 
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
