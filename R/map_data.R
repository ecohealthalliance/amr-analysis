get_map_data <- function(predicts){
  
  admin <- ne_countries(type='countries', scale = 'large') %>%
    st_as_sf() %>%
    mutate(iso3c = countrycode(sourcevar = name,
                               origin = "country.name",
                               destination = "iso3c"),
           iso3c = ifelse(is.na(iso3c), iso_a3_eh, iso3c)) %>%
    dplyr::select(name, iso3c)
  
  admin <- left_join(admin, predicts, by = "iso3c") %>%
    mutate(med = ifelse(v=="mean_pop", round(med, 0), med*10000)) %>%
    dplyr::select(name, iso3c, v, "Reported AMR Events" = n_amr_events, "Predicted AMR Events" = med) 
  
  return(admin)
}
