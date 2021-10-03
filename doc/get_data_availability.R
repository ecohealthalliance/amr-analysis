library(drake)

loadd(data)
names(data)

get_pos <- function(data, var){
  data %>% 
    filter(!is.na(get(var))) %>%
    filter(get(var) != 0) %>% 
    nrow()
}

get_pos(data, var = "n_amr_events")
get_pos(data, var = "pubcrawl_per_capita")
get_pos(data, var = "promed_mentions_per_capita")
get_pos(data, var = "ab_export_per_capita")
get_pos(data, var = "ab_import_per_capita")
get_pos(data, var = "migrant_pop_per_capita")
get_pos(data, var = "tourism_inbound_per_capita")
get_pos(data, var = "livestock_consumption_kg_per_capita")
get_pos(data, var = "livestock_pcu")

