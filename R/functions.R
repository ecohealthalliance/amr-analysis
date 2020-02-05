get_me_dat <- function(me){
  imap_dfr(me, function(x, y){
    x %>% 
      select(value = y, cond__:upper__) %>%
      mutate(var = y)
  })
}

show_imputes <- function(mice, m, raw){
  
  imp <- mice$imp %>% compact()
  
  m <- as.character(m)
  
  imp2 <- imap_dfr(imp, function(x, y){
    x %>%
      as_tibble() %>%
      rownames_to_column(var = "country_id") %>%
      gather(key = "m", value = "value", `1`:m) %>%
      mutate(field = y)
  })
  
  raw_means <- raw %>%
    select_if(~any(is.na(.))) %>%
    gather(key = "field", value = "value") %>%
    group_by(field) %>%
    summarize(mean = mean(value, na.rm = T), min = min(value, na.rm = T), max = max(value, na.rm = T))
  
  ggplot(data = imp2, aes(x = country_id,  y = value, color = m)) +
    geom_point() +
    geom_hline(data = raw_means, aes(yintercept = mean)) +
    facet_wrap(field ~., scales = "free")
}

# labelling for plots
lookup_vars <- c(
  # consumption vars
  "human_consumption_ddd" = "Human AB Consumption (DDD)", 
  "ln_livestock_consumption_kg_per_capita" = "Livestock AB Consumption (kg per capita)",
  # "ln_livestock_consumption_kg_per_pcu" = "Livestock AB Consumption (per PCU)",
  # "ln_livestock_pcu" = "Livestock Population (PCU)", 
  
  # production
  "ln_ab_export_per_capita" = "AB Exports (dollars per capita)", 
  "ab_export_bin" = "AB Exported (yes/no)",
  
  # population movement
  "ln_tourism_outbound_per_capita" = "Tourism - Outbound (per capita)", 
  "ln_tourism_inbound_per_capita"  = "Tourism - Inbound (per capita)",
  "ln_migrant_pop_per_capita" = "Migrant Population (per capita)",  
  
  # economic activity
  "health_expend_perc" ="Health Expenditure (% GDP)", 
  "ln_gdp_per_capita" = "GDP (dollars per capita)", 
  
  # surveillance 
  "ln_population" = "Population",
  "english_spoken" = "English Spoken (yes/no)", 
  "ln_pubcrawl_per_capita" = "Publication Bias Index (per capita)",
  "ln_promed_mentions_per_capita" = "ProMed Mentions (per capita)")