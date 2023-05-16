
# labelling for plots
lookup_vars <- c(
  
  # surveillance 
  "ln_population" = "Population (log)",
  "english_spoken" = "English Spoken (yes/no)", 
  "ln_pubcrawl_per_capita" = "Publication Bias Index (log per cap)",
  "ln_promed_mentions_per_capita" = "ProMed Mentions (log per cap)",
  
  # gdp
  "ln_gdp_per_capita" = "GDP (log dollars per cap)", 
  "health_expend_perc" = "Health Expenditure (% GDP)", 
  
  # consumption vars
  "human_consumption_ddd" = "Human AB Consumption (DDD per capita)", 
  "ln_livestock_consumption_kg_per_capita" = "Livestock AB Consumption (log kg per cap)",
  "ln_livestock_consumption_kg_per_capita.ln_gdp_per_capita"= "Livestock AB Consumption & GDP Interaction",
  "ln_livestock_consumption_kg_per_capita:ln_gdp_per_capita"= "Livestock AB Consumption & GDP Interaction",
  "ln_livestock_consumption_kg_per_pcu" = "Livestock AB Consumption (log kg per PCU)",
  "ln_livestock_pcu" = "Livestock Population (log PCU)", 
  
  # production
  "ln_ab_export_per_capita" = "AB Exports (log dollars per cap)", 
  "ab_export_bin" = "AB Exported (yes/no)",
  
  # population movement
  "ln_tourism_outbound_per_capita" = "Tourism - Outbound (log per cap)", 
  "ln_tourism_inbound_per_capita"  = "Tourism - Inbound (log per cap)",
  "ln_migrant_pop_per_capita" = "Migrant Population (log per cap)" 
)
