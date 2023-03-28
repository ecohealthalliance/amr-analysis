
# labelling for plots
lookup_vars <- c(
  
  # surveillance 
  "ln_population" = "ln(Population)",
  "english_spoken" = "English Spoken (yes/no)", 
  "ln_pubcrawl_per_capita" = "ln(Publication Bias Index [per cap.])",
  "ln_promed_mentions_per_capita" = "ln(ProMed Mentions [per cap.])",
  
  # gdp
  "ln_gdp_per_capita" = "ln(GDP [dollars per cap.])", 
  "health_expend_perc" = "Health Expenditure (% GDP)", 
  
  # consumption vars
  "human_consumption_ddd" = "Human AB Consumption (DDD)", 
  "ln_livestock_consumption_kg_per_capita" = "Livestock AB Consumption (kg per cap.)",
  "ln_livestock_consumption_kg_per_capita.ln_gdp_per_capita"= "Livestock AB Consumption:GDP",
  "ln_livestock_consumption_kg_per_capita:ln_gdp_per_capita"= "Livestock AB Consumption:GDP",
  "ln_livestock_consumption_kg_per_pcu" = "ln(Livestock AB Consumption [per PCU])",
  "ln_livestock_pcu" = "ln(Livestock Population [PCU])", 
  
  # production
  "ln_ab_export_per_capita" = "ln(AB Exports [dollars per cap.])", 
  "ab_export_bin" = "AB Exported (yes/no)",
  
  # population movement
  "ln_tourism_outbound_per_capita" = "ln(Tourism - Outbound [per cap.])", 
  "ln_tourism_inbound_per_capita"  = "ln(Tourism - Inbound [per cap.])",
  "ln_migrant_pop_per_capita" = "ln(Migrant Population [per cap.])" 
)
