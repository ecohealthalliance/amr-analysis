plot_gdp <- function(data_v3_countries_in_range_gdp){
  
  dat <- data_v3_countries_in_range_gdp %>% 
    dplyr::select(iso3c, gdp_per_capita, livestock_consumption_kg_per_capita, human_consumption_ddd) %>% 
    mutate(impute_status = case_when(
      is.na(livestock_consumption_kg_per_capita) & is.na(human_consumption_ddd) ~ "Missing both human and livestock antibiotic consumption",
      !is.na(livestock_consumption_kg_per_capita) & is.na(human_consumption_ddd) ~ "Missing either human or livestock antibiotic consumption",
      is.na(livestock_consumption_kg_per_capita) & !is.na(human_consumption_ddd) ~ "Missing either human or livestock antibiotic consumption",
      !is.na(livestock_consumption_kg_per_capita) & !is.na(human_consumption_ddd) ~ "Not missing human or livestock antibiotic consumption"
    )) 
  
  ggplot(dat, aes(y = gdp_per_capita, x = impute_status)) +
    geom_boxplot(width = 0.05, fill = "gray90", outlier.color = "transparent", outlier.fill = "transparent") +
    geom_jitter(width = 0.1, color = "cornflowerblue", size = 1.5, alpha = 0.5) +
    labs(x = "", y = "GDP (dollars per capita)") +
    coord_flip() +
    theme_foundation(base_size = 10, base_family =  "sans") + 
    theme(rect = element_rect(fill = "white", linetype = 0, colour = NA),
          title = element_text(size = rel(1), face = "bold"), 
          axis.text = element_text(size = rel(1)), 
          axis.ticks = element_blank(),
          axis.line = element_blank(), 
          plot.title.position = "plot",
          panel.spacing = unit(2, "lines"),
          strip.text = element_text(size = rel(1.1)),
          panel.grid.major = element_line(colour = "gray50", linetype = 3), 
          panel.grid.minor = element_blank())
  
}