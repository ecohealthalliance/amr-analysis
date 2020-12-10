plot_map <- function(map_data){
  
  # Get adm
  admin_mean <- map_data %>%
    filter(is.na(v) | v == "mean_pop") %>% 
    select(-v) %>%
    gather(-name, -iso3c, -geometry, key = "key", value = "value") %>%
    mutate(key = factor(key, levels = c("Reported AMR Events", "Predicted AMR Events"), labels = c("Reported", "Predicted"))) %>%
    filter(name != "Antarctica")
  
  pal1 <- colorNumeric("OrRd", domain = c(admin_mean$`Predicted AMR Events`, admin_mean$`Reported AMR Events`), na.color = "#e9e9f0")
  
  ggplot(admin_mean) + 
    geom_sf(aes(fill = value), size = 0.1) +
    facet_wrap(key~., strip.position="top", ncol = 1) +
    scale_fill_viridis_c(option = "plasma", alpha = 0.8) +
    labs(fill = "AMR Emergence Count") +
    theme_foundation(base_size = 11, base_family =  "sans") + 
    coord_sf() +
    theme(strip.background = element_blank(), 
          strip.text = element_text(size = rel(1)), 
          rect = element_rect(fill = "white", linetype = 0, colour = NA),
          #title = element_text(size = rel(1.1), face = "bold"), 
          axis.text = element_blank(), 
          axis.ticks = element_blank(),
          axis.line = element_blank(), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          legend.position = "right")
  # legend.title = element_text(size = rel(0.9)),
  # legend.text = element_text(size = rel(0,8)), 
  # legend.position = "left")
}

