interactive_map <- function(events, locations, map_data){
  
  admin_mean <- map_data %>%
    filter(is.na(v) | v == "mean_pop") 
  
  pal1 <- colorNumeric("OrRd", domain = c(admin_mean$`Predicted AMR Events`, admin_mean$`Reported AMR Events`), na.color = "#e9e9f0")
  
  # caption <- glue::glue(nrow(events), " AMR emergence events<br/>",
  #                       n_distinct(events$study_country), " countries<br/>",
  #                       n_distinct(events$drug), " antimicrobial drugs<br/>",
  #                       n_distinct(events$bacteria), " resistant bacteria<br/>",
  #                       str_sub(min(events$start_date), 1, 4), " - ",  str_sub(max(events$start_date), 1, 4))
  
  leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addFullscreenControl(position = "topright") %>%
    addPolygons(data = admin_mean, 
                stroke = TRUE, color = "#46464a", weight = 1,
                fill = TRUE, fillColor = ~pal1(`Predicted AMR Events`), fillOpacity = 0.9,
                label = ~paste0(name, ": ", `Predicted AMR Events`), group = "Predicted") %>%
    addPolygons(data = admin_mean, 
                stroke = TRUE, color = "#46464a", weight = 1,
                fill = TRUE, fillColor = ~pal1(`Reported AMR Events`), fillOpacity = 0.9,
                label = ~paste0(name, ": ", `Reported AMR Events`), group = "Reported") %>%
    addCircleMarkers(data = locations,  radius = 3,
                     lng = ~jitter(lon_study), lat = ~jitter(lat_study), 
                     stroke = TRUE, color = "#210106", opacity = 1, weight = 1,
                     fill = TRUE, fillColor = "#210106", fillOpacity = 0.5,
                     label = ~study_location, group = "Reported") %>%
    addLegend(data = admin_mean, pal = pal1, values = ~`Predicted AMR Events`, position = "bottomright", title = "AMR Emergence Events") %>% 
   # addControl(caption) %>%
    addLayersControl(baseGroups = c("Reported", "Predicted"), options = layersControlOptions(collapsed = FALSE), position = "bottomleft")  
  
}

