library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)

admin <- ne_countries(type='countries', scale = 'medium', returnclass = "sf") %>% 
  select(iso_a3) %>% 
  drop_na(iso_a3)

df <- tibble(iso_a3 = admin$iso_a3, value = rnorm(n = nrow(admin), 1, 1))
  
admin <- left_join(admin, df, by = "iso_a3")

ggplot(admin) + 
  geom_sf(aes(fill = value), color = "transparent") +
  scale_fill_viridis_c(option = "plasma", alpha = 0.8) +
  coord_sf() +
  theme(strip.background = element_blank(), 
        strip.text = element_text(size = rel(1)), 
        rect = element_rect(fill = "white", linetype = 0, colour = NA),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        axis.line = element_blank(), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
