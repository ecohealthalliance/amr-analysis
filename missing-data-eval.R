
suppressPackageStartupMessages({
  library(drake)
  library(tidyverse)
  library(naniar)})


loadd(data_v4_full_impute)
miss_dat <- data_v4_full_impute %>% 
  select(iso3c, n_amr_events, human_consumption_ddd, livestock_consumption_kg_per_capita, ab_export_per_capita,
         tourism_inbound_per_capita, migrant_pop_per_capita,
         population, gdp_per_capita, health_expend_perc,
         pubcrawl_per_capita, promed_mentions_per_capita, english_spoken,
         everything()
  )

miss_dat %>% 
  select(-iso3c) %>% 
  visdat::vis_miss() +
  theme(axis.text.x =  element_text(size = 12))

miss_dat %>% 
  mutate(continent = countrycode::countrycode(iso3c, origin = "iso3c", destination = "region")) %>% 
  select(-iso3c) %>% 
  gg_miss_fct(fct  = continent) +
  coord_flip() +
  scale_y_discrete(position = "right")  +
  labs(x = "", y = "") +
  theme(axis.text.x = element_text(hjust=0),
        axis.text = element_text(size = 14))
  
loadd(data_trans_3)
data_trans_3 %>% 
  select(-iso3c) %>% 
  select(n_amr_events, human_consumption_ddd, ln_livestock_consumption_kg_per_capita, ln_ab_export_per_capita,
         ln_tourism_inbound_per_capita, ln_migrant_pop_per_capita,
         ln_population, ln_gdp_per_capita, health_expend_perc,
         ln_pubcrawl_per_capita, ln_promed_mentions_per_capita, english_spoken,
         everything()
  ) %>% 
  visdat::vis_miss() +
  theme(axis.text.x =  element_text(size = 12))

loadd(data_mice_3)

data_mice_3_comp <- mice::complete(data_mice_3, action = "long")  %>% 
  select(iso3c, n_amr_events, human_consumption_ddd, ln_livestock_consumption_kg_per_capita) %>% 
  pivot_longer(cols = c(human_consumption_ddd, ln_livestock_consumption_kg_per_capita))

which_complete <- data_trans_3 %>% 
  select(iso3c, n_amr_events, human_consumption_ddd, ln_livestock_consumption_kg_per_capita) %>% 
  mutate(human_consumption_ddd = is.na(human_consumption_ddd),
         ln_livestock_consumption_kg_per_capita = is.na(ln_livestock_consumption_kg_per_capita)) %>% 
  pivot_longer(cols = c(human_consumption_ddd, ln_livestock_consumption_kg_per_capita)) %>% 
  rename(missing = value) %>% 
  mutate(missing = ifelse(missing, "imputed", "measured"))


data_mice_3_comp2 <- data_mice_3_comp %>% 
  left_join(which_complete)  %>% 
  distinct() %>% 
  mutate(missing = factor(missing, levels = c("measured", "imputed")))

ggplot(data = data_mice_3_comp2,  aes(x = value, y = n_amr_events, color = missing)) +
  geom_point(data = subset(data_mice_3_comp2, missing == "imputed"), aes(x = value, y = n_amr_events), color = "cornflowerblue", size = 1) +
  geom_point(data = subset(data_mice_3_comp2, missing == "measured"), aes(x = value, y = n_amr_events), color = "black", size = 2) +
  facet_wrap(name~.,scales = "free_x") +
  labs(x = "", color = "") +
  scale_color_manual(values = c("imputed" = "cornflowerblue", "measured" = "black")) +
  theme_minimal() +
  theme(axis.title = element_text(size = 16), strip.text = element_text(size = 16))


densityplot(data_mice_3)
  
