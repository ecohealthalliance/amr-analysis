library(tidyverse)
library(brms)
library(ggthemes)
library(rnaturalearth)
library(rnaturalearthdata)
library(countrycode)
library(sf)
library(leaflet)
library(leaflet.extras)
library(httr)
library(sjPlot)
set.seed(101)

h <- here::here
source(h("R/functions.R"))

# Setup -------------------------------------------------------------------

# model results
fit_combined <- read_rds(h("model/fit_combined.rds"))
me_all <- read_rds(h("model/fit_all_marginal_effects.rds"))

# data - raw and imputed
amr_mice <- read_rds(h("model/mice-imputation.rds")) 

amr_with_imputes <- amr_mice %>% 
  mice::complete(.) %>% 
  select(-ln_ab_import_per_capita, -ln_livestock_pcu) %>%
  mutate(country = countrycode::countrycode(sourcevar = iso3c,
                                            origin = "iso3c",
                                            destination = "country.name"))

amr_raw <- amr_mice$data %>%
  select(-iso3c) %>%
  gather(key ="var", value = "x") %>%
  drop_na()

# get posterior samples of data
beta_samples <- posterior_samples(fit_combined, subset = sample(nsamples(fit_combined), 500, replace = F)) 

# get zi vars
zi_vars <- beta_samples %>%
  select(matches("zi_"), -b_zi_Intercept) %>%
  colnames(.) %>%
  gsub("^b_zi_", "", .)

# get pois vars
pois_vars <- beta_samples %>%
  select(-matches("zi_"), -b_Intercept, -lp__) %>%
  colnames(.)%>%
  gsub("^b_", "", .)

# labelling for plots
lookup_vars <- c(
  # consumption vars
  "human_consumption_ddd" = "Human AB Consumption (DDD)", 
  "ln_livestock_consumption_kg_per_capita" = "Livestock AB Consumption (kg per capita; ln scale)",
  # "ln_livestock_consumption_kg_per_pcu" = "Livestock AB Consumption (per PCU; ln scale)",
  # "ln_livestock_pcu" = "Livestock Population (PCU; ln scale)", 
  
  # production
  "ln_ab_export_per_capita" = "AB Exports (dollars per capita)", 
  "ab_export_bin" = "AB Exported (yes/no)",
  
  # population movement
  "ln_tourism_outbound_perc" = "Tourism - Outbound (% Pop; ln scale)", 
  "ln_tourism_inbound_perc"  = "Tourism - Inbound (% Pop; ln scale)",
  "ln_migrant_pop_perc" = "Migrant Population (% Pop.; ln scale)",  
  
  # economic activity
  "health_expend_perc" ="Health Expenditure (% GDP)", 
  "ln_gdp_per_capita" = "GDP (dollars per capita; ln scale)", 
  
  # surveillance 
  "ln_population" = "Population (ln scale)",
  "english_spoken" = "English Spoken (yes/no)", 
  "ln_pubcrawl_per_capita" = "PubCrawler Publication Bias Index (per capita; ln scale)",
  "ln_promed_mentions_per_capita" = "ProMed Mentions (per capita; ln scale)")

global_labeller <- labeller(
  var = lookup_vars
)

# event info
url_events <- "https://raw.githubusercontent.com/ecohealthalliance/amr-db/master/data/events_db.csv"
events <- GET(url_events, authenticate("emmamendelsohn", Sys.getenv("GITHUB_PAT")))
events <- read_csv(content(events, "text")) 

url_locs <- "https://raw.githubusercontent.com/ecohealthalliance/amr-db/master/data/locations.csv"
locs <- GET(url_locs, authenticate("emmamendelsohn", Sys.getenv("GITHUB_PAT")))
locs <- read_csv(content(locs, "text")) %>%
  filter(study_id %in% events$study_id) %>%
  filter(!is.na(study_location)) 

# Summary and Coefficients ------------------------------------------------------------
summary(fit_combined)

brms::bayes_R2(fit_combined, summary = TRUE)

# dot plot
coefs <- get_model_data(fit_combined, type = "est") %>%
  distinct() %>%
  mutate(term_clean = as.character(term)) %>%
  mutate(term_clean = lookup_vars[term_clean]) %>%
  mutate(term_clean = fct_reorder(term_clean, estimate)) %>%
  mutate(est = round(estimate, 2)) %>%
  mutate(predictor = !(1 >= conf.low & 1 <= conf.high)) %>%
  mutate(lab = ifelse(predictor, paste0(est, "*"), est))

predictors <- coefs %>%
  filter(predictor) %>%
  mutate(lab = "*") %>%
  select(term, lab)

ggplot(coefs, aes(x = term_clean, y = estimate)) + 
  geom_hline(yintercept = 1, color = "gray60") +
  geom_segment(aes(y = conf.low, yend = conf.high, xend = term_clean), color = "cornflowerblue") +
  geom_point(aes(color = group), show.legend = FALSE) +
  geom_text(aes(label = lab), nudge_x = 0.25) +
  scale_color_manual(values = c("neg" = "cornflowerblue", "pos" = "cornflowerblue")) +
  labs(x = "", y = "Incidence Rate Ratio") +
  coord_flip() +
  theme_bw() +
  theme(axis.text = element_text(color = "black"))

ggsave(filename = h("plots/dot_plot.png"), width = 8)

# Marginal effects --------------------------------------------------------
me_all2 <- imap_dfr(me_all, function(x, y){
  get_me_dat(x) %>%
    mutate(iteration = y) 
}) %>%
  mutate(var = factor(var, levels = names(lookup_vars)))

me_all2_avg <- me_all2 %>%
  group_by(value, var) %>%
  summarize(mean = mean(estimate__)) %>%
  ungroup() 

predictors <- me_all2_avg %>%
  group_by(var) %>%
  summarize(xval = 0.9 * max(value),
            yval = 0.9 * max(mean)) %>%
  ungroup() %>%
  right_join(predictors, by = c("var" = "term")) %>%
  mutate(var = factor(var, levels = names(lookup_vars)))

ggplot(me_all2, aes(x = value)) + 
  geom_line(aes(y = estimate__, group = iteration), color = "cornflowerblue", size=.5, alpha = 0.4) +
  geom_line(data = me_all2_avg, aes(x = value, y = mean)) +
  geom_text(data=predictors, aes(label = lab, x = Inf, y = Inf),
            hjust = 2, vjust = 1.5, size = 14) +
  facet_wrap(~var,  labeller = global_labeller, scales = "free", ncol = 2) +
  labs(x = "", y = "Additive Change in AMR Emergence Event Count\n", title = "") +
  theme_minimal() +
  theme(axis.title.y = element_text(hjust = 1, size = 14),
        strip.text = element_text(size = 14),
        axis.text = element_text(size = 11))

ggsave(filename = h("plots/marginal_effects_multi.png"), width = 12, height = 20)

# Zi logistic model ---------------------------------------------------------------

# Get Zi partial effects
out_zi <- map_dfr(zi_vars, function(var){
  
  minx <- amr_with_imputes %>% pull(var) %>% min(., na.rm = T)
  maxx <- amr_with_imputes %>% pull(var) %>% max(., na.rm = T)
  seqx <- seq(from = minx, to = maxx, length.out = 100)
  
  betas <- as.matrix(beta_samples[,grep("zi", colnames(beta_samples))])
  
  X <- matrix(
    c(1, colMeans(as.matrix(amr_with_imputes[,zi_vars]))),
    nrow = length(zi_vars) + 1, ncol = length(seqx),
  )
  rownames(X) <- c("Intercept", zi_vars)
  
  assertthat::assert_that(all(rownames(X) ==  gsub("^b_zi_", "", colnames(betas))))
  
  X[var, ] <- seqx
  Y <- plogis(betas %*% X)
  out <- as_tibble(Y) %>% 
    mutate(samp = 1:nrow(Y)) %>% 
    gather("x", "y", -samp) %>% 
    mutate(x = rep(seqx, each = nrow(betas)),
           var = var)
  
  return(out)
})

# reverse axis
out_zi <- out_zi %>% mutate(y = 1-y)

# labels for x axis
out_zi <- out_zi %>% 
  mutate(x_trans = ifelse(grepl("ln_", var), exp(x), x))

# summarize
out_zi_sum <- out_zi %>% 
  group_by(x, x_trans, var) %>% 
  summarise(med = median(y),
            lo = quantile(y, .025),
            hi = quantile(y, .975)) %>%
  ungroup()

p_zi_lines <- 
  ggplot() + 
  geom_line(data = out_zi, aes(x = x, y = y, group = samp), color = "gray60", alpha = 0.1) + #For lots of lines
  geom_line(data = out_zi_sum, aes(x = x, y = med), color = "cornflowerblue", size = 1.5) +
  geom_line(data = out_zi_sum, aes(x = x, y = lo), color = "cornflowerblue", size = 0.5, alpha = 0.8) +
  geom_line(data = out_zi_sum, aes(x = x, y = hi), color = "cornflowerblue", size = 0.5, alpha = 0.8) +
  geom_rug(data = filter(amr_raw, var %in% zi_vars), mapping = aes(x = x)) +
  facet_wrap(var ~ ., scales = "free_x", drop = FALSE,  ncol = 2, labeller = global_labeller) +
  labs(y = "Logisitic Prob. of Non-zero Outcome\n", x = "", main = "") +
  theme_few() +
  theme(strip.text.x = element_text(size = 14), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

ggsave(plot = p_zi_lines, 
       filename = h("plots/zi_partial_effects.png"), width = 12.5, height = 12)

# Poisson model ---------------------------------------------------------------

out_pois <- map_dfr(pois_vars, function(var){
  
  minx <- amr_with_imputes %>% pull(var) %>% min(., na.rm = T)
  maxx <- amr_with_imputes %>% pull(var) %>% max(., na.rm = T)
  seqx <- seq(from = minx, to = maxx, length.out = 100)
  
  betas <- cbind(as.matrix(beta_samples[,c(grep("b_[^z]", colnames(beta_samples)))]), 1)
  colnames(betas)[ncol(betas)] <- "offset"
  
  X <- matrix(
    c(1, colMeans(as.matrix(amr_with_imputes[,c(pois_vars, "ln_population")]))),
    nrow = length(pois_vars) + 2, ncol = length(seqx),
  )
  rownames(X) <- c("Intercept", pois_vars, "offset") 
  
  assertthat::assert_that(all(rownames(X) ==  gsub("^b_", "", colnames(betas))))
  
  X[var, ] <- seqx
  Y <- exp(betas %*% X)
  out <- as_tibble(Y) %>% 
    mutate(samp = 1:nrow(Y)) %>% 
    gather("x", "y", -samp) %>% 
    mutate(x = rep(seqx, each = nrow(betas)),
           var = var)
  
  return(out)
})

out_pois_sum <- out_pois %>% 
  group_by(x, var) %>% 
  summarise(med = median(y),
            lo = quantile(y, .025),
            hi = quantile(y, .975))

p_pois_lines <-  
  ggplot() + 
  geom_line(data = out_pois, aes(x = x, y = y, group = samp), color = "gray60", alpha = 0.1) + #For lots of lines
  geom_line(data = out_pois_sum, aes(x = x, y = med), color = "cornflowerblue", size = 1.5) +
  geom_line(data = out_pois_sum, aes(x = x, y = lo), color = "cornflowerblue", size = 0.5, alpha = 0.8) +
  geom_line(data = out_pois_sum, aes(x = x, y = hi), color = "cornflowerblue", size = 0.5, alpha = 0.8) +
  geom_rug(data = filter(amr_raw, var %in% pois_vars), mapping = aes(x = x, y = 0)) + 
  facet_wrap(var ~ ., scales = "free", drop = FALSE,  nrow = 3, labeller = global_labeller) +
  labs(y = "AMR Emergence Event Count\n", x = "", main = "Poisson Model (zi)") +
  theme_few() +
  theme(strip.text.x = element_text(size = 14), 
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14))

ggsave(plot = p_pois_lines,
       filename = h("plots/pois_partial_effects.png"), width = 25, height = 12)


# Generate predictions --------------------------------------------
amr_predict <- amr_with_imputes %>%
  mutate(mod_predict = predict(fit_combined)[,1]) %>%
  select(iso3c, mod_predict, n_amr_events)

# get means
# amr_means <- colMeans(as.matrix(amr_with_imputes[,unique(c(zi_vars, pois_vars))])) %>%
#   enframe() %>%
#   pivot_wider(names_from = name, values_from = value)
# predict(fit_combined, newdata = amr_means) 
# amr_means$ln_gdp_per_capita <- log(exp(amr_means$ln_gdp_per_capita) * 2)
# predict(fit_combined, newdata = amr_means) 

# matrix of beta samples
betas <- cbind(as.matrix(beta_samples[,c(grep("b_[^z]", colnames(beta_samples)))]), 1)
colnames(betas)[ncol(betas)] <- "b_ln_population"

# matrix of data by country
X <- cbind("Intercept" = 1 , amr_with_imputes[,c(pois_vars, "ln_population")]) %>%
  as.matrix(.) %>%
  t()
colnames(X) <- amr_with_imputes$iso3c
X2 <- X
X2["ln_population",] <- 0 # set population to zero to calculate rate

# calc poisson predictions for each country for each beta sample
assertthat::assert_that(all(rownames(X) ==  gsub("^b_", "", colnames(betas))))
Y <- exp(betas %*% X)
Y2 <- exp(betas %*% X2)

# post process
pois_predicts <- as_tibble(Y) %>% 
  mutate(samp = 1:nrow(Y)) %>% 
  gather("iso3c", "pois_predict", -samp) %>%
  mutate(v = "mean_pop")

Y2 <- as_tibble(Y2) %>% 
  mutate(samp = 1:nrow(Y2)) %>% 
  gather("iso3c", "pois_predict", -samp) %>%
  mutate(v = "zero_pop")

pois_predicts <- bind_rows(pois_predicts, Y2) %>%
  group_by(iso3c, v) %>%
  summarise(med = median(pois_predict),
            lo = quantile(pois_predict, .025),
            hi = quantile(pois_predict, .975)) %>%
  ungroup() %>%
  left_join(amr_predict) %>% # would need full model equation for distributions of predictions 
  mutate(continent = countrycode(sourcevar = iso3c,
                                 origin = "iso3c",
                                 destination = "continent"),
         country = countrycode(sourcevar = iso3c,
                               origin = "iso3c",
                               destination = "country.name"))

# view largest differences
test = pois_predicts %>%
  filter(v == "mean_pop") %>%
  mutate(diff = med - n_amr_events) %>%
  arrange(-diff) %>%

mean(test$diff)

# Map predictions ---------------------------------------------------------

# Get adm
admin <- ne_countries(type='countries', scale = 'large') %>%
  st_as_sf() %>%
  mutate(iso3c = countrycode(sourcevar = name,
                             origin = "country.name",
                             destination = "iso3c"),
         iso3c = ifelse(is.na(iso3c), iso_a3_eh, iso3c)) %>%
  select(name, iso3c)

admin <- left_join(admin, pois_predicts, by = "iso3c") %>%
  mutate(med = ifelse(v=="mean_pop", round(med, 0), med*10000)) %>%
  select(name, iso3c, v, "Reported AMR Events" = n_amr_events, "Predicted AMR Events" = med) 

admin_mean <- admin %>%
  filter(is.na(v) | v == "mean_pop") %>% 
  select(-v) %>%
  gather(-name, -iso3c, -geometry, key = "key", value = "value") %>%
  mutate(key = factor(key, levels = c("Reported AMR Events", "Predicted AMR Events"), labels = c("Reported", "Predicted"))) %>%
  filter(name != "Antarctica")

pal1 <- colorNumeric("OrRd", domain = c(admin_mean$`Predicted AMR Events`, admin_mean$`Reported AMR Events`), na.color = "#e9e9f0")

caption <- glue::glue(nrow(events), " AMR emergence events<br/>",
                      n_distinct(events$study_country), " countries<br/>",
                      n_distinct(events$drug), " antimicrobial drugs<br/>",
                      n_distinct(events$bacteria), " resistant bacteria<br/>",
                      str_sub(min(events$start_date), 1, 4), " - ",  str_sub(max(events$start_date), 1, 4))

ggplot(admin_mean) + 
  geom_sf(aes(fill = value), color = "transparent") +
  facet_wrap(key~., strip.position="top", ncol = 1) +
  scale_fill_viridis_c(option = "plasma", alpha = 0.8) +
  #scale_fill_gradient(high = "#E34A33", low = "#b0a390")  +
  labs(fill = "AMR Emergence Count") +
  theme_map() +
  theme(strip.background = element_blank(), strip.text = element_text(size = 14), 
        legend.title = element_text(size = 14), legend.text = element_text(size = 11))

ggsave(filename = h("plots/map_predictions.png"), width = 12, height = 8)

admin_mean2 <- admin %>%
  filter(v == "mean_pop")

lf1 <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  addFullscreenControl(position = "topright") %>%
  addPolygons(data = admin_mean2, 
              stroke = TRUE, color = "#46464a", weight = 1,
              fill = TRUE, fillColor = ~pal1(`Predicted AMR Events`), fillOpacity = 0.9,
              label = ~paste0(name, ": ", `Predicted AMR Events`), group = "Predicted") %>%
  addPolygons(data = admin_mean2, 
              stroke = TRUE, color = "#46464a", weight = 1,
              fill = TRUE, fillColor = ~pal1(`Reported AMR Events`), fillOpacity = 0.9,
              label = ~paste0(name, ": ", `Reported AMR Events`), group = "Reported") %>%
  addCircleMarkers(data = locs,  radius = 3,
                   lng = ~jitter(lon_study), lat = ~jitter(lat_study), 
                   stroke = TRUE, color = "#210106", opacity = 1, weight = 1,
                   fill = TRUE, fillColor = "#210106", fillOpacity = 0.5,
                   label = ~study_location, group = "Reported") %>%
  addLegend(data = admin_mean2, pal = pal1, values = ~`Predicted AMR Events`, position = "bottomright", title = "AMR Emergence Events") %>% 
  addControl(caption) %>%
  addLayersControl(baseGroups = c("Reported", "Predicted"), options = layersControlOptions(collapsed = FALSE), position = "bottomleft")  

lf1

htmlwidgets::saveWidget(lf1, h("plots/map_predictions_interactive.html"))
