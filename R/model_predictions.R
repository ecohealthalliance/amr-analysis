get_predictions <- function(data_mice_compl, betas, pois_vars){
  
  # add interaction to data_mice_compl
  data_mice_compl <- data_mice_compl %>% 
    mutate("ln_livestock_consumption_kg_per_capita:ln_gdp_per_capita" = ln_livestock_consumption_kg_per_capita * ln_gdp_per_capita)
  
  # matrix of beta samples
  betas <- cbind(as.matrix(betas[,c(grep("b_[^z]", colnames(betas)))]), 1)
  colnames(betas)[ncol(betas)] <- "b_ln_population"
  
  # matrix of data by country
  X <- cbind("Intercept" = 1 , data_mice_compl[,c(pois_vars, "ln_population")]) %>%
    as.matrix(.) %>%
    t()
  colnames(X) <- data_mice_compl$iso3c
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
    mutate(continent = countrycode::countrycode(sourcevar = iso3c,
                                                origin = "iso3c",
                                                destination = "continent"),
           country = countrycode::countrycode(sourcevar = iso3c,
                                              origin = "iso3c",
                                              destination = "country.name"))
  
  amr_events <- data_mice_compl %>% 
    dplyr::select(iso3c, n_amr_events) 
  
  pois_predicts <- left_join(pois_predicts, amr_events, by = "iso3c")
    
  return(pois_predicts)
  
}