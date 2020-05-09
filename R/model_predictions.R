get_model_predictions <- function(imputed_data_complete, beta_samples, pois_vars){
  
  # matrix of beta samples
  betas <- cbind(as.matrix(beta_samples[,c(grep("b_[^z]", colnames(beta_samples)))]), 1)
  colnames(betas)[ncol(betas)] <- "b_ln_population"
  
  # matrix of data by country
  X <- cbind("Intercept" = 1 , imputed_data_complete[,c(pois_vars, "ln_population")]) %>%
    as.matrix(.) %>%
    t()
  colnames(X) <- imputed_data_complete$iso3c
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
  
  amr_events <- imputed_data_complete %>% 
    select(iso3c, n_amr_events) 
  
  pois_predicts <- left_join(pois_predicts, amr_events, by = "iso3c")
    
  return(pois_predicts)
  
}