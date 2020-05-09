get_zi_vars <-  function(beta_samples) {
  beta_samples %>%
    select(matches("zi_"), -b_zi_Intercept) %>%
    colnames(.) %>%
    gsub("^b_zi_", "", .)
}

get_pois_vars <- function(beta_samples){
  beta_samples %>%
    select(-matches("zi_"), -b_Intercept, -lp__) %>%
    colnames(.)%>%
    gsub("^b_", "", .)
}
