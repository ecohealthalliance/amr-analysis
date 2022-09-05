get_zi_vars <-  function(betas) {
  betas %>%
    select(matches("zi_"), -b_zi_Intercept) %>%
    colnames(.) %>%
    gsub("^b_zi_", "", .)
}

get_pois_vars <- function(betas){
  betas %>%
    select(-matches("zi_"), -b_Intercept, -lp__, -lprior) %>%
    colnames(.)%>%
    gsub("^b_", "", .)
}
