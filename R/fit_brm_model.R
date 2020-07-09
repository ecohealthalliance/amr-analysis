fit_brm_model <- function(data_mice, seed, formula) {
  brm_multiple(formula,
               data = data_mice,
               family = zero_inflated_poisson(),
               set_prior("student_t(3,0,10)", class = "b"),
               chains = 4,
               inits = "0", 
               iter = 2000,
               control = list(adapt_delta = 0.9),
               cores = 4,
               combine = FALSE,
               seed = seed)
}
