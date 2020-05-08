
plot_trace <- function(brm_model_combined){
  params <- mcmc_trace_data(brm_model_combined)$parameter %>% as.character() %>% unique()
  params_abbr <- str_remove_all(params, "_per_capita|_perc|_kg|_ddd")
  names(params_abbr) <- params
  global_labeller <- labeller(
    parameter = params_abbr
  )
  trace_plot <- mcmc_trace(brm_model_combined, facet_args = list(labeller = global_labeller)) + facet_text(size = 8) 
  cowplot::plot_grid(plotlist = list(trace_plot), labels = c("A"))
}

plot_posteriors <- function(y, yrep){
  
  # Generate posterior predictions
  yord <- order(y)
  yrep <- yrep[,yord]
  y <- y[yord]
  
  # Density overlay plots
  dens_plot <- ppc_dens_overlay(y, yrep) + 
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(0, 50))
  
  # Proportion zero plots
  prop_zero <- function(x) mean(x == 0)
  zero_plot <- ppc_stat(y, yrep, stat = "prop_zero", binwidth = 0.005)
  
  # Interval plots
  interval_plot <- ppc_intervals(y, yrep)# + labs(title = "Observations versus Predictions (individual observations)", caption = paste0("dark line = 50% probability\nfaded line = 90% probability\n", p50, "% non-zeros in 50% prob\n", p90,  "% non-zeros in 90% prob"))
  
  # all plots
  all_plots <- list(dens_plot, interval_plot, zero_plot)
  cowplot::plot_grid(plotlist=all_plots, labels = c("B", "C", "D"), nrow = 1)
}