
plot_trace <- function(mod_comb){
  params <- mcmc_trace_data(mod_comb)$parameter %>% as.character() %>% unique()
  params_abbr <- str_remove_all(params, "_per_capita|_perc|_kg|_ddd")
  names(params_abbr) <- params
  global_labeller <- labeller(
    parameter = params_abbr
  )
  trace_plot <- mcmc_trace(mod_comb, facet_args = list(labeller = global_labeller)) + facet_text(size = 8) 
  cowplot::plot_grid(plotlist = list(trace_plot), labels = c("A"))
}


plot_posteriors <- function(data_trans, post_y){
  
  y <- data_trans$n_amr_events 
  
  # Generate posterior predictions
  yord <- order(y)
  post_y <- post_y[,yord]
  y <- y[yord]
  
  # Density overlay plots
  dens_plot <- ppc_dens_overlay(y, post_y) + 
    scale_y_continuous(limits = c(0, 1)) +
    scale_x_continuous(limits = c(0, 50)) +
    labs(x = "")
  
  # Proportion zero plots
  prop_zero <- function(x) mean(x == 0)
  zero_plot <- ppc_stat(y, post_y, stat = "prop_zero", binwidth = 0.005) + labs(x = "")
  
  # Interval plots
  interval_plot <- ppc_intervals(y, post_y) + labs(x = "")# + labs(title = "Observations versus Predictions (individual observations)", caption = paste0("dark line = 50% probability\nfaded line = 90% probability\n", p50, "% non-zeros in 50% prob\n", p90,  "% non-zeros in 90% prob"))
  
  # all plots
  all_plots <- list(dens_plot, interval_plot, zero_plot)
  cowplot::plot_grid(plotlist=all_plots, labels = c("A", "B", "C"), nrow = 1)
}
