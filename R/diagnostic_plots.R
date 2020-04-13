
make_trace_plots <- function(brm_model_combined){
  params <- mcmc_trace_data(brm_model_combined)$parameter %>% as.character() %>% unique()
  params_abbr <- str_remove_all(params, "_per_capita|_perc|_kg|_ddd")
  names(params_abbr) <- params
  global_labeller <- labeller(
    parameter = params_abbr
  )
  trace_plot <- mcmc_trace(brm_model_combined, facet_args = list(labeller = global_labeller)) + facet_text(size = 8) 
  cowplot::plot_grid(plotlist = list(trace_plot), labels = c("A"))
}
