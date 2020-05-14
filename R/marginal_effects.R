
plot_marginal_effects <- function(marg_eff, plot_labels, consistent_preds, data_reshape){
  
  marg_effects_data <- imap_dfr(marg_eff, function(me, iter){
    imap_dfr(me, function(x, y){
      x %>% 
        select(value = y, cond__:upper__) %>%
        mutate(var = y)
    }) %>% 
      mutate(iteration = iter) 
  }) %>%
    mutate(value_backtrans = ifelse(str_detect(var, "ln_"), exp(value), value)) %>%
    mutate(var = factor(var, levels = names(plot_labels))) 
  
  marg_effects_avg <- marg_effects_data %>%
    group_by(value_backtrans, var) %>%
    summarize(mean = mean(estimate__)) %>%
    ungroup() 
  
  predictors <- marg_effects_avg %>%
    group_by(var) %>%
    summarize(xval = 0.9 * max(value_backtrans),
              yval = 0.9 * max(mean)) %>%
    ungroup() %>%
    right_join(consistent_preds, by = c("var" = "term")) %>%
    mutate(var = factor(var, levels = names(plot_labels)))
  
  
  me_plots <- map(names(plot_labels), function(lv){
    p <- ggplot(data = filter(marg_effects_data, var == lv), aes(x = value_backtrans)) + 
      geom_line(aes(y = estimate__, group = iteration), color = "cornflowerblue", size=.5, alpha = 0.4) +
      geom_line(data = filter(marg_effects_avg, var == lv), aes(x = value_backtrans, y = mean), size = 1.25) +
      geom_rug(data = filter(data_reshape, var ==lv), mapping = aes(x = x_backtrans)) +
      scale_y_continuous(limits = c(0, 10), 
                         breaks = c(0, 2.5, 5, 7.5, 10), 
                         labels = c("0", "", "5", "", "10")) +
      labs(title = plot_labels[lv], y = "", x="") +
      theme_foundation(base_size = 12, base_family =  "sans") + 
      theme(rect = element_rect(fill = "white", linetype = 0, colour = NA),
            title = element_text(size = rel(1.1)), 
            axis.text = element_text( size = rel(1)), 
            axis.ticks = element_blank(),
            axis.line = element_blank(), 
            panel.grid.major = element_line(colour = "gray50", linetype = 3), 
            panel.grid.minor = element_blank()
      )
    if(lv == "ln_population"){
      p <- p + scale_x_log10()
    }
    if(lv %in% c("ln_pubcrawl_per_capita", "ln_gdp_per_capita")){
      p <- p + scale_x_continuous(labels = function(x) format(x, scientific = TRUE))
    }
    if(lv %in% predictors$var){
      p <- p +
        geom_text(data = predictors %>% filter(var == lv), aes(label = lab, x = Inf, y = Inf),
                  hjust = 2, vjust = 1.5, size = 14) 
    }
    return(p)
  })
  
  cowplot::plot_grid(plotlist=me_plots, 
                     ncol = 2) 
}
