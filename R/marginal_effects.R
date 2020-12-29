
plot_marginal_effects <- function(marg_eff, lookup_vars, consistent_preds, data_reshape){
  
  marg_effects_data <- imap_dfr(marg_eff, function(me, iter){
    imap_dfr(me, function(x, y){
      x %>% 
        select(effect1__, suppressWarnings(one_of("effect2__")), cond__:upper__) %>%
        mutate(var = y)
    }) %>% 
      mutate(iteration = iter) 
  }) %>%
    separate(var, into = c("var1", "var2"), sep = ":", remove = FALSE) %>% 
    mutate(effect1_backtrans = ifelse(str_detect(var1, "ln_"), exp(effect1__), effect1__)) %>%
    mutate(effect2_backtrans = ifelse(str_detect(var2, "ln_"), signif(exp(as.numeric(as.character(effect2__))), 1), effect2__)) %>%
    mutate(effect2_backtrans = replace_na(as.character(effect2_backtrans), "")) %>% 
    mutate(effect2_backtrans = factor(effect2_backtrans, levels = c("", sort(as.numeric(unique(effect2_backtrans)))))) %>% 
    mutate(effect2_backtrans = fct_rev(effect2_backtrans)) %>% 
    mutate_at(.vars = c("var1", "var2"),  ~factor(., levels = names(lookup_vars)))  
  
  marg_effects_avg <- marg_effects_data %>%
    group_by(effect1_backtrans, effect2_backtrans, var, var1, var2) %>%
    summarize(mean = mean(estimate__)) %>%
    ungroup() 
  
  predictors <- marg_effects_avg %>%
    group_by(var) %>%
    summarize(xval = 0.9 * max(effect1_backtrans),
              yval = 0.9 * max(mean)) %>%
    ungroup() %>%
    right_join(consistent_preds, by = c("var" = "term")) %>%
    mutate(var = factor(var, levels = names(lookup_vars)))
  
  me_plots <- map(names(lookup_vars), function(lv){
    dat <- filter(marg_effects_data, var == lv)
    if(nrow(dat) == 0) return(NULL)
    p <- ggplot(data = dat, aes(x = effect1_backtrans)) + 
      geom_line(aes(y = estimate__, group = interaction(iteration, effect2_backtrans), color = effect2_backtrans), size=.5, alpha = 0.2) +
      geom_line(data = filter(marg_effects_avg, var == lv), 
                aes(x = effect1_backtrans, y = mean, group = effect2_backtrans, color = effect2_backtrans), size = 1.5) +
      geom_rug(data = filter(data_reshape, var ==lv), mapping = aes(x = x_backtrans)) +
      # scale_y_continuous(limits = c(0, 100),
      #                    breaks = c(0, 25, 50, 75, 100),
      #                    labels = c("0", "", "50", "", "100")) +
      scale_color_viridis_d() +
      labs(title = lookup_vars[lv], y = "", x="", color = lookup_vars[unique(dat$var2)]) +
      theme_foundation(base_size = 12, base_family =  "sans") + 
      theme(rect = element_rect(fill = "white", linetype = 0, colour = NA),
            title = element_text(size = rel(1.1)), 
            axis.text = element_text( size = rel(1)), 
            axis.ticks = element_blank(),
            axis.line = element_blank(), 
            panel.grid.major = element_line(colour = "gray50", linetype = 3), 
            panel.grid.minor = element_blank()
      )
    if(is.na(unique(dat$var2))){
      p <- p + theme(legend.position = "none")
    }else{
      p <- p + theme(legend.position = "top")
    }
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
  
  cowplot::plot_grid(plotlist = purrr::compact(me_plots), 
                     ncol = 2) 
}
