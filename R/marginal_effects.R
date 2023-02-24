
plot_conditional_effects <- function(cond_eff, lookup_vars, consistent_preds, data_reshape, variables = NULL, ncol = 2){
  
  cond_effects_data <- imap_dfr(cond_eff, function(x, y){
    x %>%
      select(effect1__, suppressWarnings(one_of("effect2__")), estimate__:upper__) %>%
      mutate(var = y)
  })  
  
  if(!is.null(variables)) cond_effects_data <- cond_effects_data |> filter(var %in% variables)
  
  cond_effects_data <- cond_effects_data  %>%
    separate(var, into = c("var1", "var2"), sep = ":", remove = FALSE) %>%
    mutate(effect1_backtrans = ifelse(str_detect(var1, "ln_"), exp(effect1__), effect1__)) %>%
    mutate(effect2_backtrans = ifelse(str_detect(var2, "ln_"), signif(exp(as.numeric(as.character(effect2__))), 1), effect2__)) %>%
    mutate_at(.vars = c("var1", "var2"),  ~factor(., levels = names(lookup_vars)))
  
  me_plots <- map(unique(cond_effects_data$var), function(lv){
    dat <- filter(cond_effects_data, var == lv)
    observed <- filter(data_reshape, var == lv)
    if(nrow(dat) == 0) return(NULL)
    
    p <- ggplot(data = dat, aes(x = effect1__)) + 
      geom_line(aes(y = estimate__), color = "gray20", size = 1.5) +
      geom_ribbon(aes(ymin = lower__, ymax = upper__), alpha = 0.4) +
      geom_rug(data = filter(data_reshape, var == lv), mapping = aes(x = x)) +
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
    if(lv == "ln_livestock_consumption_kg_per_capita:ln_gdp_per_capita"){
      
      var1 <- lookup_vars[unique(dat$var1)]
      var2 <- lookup_vars[unique(dat$var2)]
      
      p <-  ggplot() + 
        geom_contour(data = dat, aes(x = effect1__, y = effect2__, z = estimate__, color = ..level..)) +
        stat_contour(data = dat, aes(x = effect1__, y = effect2__, z = estimate__, color = ..level..),  bins = 30) + 
        scale_color_viridis_c()   +
        geom_rug(data = data_reshape |> filter(var == names(var1)), aes(x = x)) +
        geom_rug(data = data_reshape |> filter(var == names(var2)), aes(y = x)) +
        labs(title = lookup_vars[lv], 
             y = var2, 
             x = var1, 
             color = "AMR Events") +
        theme_foundation(base_size = 12, base_family =  "sans") + 
        theme(rect = element_rect(fill = "white", linetype = 0, colour = NA),
              title = element_text(size = rel(1.1)), 
              axis.text = element_text( size = rel(1)), 
              axis.ticks = element_blank(),
              axis.line = element_blank(), 
              panel.grid.major = element_line(colour = "gray50", linetype = 3), 
              panel.grid.minor = element_blank(),
              legend.position = "right")
      
    }
    if(lv == "english_spoken" ){
      p <- p +
        scale_x_continuous(breaks = c(0, 1))
    }
    if(lv %in% consistent_preds$term){
      p <- p +
        geom_text(data = consistent_preds %>% filter(term == lv), aes(label = lab, x = Inf, y = Inf),
                  hjust = 2, vjust = 1.5, size = 14) 
    }
    if(length(variables) == 4){ #lazy way to check if we're formatting logistic plots
      p <- p + scale_y_continuous(limits = c(0,1))
    }
    return(p)
    
  })
  
  me_plots <- purrr::compact(me_plots)
  cowplot::plot_grid(plotlist = me_plots, 
                     ncol = ncol) 
}
