
plot_marginal_effects <- function(marg_eff, lookup_vars, consistent_preds, data_reshape, variables = NULL, ncol = 2){
  marg_effects_data <- imap_dfr(marg_eff, function(me, iter){
    if(!is.null(variables)) me <- me[variables]
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
  
  me_plots <- map(unique(marg_effects_data$var), function(lv){
    dat <- filter(marg_effects_data, var == lv)
    if(nrow(dat) == 0) return(NULL)
    p <- ggplot(data = dat, aes(x = effect1_backtrans)) + 
      geom_line(aes(y = estimate__, 
                    group = interaction(iteration, effect2_backtrans)),
                color = "gray60", size=.5, alpha = 0.4) +
      geom_line(data = filter(marg_effects_avg, var == lv), 
                aes(x = effect1_backtrans, y = mean, group = effect2_backtrans), color = "gray20", size = 1.5) +
      geom_rug(data = filter(data_reshape, var == lv), mapping = aes(x = x_backtrans)) +
      # scale_y_continuous(limits = c(0, 100),
      #                    breaks = c(0, 25, 50, 75, 100),
      #                    labels = c("0", "", "50", "", "100")) +
      #scale_color_viridis_d() +
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
      
      # get three levels of interaction
      gdp_levels <- dat %>% 
        filter(var==lv) %>% 
        distinct(effect2__, effect2_backtrans) %>% 
        mutate(effect2__= as.numeric(as.character(effect2__))) %>% 
        pull(effect2__) %>% 
        sort()
      
      # split raw values by gdp levels
      interaction_rug <- data_reshape %>% 
        filter(var=="ln_livestock_consumption_kg_per_capita") %>% 
        mutate(effect2__ = if_else(interaction_ln_gdp_per_capita < gdp_levels[1],
                           gdp_levels[1],
                           if_else(interaction_ln_gdp_per_capita >= gdp_levels[3],
                                   gdp_levels[3], 
                                   gdp_levels[2]))) %>% 
        mutate(effect2_backtrans = signif(exp(as.numeric(effect2__)), 1)) %>% 
        mutate(effect2_backtrans = factor(effect2_backtrans, levels =  sort(as.numeric(unique(effect2_backtrans))))) %>% 
        mutate(effect2_backtrans = fct_rev(effect2_backtrans)) 
      
      # get max livestock value by gdp group
      x_limits <- interaction_rug %>% 
        group_by(effect2_backtrans) %>% 
        filter(x_backtrans == max(x_backtrans)) %>% 
        ungroup() %>% 
        select(effect2_backtrans, max_x_backtrans = x_backtrans)
      
      # truncate curves to only show range of raw x values
      dat_interaction <- dat %>% 
        filter(var==lv) %>% 
        left_join(x_limits) %>% 
        mutate(effect1_backtrans = ifelse(effect1_backtrans > max_x_backtrans, NA, effect1_backtrans)) %>% 
        drop_na(effect1_backtrans)
      
      out_pois_sum_interaction <- marg_effects_avg %>% 
        filter(var==lv) %>% 
        left_join(x_limits) %>% 
        mutate(effect1_backtrans = ifelse(effect1_backtrans > max_x_backtrans, NA, effect1_backtrans)) %>% 
        drop_na(effect1_backtrans)
      
      p <- ggplot(data = dat_interaction, aes(x = effect1_backtrans)) + 
        geom_line(aes(y = estimate__, 
                      group = interaction(iteration, effect2_backtrans),
                      color = effect2_backtrans), size=.5, alpha = 0.2) +
        geom_line(data = out_pois_sum_interaction, 
                  aes(x = effect1_backtrans, y = mean, group = effect2_backtrans, color = effect2_backtrans),
                  size = 1.5) +
        geom_rug(data = interaction_rug, mapping = aes(x = x_backtrans, color = effect2_backtrans)) +
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
              panel.grid.minor = element_blank(),
              legend.position = "top")
    }
    if(str_detect(lv, "ln_")){
      p <- p + scale_x_log10()
    }
    if(lv %in% c("ln_pubcrawl_per_capita", "ln_gdp_per_capita")){
      p <- p + scale_x_log10(labels = function(x) format(x, scientific = TRUE))
    }
    if(lv %in% predictors$var){
      p <- p +
        geom_text(data = predictors %>% filter(var == lv), aes(label = lab, x = Inf, y = Inf),
                  hjust = 2, vjust = 1.5, size = 14) 
    }
    return(p)
    
  })
  
  me_plots <- purrr::compact(me_plots)
  cowplot::plot_grid(plotlist = me_plots, 
                     ncol = ncol) 
}
