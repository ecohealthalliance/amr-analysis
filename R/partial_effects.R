
plot_zi_partial_effects <- function(betas, zi_vars, data_mice_compl, data_reshape){
  # Get Zi partial effects
  out_zi <- map_dfr(zi_vars, function(var){
    
    minx <- data_mice_compl %>% pull(var) %>% min(., na.rm = T)
    maxx <- data_mice_compl %>% pull(var) %>% max(., na.rm = T)
    seqx <- seq(from = minx, to = maxx, length.out = 100)
    
    betas <- as.matrix(betas[,grep("zi", colnames(betas))])
    
    X <- matrix(
      c(1, colMeans(as.matrix(data_mice_compl[,zi_vars]))),
      nrow = length(zi_vars) + 1, ncol = length(seqx),
    )
    rownames(X) <- c("Intercept", zi_vars)
    
    assertthat::assert_that(all(rownames(X) ==  gsub("^b_zi_", "", colnames(betas))))
    
    X[var, ] <- seqx
    Y <- plogis(betas %*% X)
    out <- as_tibble(Y, .name_repair = "universal") %>% 
      mutate(samp = 1:nrow(Y)) %>% 
      gather("x", "y", -samp) %>% 
      mutate(x = rep(seqx, each = nrow(betas)),
             var = var)
    
    return(out)
  })
  
  # reverse axis
  out_zi <- out_zi %>% mutate(y = 1-y)
  
  # labels for x axis
  out_zi <- out_zi %>% 
    mutate(x_backtrans = ifelse(grepl("ln_", var), exp(x), x))
  
  # summarize
  out_zi_sum <- out_zi %>% 
    group_by(x, x_backtrans, var) %>% 
    summarise(med = median(y),
              lo = quantile(y, .025),
              hi = quantile(y, .975)) %>%
    ungroup()
  
  zi_plots <- map(zi_vars, function(zv){
    p <-   ggplot() + 
      geom_line(data = filter(out_zi, var==zv), aes(x = x_backtrans, y = y, group = samp), color = "gray60", size=.5, alpha = 0.2) + #For lots of lines
      geom_line(data = filter(out_zi_sum, var==zv), aes(x = x_backtrans, y = med), size = 1) +
      geom_line(data = filter(out_zi_sum, var==zv), aes(x = x_backtrans, y = lo),  size = 0.5, alpha = 0.8) +
      geom_line(data = filter(out_zi_sum, var==zv), aes(x = x_backtrans, y = hi),  size = 0.5, alpha = 0.8) +
      geom_rug(data = filter(data_reshape, var==zv), mapping = aes(x = x_backtrans)) +
      scale_y_continuous(limits = c(0,1)) +
      labs(y = "", x = "", title = lookup_vars[zv]) +
      theme_foundation(base_size = 10, base_family =  "sans") + 
      theme(rect = element_rect(fill = "white", linetype = 0, colour = NA),
            title = element_text(size = rel(1)), 
            axis.text = element_text( size = rel(1)), 
            axis.ticks = element_blank(),
            axis.line = element_blank(), 
            panel.grid.major = element_line(colour = "gray50", linetype = 3), 
            panel.grid.minor = element_blank()
      )
    
    if(zv == "ln_population"){
      p <- p + scale_x_log10()
    }
    if(zv %in% c("ln_pubcrawl_per_capita", "ln_gdp_per_capita")){
      p <- p + scale_x_continuous(labels = function(x) format(x, scientific = TRUE))
    }
    return(p)
  })
  
  cowplot::plot_grid(plotlist=zi_plots, 
                     ncol = 3) 
}


plot_pois_partial_effects <- function(betas, pois_vars, data_mice_compl, data_reshape){
  
  # add interaction to data_mice_compl
  data_mice_compl <- data_mice_compl %>% 
    mutate("ln_livestock_consumption_kg_per_capita:ln_gdp_per_capita" = ln_livestock_consumption_kg_per_capita * ln_gdp_per_capita)
  
  # Get poisson partial effects
  out_pois <- map_dfr(pois_vars, function(var){
    
    minx <- data_mice_compl %>% pull(var) %>% min(., na.rm = T)
    maxx <- data_mice_compl %>% pull(var) %>% max(., na.rm = T)
    seqx <- seq(from = minx, to = maxx, length.out = 100)
    
    betas <- cbind(as.matrix(betas[,c(grep("b_[^z]", colnames(betas)))]), 1)
    colnames(betas)[ncol(betas)] <- "offset"
    
    X <- matrix(
      c(1, colMeans(as.matrix(data_mice_compl[,c(pois_vars, "ln_population")]))),
      nrow = length(pois_vars) + 2, ncol = length(seqx),
    )
    rownames(X) <- c("Intercept", pois_vars, "offset") 
    
    assertthat::assert_that(all(rownames(X) ==  gsub("^b_", "", colnames(betas))))
    
    X[var, ] <- seqx
    Y <- exp(betas %*% X)
    out <- as_tibble(Y, .name_repair = "universal") %>% 
      mutate(samp = 1:nrow(Y)) %>% 
      gather("x", "y", -samp) %>% 
      mutate(x = rep(seqx, each = nrow(betas)),
             var = var)
    
    return(out)
  })
  
  # labels for x axis
  out_pois <- out_pois %>% 
    mutate(x_backtrans = ifelse(grepl("ln_", var), exp(x), x))
  
  # summarize
  out_pois_sum <- out_pois %>% 
    group_by(x, x_backtrans, var) %>% 
    summarise(med = median(y),
              lo = quantile(y, .025),
              hi = quantile(y, .975))
  
  pois_plots <- map(pois_vars, function(pv){
    p <-   ggplot() + 
      geom_line(data = filter(out_pois, var==pv), aes(x = x_backtrans, y = y, group = samp), color = "gray60", size=.5, alpha = 0.2) + #For lots of lines
      geom_line(data = filter(out_pois_sum, var==pv), aes(x = x_backtrans, y = med), size = 1) +
      geom_line(data = filter(out_pois_sum, var==pv), aes(x = x_backtrans, y = lo),  size = 0.5, alpha = 0.8) +
      geom_line(data = filter(out_pois_sum, var==pv), aes(x = x_backtrans, y = hi),  size = 0.5, alpha = 0.8) +
      geom_rug(data = filter(data_reshape, var==pv), mapping = aes(x = x_backtrans)) +
      # scale_y_continuous(limits = c(0, 10), 
      #                    breaks = c(0, 2.5, 5, 7.5, 10), 
      #                    labels = c("0", "", "5", "", "10")) +
      labs(y = "", x = "", title = lookup_vars[pv]) +
      theme_foundation(base_size = 10, base_family =  "sans") + 
      theme(rect = element_rect(fill = "white", linetype = 0, colour = NA),
            title = element_text(size = rel(1)), 
            axis.text = element_text( size = rel(1)), 
            axis.ticks = element_blank(),
            axis.line = element_blank(), 
            panel.grid.major = element_line(colour = "gray50", linetype = 3), 
            panel.grid.minor = element_blank()
      )
    
    if(pv == "ln_population"){
      p <- p + scale_x_log10()
    }
    if(pv %in% c("ln_pubcrawl_per_capita", "ln_gdp_per_capita")){
      p <- p + scale_x_continuous(labels = function(x) format(x, scientific = TRUE))
    }
    return(p)
  })
  
  cowplot::plot_grid(plotlist=pois_plots, 
                     ncol = 3) 
}
