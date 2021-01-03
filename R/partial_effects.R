
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
  
  zi_vars_ordered <- intersect(names(lookup_vars), zi_vars)
  
  zi_plots <- map(zi_vars_ordered, function(zv){
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
    
    if(str_detect(zv, "ln_")){
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
    
    var1 <- str_split(var, ":")[[1]][[1]]
    
    minx <- data_mice_compl %>% pull(var1) %>% min(., na.rm = T)
    maxx <- data_mice_compl %>% pull(var1) %>% max(., na.rm = T)
    seqx <- seq(from = minx, to = maxx, length.out = 100)
    
    betas <- cbind(as.matrix(betas[,c(grep("b_[^z]", colnames(betas)))]), 1)
    colnames(betas)[ncol(betas)] <- "offset"
    
    X <- matrix(
      c(1, colMeans(as.matrix(data_mice_compl[,c(pois_vars, "ln_population")]))),
      nrow = length(pois_vars) + 2, ncol = length(seqx),
    )
    rownames(X) <- c("Intercept", pois_vars, "offset") 
    
    assertthat::assert_that(all(rownames(X) ==  gsub("^b_", "", colnames(betas))))
    
    if(var == "ln_livestock_consumption_kg_per_capita:ln_gdp_per_capita"){
      var2 <- str_split(var, ":")[[1]][[2]]
      distz <- data_mice_compl %>% pull(var2)
      z25 <- quantile(distz, 0.25)
      z50 <- quantile(distz, 0.5)
      z75 <- quantile(distz, 0.75)
      
      X <- cbind(cbind(X, X), X)
      X[var1, ] <- rep(seqx, 3) # replace with seqx for livestock, and 3 values for gdp, then multiple together and exponentiate
      X[var2, ] <- rep(c(z25, z50, z75), each = 100)
      X[var,] <-  X[var1, ] *  X[var2, ]
      multiplier <- 3
    }else{
      X[var, ] <- seqx
      multiplier <- 1
    }
    
    Y <- exp(betas %*% X)
    # rownames(Y) <- paste0("samp", 1:nrow(Y))
    
    if(var == "ln_livestock_consumption_kg_per_capita:ln_gdp_per_capita"){
      colnames(Y) <- X[var2, ]
    }else{
      colnames(Y) <- rep(1, 100)
    }
    
    out <- as_tibble(Y, .name_repair = "minimal") %>% 
      set_names(paste(colnames(.), 1:ncol(.),  sep = "_")) %>% 
      mutate(samp = 1:nrow(Y)) %>% 
      gather("z", "y", -samp) %>% 
      mutate(x = rep(rep(seqx, multiplier), each = nrow(betas)),
             z = str_extract(z, "[^_]+"), 
             var = var)
    
    return(out)
  })
  
  # labels for x axis
  out_pois <- out_pois %>% 
    mutate(x_backtrans = ifelse(grepl("ln_", var), exp(x), x)) %>% 
    mutate(z = signif(exp(as.numeric(z)), 1)) %>% 
    mutate(z = factor(z, levels =  sort(as.numeric(unique(z))))) %>% 
    mutate(z = fct_rev(z)) 
  
  # summarize over samples
  out_pois_sum <- out_pois %>% 
    group_by(z, x, x_backtrans, var) %>% 
    summarise(med = median(y),
              lo = quantile(y, .025),
              hi = quantile(y, .975)) %>% 
    ungroup()
  
  pois_vars_ordered <- intersect(names(lookup_vars), pois_vars)
  
  pois_plots <- map(pois_vars_ordered, function(pv){
    p <-   ggplot() + 
      geom_line(data = filter(out_pois, var==pv), aes(x = x_backtrans, y = y, group = samp), color = "gray60", size=.5, alpha = 0.2) + #For lots of lines
      geom_line(data = filter(out_pois_sum, var==pv), aes(x = x_backtrans, y = med), size = 1) +
      geom_line(data = filter(out_pois_sum, var==pv), aes(x = x_backtrans, y = lo),  size = 0.5, alpha = 0.8) +
      geom_line(data = filter(out_pois_sum, var==pv), aes(x = x_backtrans, y = hi),  size = 0.5, alpha = 0.8) +
      geom_rug(data = filter(data_reshape, var==pv), mapping = aes(x = x_backtrans)) +
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
    
    if(pv == "ln_livestock_consumption_kg_per_capita:ln_gdp_per_capita"){
      p <- ggplot() + 
        geom_line(data = filter(out_pois, var==pv), aes(x = x_backtrans, y = y, group = interaction(samp, z), color = z), size=.5, alpha = 0.1) + #For lots of lines
        geom_line(data = filter(out_pois_sum, var==pv), aes(x = x_backtrans, y = med, group = z, color = z), size = 1.5) +
        geom_rug(data = filter(data_reshape, var==pv), mapping = aes(x = x_backtrans)) +
        scale_color_viridis_d() +
        labs(y = "", x = "", title = lookup_vars[pv], color = lookup_vars["ln_gdp_per_capita"]) +
        theme_foundation(base_size = 12, base_family =  "sans") + 
        theme(rect = element_rect(fill = "white", linetype = 0, colour = NA),
              title = element_text(size = rel(1)), 
              axis.text = element_text( size = rel(1)), 
              axis.ticks = element_blank(),
              axis.line = element_blank(), 
              panel.grid.major = element_line(colour = "gray50", linetype = 3), 
              panel.grid.minor = element_blank(),
              legend.position = "top"
        )
    }
    
    if(str_detect(pv, "ln_")){
      p <- p + scale_x_log10()
    }
    if(pv %in% c("ln_pubcrawl_per_capita", "ln_gdp_per_capita")){
      p <- p + scale_x_log10(labels = function(x) format(x, scientific = TRUE))
    }
    if(pv ==  "ln_livestock_consumption_kg_per_capita"){
      p <- p + scale_y_log10()
    }
    
    return(p)
  })
  
  cowplot::plot_grid(plotlist=pois_plots, 
                     ncol = 3) 
}
