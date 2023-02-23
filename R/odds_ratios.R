get_coefficients <- function(mod_comb){
  sjPlot::get_model_data(mod_comb, type = "est", ci.lvl = 0.95) %>% #by default, stanreg-models are printed with two intervals: the "inner" interval, which defaults to the 50%-CI; and the "outer" interval, which defaults to the 89%-C
    distinct() %>%
    mutate(term_clean = as.character(term)) %>%
    mutate(term_clean = lookup_vars[term_clean]) %>%
    mutate(term_clean = fct_reorder(term_clean, estimate)) %>%
    mutate(est = round(estimate, 2)) %>%
    mutate(predictor = !(1 >= conf.low & 1 <= conf.high)) %>%
    mutate(lab = ifelse(predictor, paste0(est, "*"), est))
}

get_consistent_predictors <- function(coefs){
  coefs %>%
    filter(predictor) %>%
    mutate(lab = "*") %>%
    select(term, lab, model = wrap.facet) %>% 
    mutate( term = str_replace(term, "\\.", ":")) |>
    mutate(model = case_when(model=="Conditional Model" ~ "pois", model=="Zero-Inflated Model" ~ "zi"))
} 

plot_coefficients <- function(coefs, fancy_lab){
  
  var_order <-  unname(lookup_vars) %>% unique()
  
  coefs <- coefs %>% 
    mutate(wrap.facet = factor(wrap.facet, levels = c("Zero-Inflated Model", "Conditional Model"))) %>% 
    mutate(term_clean = factor(term_clean, levels = var_order)) %>% 
    mutate(term_clean = fct_rev(term_clean))
  ggplot(coefs, aes(x = term_clean, y = estimate)) + 
    geom_hline(yintercept = 1, color = "gray60") +
    geom_segment(aes(y = conf.low, yend = conf.high, xend = term_clean), color = "cornflowerblue") +
    geom_point(aes(color = group), show.legend = FALSE) +
    geom_text(aes(label = lab), nudge_x = 0.25) +
    scale_y_log10() +
    # scale_y_continuous(limits = c(0.25, 2)) +
    scale_color_manual(values = c("neg" = "cornflowerblue", "pos" = "cornflowerblue")) +
    facet_wrap(wrap.facet ~ .) +
    labs(x = "", y = "Odds Ratio", title = paste0(fancy_lab)) +
    coord_flip() +
    theme_foundation(base_size = 10, base_family =  "sans") + 
    theme(rect = element_rect(fill = "white", linetype = 0, colour = NA),
          title = element_text(size = rel(1), face = "bold"), 
          axis.text = element_text(size = rel(1)), 
          axis.ticks = element_blank(),
          axis.line = element_blank(), 
          plot.title.position = "plot",
          panel.spacing = unit(2, "lines"),
          strip.text = element_text(size = rel(1.1)),
          plot.margin = margin(5.5, 18, 5.5, 5.5, "pt"),
          panel.grid.major = element_line(colour = "gray50", linetype = 3), 
          panel.grid.minor = element_blank())
}

export_coefficient_table <- function(coefs){
  coefs %>% 
    mutate(wrap.facet = factor(wrap.facet, levels = c( "Conditional Model", "Zero-Inflated Model"), 
                               labels = c("Conditional (Poisson)", "Zero-Inflated (Logistic)"),
                                 ordered = TRUE)) |> 
    mutate_at(vars("estimate", "conf.low", "conf.high"), ~signif(., 2)) |> 
    mutate(Estimate = paste0(estimate , " (", conf.low, "-", conf.high, ")", ifelse(predictor, "*", "")))  |> 
    arrange(wrap.facet, -estimate) |> 
    select(Model = wrap.facet, Predictor = term_clean, Estimate, group, predictor) 
    
}
