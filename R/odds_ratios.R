get_coefficients <- function(mod_comb){
  sjPlot::get_model_data(mod_comb, type = "est") %>%
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
    select(term, lab)
} 

plot_coefficients <- function(coefs){
  
  ggplot(coefs, aes(x = term_clean, y = estimate)) + 
    geom_hline(yintercept = 1, color = "gray60") +
    geom_segment(aes(y = conf.low, yend = conf.high, xend = term_clean), color = "cornflowerblue") +
    geom_point(aes(color = group), show.legend = FALSE) +
    geom_text(aes(label = lab), nudge_x = 0.25) +
    scale_y_continuous(limits = c(0.25, 2)) +
    scale_color_manual(values = c("neg" = "cornflowerblue", "pos" = "cornflowerblue")) +
    labs(x = "", y = "Odds Ratio", title = "Model Coefficients") +
    coord_flip() +
    theme_foundation(base_size = 10, base_family =  "sans") + 
    theme(rect = element_rect(fill = "white", linetype = 0, colour = NA),
          title = element_text(size = rel(1), face = "bold"), 
          axis.text = element_text( size = rel(1)), 
          axis.ticks = element_blank(),
          axis.line = element_blank(), 
          plot.title.position = "plot",
          panel.grid.major = element_line(colour = "gray50", linetype = 3), 
          panel.grid.minor = element_blank())
}
