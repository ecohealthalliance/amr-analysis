plot_slope <- function(model_predictions){
  
  # view largest differences between predicted and acctual
  diffs <- model_predictions %>%
    filter(v == "mean_pop") %>%
    mutate(diff = med - n_amr_events) %>%
    mutate(country_lab = paste0(country, " (", round(lo, 0), " - ", round(hi,0), ")")) %>%
    arrange(-abs(diff))
  
  # mean(diffs$diff)
  # diffs %>% filter(diff > 0) %>% nrow()
  
  diffs_reshape <- diffs %>%
    select(country_lab, n_amr_events, med, diff) %>%
    mutate(increase = diff > 0) %>%
    mutate(top_10 = row_number() <= 10) %>%
    gather(-country_lab, -increase, -top_10, -diff, key = "key", value = "value") %>%
    mutate(key = factor(key, levels = c("n_amr_events", "med"), labels = c("Reported", "Predicted"))) 
  
  diffs_reshape_top10 <- diffs_reshape %>% filter(top_10==TRUE)
  
  # slope graph
  ggplot() +
    geom_line(data = diffs_reshape, aes(x = key, y = value, group = country_lab), color = "gray40", alpha = 0.2) +
    geom_point(data = diffs_reshape, aes(x = key, y = value, group = country_lab), color = "gray40", alpha = 0.2) +
    geom_line(data = diffs_reshape_top10, aes(x = key, y = value, group = country_lab)) +
    geom_point(data = diffs_reshape_top10, aes(x = key, y = value, group = country_lab)) +
    geom_text(data = filter(diffs_reshape_top10, key == "Predicted", ), aes(label = country_lab, x = key, y = value),
              hjust = "outward", nudge_x = 0.02) +
    scale_x_discrete(expand = c(0.1, 0,0 ,0.5))+
    # scale_color_manual(values = c(`TRUE` = "hotpink",
    #                               `FALSE` = "blue")) +
    labs(x = "", y = "", title = "")  +
    theme_foundation(base_size = 12, base_family =  "sans") + 
    theme(rect = element_rect(fill = "white", linetype = 0, colour = NA),
          title = element_text(size = rel(1), face = "bold"), 
          axis.text = element_text( size = rel(1)), 
          axis.ticks = element_blank(),
          axis.line = element_blank(), 
          legend.position ="none",
          panel.grid.major =  element_blank(), 
          panel.grid.minor = element_blank())
}