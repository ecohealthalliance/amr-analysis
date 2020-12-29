get_me_dat <- function(me){
  imap_dfr(me, function(x, y){
    x %>% 
      select(value = y, cond__:upper__) %>%
      mutate(var = y)
  })
}

show_imputes <- function(mice, m, raw){
  
  imp <- mice$imp %>% compact()
  
  m <- as.character(m)
  
  imp2 <- imap_dfr(imp, function(x, y){
    x %>%
      as_tibble() %>%
      rownames_to_column(var = "country_id") %>%
      gather(key = "m", value = "value", `1`:m) %>%
      mutate(field = y)
  })
  
  raw_means <- raw %>%
    select_if(~any(is.na(.))) %>%
    gather(key = "field", value = "value") %>%
    group_by(field) %>%
    summarize(mean = mean(value, na.rm = T), min = min(value, na.rm = T), max = max(value, na.rm = T))
  
  ggplot(data = imp2, aes(x = country_id,  y = value, color = m)) +
    geom_point() +
    geom_hline(data = raw_means, aes(yintercept = mean)) +
    facet_wrap(field ~., scales = "free")
}
