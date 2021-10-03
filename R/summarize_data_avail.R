
summarize_data_avail <- function(data){
  data %>% 
    select_if(~is.numeric(.)) %>% 
    gather() %>% 
    drop_na(value) %>% 
    group_by(key) %>% 
    count() %>% 
    ungroup()
}