
data_avail_summary <- function(data){
  data %>% 
    select_if(~is.numeric(.)) %>% 
    gather() %>% 
    drop_na(value) %>% 
    group_by(key) %>% 
    count() %>% 
    ungroup()
}