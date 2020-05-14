get_events <- function(){
  # event info
  url_events <- "https://raw.githubusercontent.com/ecohealthalliance/amr-db/master/events-db.csv"
  events <- GET(url_events, authenticate(Sys.getenv("GITHUB_USERNAME"), Sys.getenv("GITHUB_PAT")))
  read_csv(content(events, "text"))  %>%
    mutate(start_year = as.integer(substr(start_date, 1, 4))) %>%
    filter(start_year >= 2006) # removes promed mentions prior to 2006
}


get_locations <- function(events){
  url_locs <- "https://raw.githubusercontent.com/ecohealthalliance/amr-db/master/data-processed/locations.csv"
  locs <- GET(url_locs, authenticate(Sys.getenv("GITHUB_USERNAME"), Sys.getenv("GITHUB_PAT")))
  read_csv(content(locs, "text")) %>%
    filter(study_id %in% events$study_id) %>%
    filter(!is.na(study_location)) 
}
