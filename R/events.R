get_events <- function(){
  # event info
  url_events <- "https://raw.githubusercontent.com/ecohealthalliance/amr-db/master/events-db.csv"
  events <- GET(url_events, authenticate(Sys.getenv("GITHUB_USERNAME"), Sys.getenv("GITHUB_PAT")))
  read_csv(content(events, "text"))  %>%
    mutate(start_year = as.integer(substr(start_date, 1, 4))) %>%
    filter(start_year >= 2006) # removes promed mentions prior to 2006
}
