
get_locations <- function(events){
  url_locs <- "https://raw.githubusercontent.com/ecohealthalliance/amr-db/master/data-processed/locations.csv"
  locs <- GET(url_locs, authenticate(Sys.getenv("GITHUB_USERNAME"), Sys.getenv("GITHUB_PAT")))
  read_csv(content(locs, "text")) %>%
    filter(study_id %in% events$study_id) %>%
    filter(!is.na(study_location)) 
}
