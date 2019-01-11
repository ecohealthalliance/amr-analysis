library(tidyverse)
library(httr)
library(lubridate)
library(wbstats)

url <- "https://raw.githubusercontent.com/ecohealthalliance/amr-db/master/data/events_db.csv"
events <- GET(url, authenticate("emmamendelsohn", Sys.getenv("GITHUB_PAT")))
read_csv(content(events, "text"))

#-----------------Country-wide data-----------------
events_by_country <- events %>%
  group_by(study_country) %>%
  count()
#-----------------World Bank data-----------------
# Population, total
pop_data <- wb(indicator = "SP.POP.TOTL", startdate = 1998, enddate = 2015, return_wide = TRUE) %>%
  mutate(country = tolower(country)) %>%
  filter(country %in% events_by_country$study_country[!is.na(events_by_country$study_country)])

# a=unique(pop_data$country)
# b=unique(events_by_country$study_country)
# b[!b%in%a]
# 
# "egypt, arab rep."
# "iran, islamic rep."  
# "slovak republic"
# "korea, rep."
# "venezuela, rb"    
# "taiwan"
# "palestine"