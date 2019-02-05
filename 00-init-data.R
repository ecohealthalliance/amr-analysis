library(tidyverse)
library(httr)
library(lubridate)
library(wbstats)
library(here)

url <- "https://raw.githubusercontent.com/ecohealthalliance/amr-db/master/data/events_db.csv"
events <- GET(url, authenticate("emmamendelsohn", Sys.getenv("GITHUB_PAT")))
events <- read_csv(content(events, "text"))

#-----------------Country-wide data-----------------
# Rename a few countries in our data to match WB
events <- events %>%
  # need to follow up on this (and implement earlier in workflow)
  mutate(study_country = replace(study_country, study_country == "palestine", "israel"),
         study_country = replace(study_country, study_country == "taiwan", "china")
  )

# Summarize counts
events_by_country <- events %>%
  group_by(study_country) %>%
  count() %>%
  ungroup() %>%
  rename(country = study_country) %>%
  na.omit()
#-----------------World Bank data-----------------
# Population, total
wb_data <- wb(indicator = c("SP.POP.TOTL", "NY.GDP.MKTP.CD"), startdate = 2015, enddate = 2015, return_wide = TRUE) %>%
  mutate(country = tolower(country),
         country = str_remove(country, ",.*$"),
         country = replace(country, country == "slovak republic", "slovakia"),
         country = replace(country, country == "korea", "south korea")) %>%
  right_join(events_by_country) %>%
  dplyr::select(-iso3c, -iso2c) %>%
  mutate(continent = countrycode::countrycode(sourcevar = country,
                                origin = "country.name",
                                destination = "continent"))

# a=unique(wb_data$country)
# b=unique(events_by_country$country)
# b[!b%in%a]

write_csv(wb_data, here::here("country_level.csv"))
