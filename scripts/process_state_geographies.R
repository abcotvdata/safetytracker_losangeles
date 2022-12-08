library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.providers)
library(sf)
library(tidyr)

# Get demographic data and geography for Census places
cal_places <- get_decennial(geography = "place", 
                            year = 2020,
                            output = 'wide',
                            variables = "P1_001N", 
                            state = "CA",
                            geometry = TRUE) %>%
  rename("population"="P1_001N") %>%
  janitor::clean_names()

# Get demographic data and geography for Census places
cal_counties <- get_decennial(geography = "county", 
                              year = 2020,
                              output = 'wide',
                              variables = "P1_001N", 
                              state = "CA",
                              geometry = TRUE) %>%
  rename("population"="P1_001N") %>%
  janitor::clean_names()

# Some adjustments to the fields in cal_places to merge to crime data
cal_places$place <- cal_places$name
cal_places$place <- str_replace(cal_places$place," CDP, California","")
cal_places$place <- str_replace(cal_places$place," city, California","")
cal_places$place <- str_replace(cal_places$place," town, California","")
cal_places$place <- sub(" CDP.*", "\\1", cal_places$place)

# Some adjustments to the fields in cal_places to merge to crime data
cal_counties$county <- cal_counties$name
cal_counties$county <- str_replace(cal_counties$county,", California","")

saveRDS(cal_places,"scripts/rds/cal_places.rds")
saveRDS(cal_places,"scripts/rds/cal_counties.rds")