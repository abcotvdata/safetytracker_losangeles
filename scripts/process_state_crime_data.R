library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.providers)
library(sf)
library(tidyr)

# Get the map files we need
districts <- readRDS("scripts/rds/la_county_police_districts.rds")

# Read in the state crime file
california_crime_annual <- read_csv("data/source/reference/california_crime_annual.csv", 
                                    col_types = cols(Year = col_character()))

# OPEN WORK: This matched completely
# There are no actual police jurisdictions without a location on the map
zz_la_county_test <- left_join(cal_crime_assault %>% filter(county=="Los Angeles County"),districts,by=c("ncic_code"="place_name"))

# Now reverse test to see which areas we don't have data for from the state file
z2z_la_county_test <- left_join(districts,cal_crime_assault %>% filter(county=="Los Angeles County"),by=c("place_name"="ncic_code"))
# Only areas without crime data are the unincorporated sheriff areas which we can pull from sheriff file
# and all the areas within Los Angeles were given the totals for Los Angeles so we need to filter that stuff out
# need a process to replace the cal_state data with the local data from the city police and sheriff's department unincorporated areas


# Quick look/test at the other counties
cal_places <- readRDS("scripts/rds/cal_places.rds")
z33z_socal_test <- left_join(cal_places,cal_crime_assault,by=c("place"="ncic_code"))


# Starter file for a table
la_table_test <- left_join(districts,cal_crime_murder %>% filter(county=="Los Angeles County"),by=c("place_name"="ncic_code"))
la_table_test$rate19 <- round(la_table_test$`2019`/la_table_test$population*100000,1)
la_table_test$rate20 <- round(la_table_test$`2020`/la_table_test$population*100000,1)
la_table_test$rate21 <- round(la_table_test$`2021`/la_table_test$population*100000,1)
la_table_test$moredata <- ifelse(la_table_test$agency=="LAPD","Live data available. <a href='https://abcotvdata.github.io/safetytracker_losangeles/Los_Angeles_Safety_Tracker.html'>Go to Los Angeles Tracker</a>.",NA)
la_table_test$place <- paste0(la_table_test$place_name," ^",la_table_test$county,"^")
la_table_test %>% st_drop_geometry() %>% write_csv("data/output/la_table_test.csv")

Puerto Rico ^United States^
  
  