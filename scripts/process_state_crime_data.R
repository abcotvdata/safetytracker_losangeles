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
z33z_socal_test <- left_join(cal_places,cal_crime_assault,by=c("place"="ncic_code")) %>% filter(!is.na(county))


# Starter file for a table for MURDERS
laco_murders <- left_join(districts,cal_crime_murder %>% filter(county=="Los Angeles County"),by=c("place_name"="ncic_code"))
laco_murders$rate19 <- round(laco_murders$`2019`/laco_murders$population*100000,1)
laco_murders$rate20 <- round(laco_murders$`2020`/laco_murders$population*100000,1)
laco_murders$rate21 <- round(laco_murders$`2021`/laco_murders$population*100000,1)
laco_murders$moredata <- ifelse(laco_murders$agency=="LAPD","<a href='https://abcotvdata.github.io/safetytracker_losangeles/Los_Angeles_Safety_Tracker.html'>Live data available.</a>.",NA)
laco_murders$place <- paste0(laco_murders$district," ^",laco_murders$agency,"^")
laco_murders %>% st_drop_geometry() %>% select(34,8:32,33) %>% write_csv("data/output/laco_murders.csv")
# Starter file for ROBBERIES
laco_robberies <- left_join(districts,cal_crime_robbery %>% filter(county=="Los Angeles County"),by=c("place_name"="ncic_code"))
laco_robberies$rate19 <- round(laco_robberies$`2019`/laco_robberies$population*100000,1)
laco_robberies$rate20 <- round(laco_robberies$`2020`/laco_robberies$population*100000,1)
laco_robberies$rate21 <- round(laco_robberies$`2021`/laco_robberies$population*100000,1)
laco_robberies$moredata <- ifelse(laco_robberies$agency=="LAPD","<a href='https://abcotvdata.github.io/safetytracker_losangeles/Los_Angeles_Safety_Tracker.html'>Live data available.</a>.",NA)
laco_robberies$place <- paste0(laco_robberies$district," ^",laco_robberies$agency,"^")
laco_robberies %>% st_drop_geometry() %>% select(34,8:32,33) %>% write_csv("data/output/laco_robberies.csv")
# Starter file for a BURGLARIES
laco_burglaries <- left_join(districts,cal_crime_burglary %>% filter(county=="Los Angeles County"),by=c("place_name"="ncic_code"))
laco_burglaries$rate19 <- round(laco_burglaries$`2019`/laco_burglaries$population*100000,1)
laco_burglaries$rate20 <- round(laco_burglaries$`2020`/laco_burglaries$population*100000,1)
laco_burglaries$rate21 <- round(laco_burglaries$`2021`/laco_burglaries$population*100000,1)
laco_burglaries$moredata <- ifelse(laco_burglaries$agency=="LAPD","<a href='https://abcotvdata.github.io/safetytracker_losangeles/Los_Angeles_Safety_Tracker.html'>Live data available.</a>.",NA)
laco_burglaries$place <- paste0(laco_burglaries$district," ^",laco_burglaries$agency,"^")
laco_burglaries %>% st_drop_geometry() %>% select(34,8:32,33) %>% write_csv("data/output/laco_burglaries.csv")
# Starter file for a table
laco_autothefts <- left_join(districts,cal_crime_autotheft %>% filter(county=="Los Angeles County"),by=c("place_name"="ncic_code"))
laco_autothefts$rate19 <- round(laco_autothefts$`2019`/laco_autothefts$population*100000,1)
laco_autothefts$rate20 <- round(laco_autothefts$`2020`/laco_autothefts$population*100000,1)
laco_autothefts$rate21 <- round(laco_autothefts$`2021`/laco_autothefts$population*100000,1)
laco_autothefts$moredata <- ifelse(laco_autothefts$agency=="LAPD","<a href='https://abcotvdata.github.io/safetytracker_losangeles/Los_Angeles_Safety_Tracker.html'>Live data available.</a>.",NA)
laco_autothefts$place <- paste0(laco_autothefts$district," ^",laco_autothefts$agency,"^")
laco_autothefts %>% st_drop_geometry() %>% select(34,8:32,33) %>% write_csv("data/output/laco_autothefts.csv")

  