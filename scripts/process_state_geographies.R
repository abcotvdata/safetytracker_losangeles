library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.providers)
library(sf)
library(tidyr)
library(rgeos)

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

# Some adjustments to the fields in cal_counties to merge to crime data
cal_counties$county <- cal_counties$name
cal_counties$county <- str_replace(cal_counties$county,", California","")

saveRDS(cal_places,"scripts/rds/cal_places.rds")
saveRDS(cal_places,"scripts/rds/cal_counties.rds")

# Creating a singular file for making rural cutouts by county
# must incude st_make_valid to work
all_cal_places <- cal_places %>%
  group_by(state) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>% st_make_valid()

# Create five single polygon county files
riverside_county <- cal_counties %>% filter(county=="Riverside County") %>% st_make_valid()
orange_county <- cal_counties %>% filter(county=="Orange County") %>% st_make_valid()
sanbern_county <- cal_counties %>% filter(county=="San Bernardino County") %>% st_make_valid()
ventura_county <- cal_counties %>% filter(county=="Ventura County") %>% st_make_valid()

# Make the rural "remnant" area polygons for each county
rural_riverside <- st_difference(riverside_county,all_cal_places)
rural_orange <- st_difference(orange_county,all_cal_places)
rural_sanbern <- st_difference(sanbern_county,all_cal_places)
rural_ventura <- st_difference(ventura_county,all_cal_places)

# Jurisdiction mapping for testing purposes only

# Set bins for beats pop map
popbins <- c(0,5000,50000,100000,125000,200000,Inf)
poppal <- colorBin("viridis", cal_places2$population, bins = popbins)
poplabel <- paste(sep = "<br>", cal_places2$place,cal_places2$County,cal_places2$Homicide_sum,cal_places2$HomicideClr_sum,prettyNum(cal_places2$population, big.mark = ","))
# Create map
so_cal_map <- leaflet(cal_places) %>%
  setView(-117.243, 33.70, zoom = 10) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "CartoDB.PositronOnlyLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~poppal(`population`)) %>%
  addPolygons(data = rural_riverside, color = "transparent", popup = paste("YO HO HO NOBODY LIVES HERE"), weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0) %>%
  addPolygons(data = rural_orange, color = "transparent", popup = paste("YO HO HO NOBODY LIVES HERE"), weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0) %>%
  addPolygons(data = rural_sanbern, color = "transparent", popup = paste("YO HO HO NOBODY LIVES HERE"), weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0) %>%
  addPolygons(data = rural_ventura, color = "transparent", popup = paste("YO HO HO NOBODY LIVES HERE"), weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0)
so_cal_map

