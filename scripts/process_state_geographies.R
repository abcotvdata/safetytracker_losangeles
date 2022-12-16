library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.providers)
# library(tidyr)
library(sf)

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
cal_places$place <- str_replace(cal_places$place,"Big Bear City","Big Bear")
cal_places$place <- str_replace(cal_places$place,"La Cañada Flintridge","La Canada-Flintridge")
cal_places$place <- sub(" CDP.*", "\\1", cal_places$place)
cal_places$place <- ifelse(cal_places$geoid=="0665042","Ventura",cal_places$place)
cal_places$state <- "California"

# Some adjustments to the fields in cal_counties to merge to crime data
cal_counties$county <- cal_counties$name
cal_counties$county <- str_replace(cal_counties$county,", California","")

# Assign county names for filter and temporarily import Cal DOJ list for filter
counties <- c("Los Angeles County", "Orange County","Ventura County","San Bernardino County","Riverside County")
socal_annual <- readRDS("scripts/rds/socal_annual.rds")
# Apply filters to assign places to counties, filter for counties and Cal DOJ agencies
socal_places <- st_join(cal_places, cal_counties %>% select(5), left = FALSE, largest = TRUE) %>%
  filter(county %in% counties) %>% filter(place %in% socal_annual$ncic_code)
# Round population to estimate, nearest hundred, consistent with LA Districts
socal_places$population <- round(socal_places$population,-2)

# Creating a singular file for making rural cutouts by county
# make sure to make the resulting file a valid sf file
all_socal_places <- socal_places %>%
  group_by(state) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup() %>% st_make_valid()

# Create five single polygon county files
riverside_county <- cal_counties %>% filter(county=="Riverside County") %>% st_make_valid()
orange_county <- cal_counties %>% filter(county=="Orange County") %>% st_make_valid()
sanbern_county <- cal_counties %>% filter(county=="San Bernardino County") %>% st_make_valid()
ventura_county <- cal_counties %>% filter(county=="Ventura County") %>% st_make_valid()

# Make the rural "remnant" area polygons for each county
rural_riverside <- st_difference(riverside_county,all_socal_places)
rural_orange <- st_difference(orange_county,all_socal_places)
rural_sanbern <- st_difference(sanbern_county,all_socal_places)
rural_ventura <- st_difference(ventura_county,all_socal_places)

# Make the rural "remnant" area polygons for each county
rural_riverside$place <- "Riverside Co. Sheriff's Department"
rural_orange$place <- "Orange Co. Sheriff's Department"
rural_sanbern$place <- "San Bernardino Co. Sheriff's Department"
rural_ventura$place <- "Ventura Co. Sheriff's Department"

# Make the rural "remnant" area polygons for each county
socal_county_pops <- socal_places %>% group_by(county) %>% summarise(pop=sum(population))
rural_riverside$population <- riverside_county$population - 2027000
rural_orange$population <- orange_county$population - 3053900
rural_sanbern$population <- sanbern_county$population - 1888800
rural_ventura$population <- ventura_county$population - 750000

# Add these rural sheriff's coverage areas back into socal_places
socal_places <- rbind(socal_places,rural_riverside,rural_orange,rural_sanbern,rural_ventura)

# Do some cleanup 
rm(rural_riverside,rural_orange,rural_sanbern,rural_ventura)
rm(riverside_county,orange_county,sanbern_county,ventura_county)
rm(cal_counties, cal_places, all_socal_places,socal_county_pops)

# Create new police_map
# la_districts <- readRDS(data/rds/la_county_police_districts.rds)
# Remove all LA County places from the so_cal places now
police_map <- bind_rows(socal_places %>% filter(county!="Los Angeles County"),la_districts)
# Add upgraded LA County districts to the new region wide police districts map
police_map$county <- ifelse(is.na(police_map$county),"Los Angeles County",police_map$county)
# Recode place field for all the LA county policing districts
police_map$place <- case_when(police_map$agency=="OTHER" & police_map$commtype=="City" ~ police_map$place_name,
                              police_map$agency=="LASD" & police_map$commtype=="City" ~ police_map$place_name,
                              police_map$agency=="LASD" & police_map$commtype=="Unincorporated" ~ paste(police_map$place_name, police_map$district),
                              police_map$agency=="LAPD" ~ paste(police_map$agency, police_map$district),
                              TRUE ~ police_map$place)


# Set bins for beats pop map
popbins <- c(0,1000,5000,25000,50000,75000,100000,125000,150000,175000,200000,Inf)
poppal <- colorBin("viridis", police_map$population, bins = popbins)
poplabel <- paste(sep = "<br>", police_map$place,police_map$county,police_map$agency,police_map$commtype,prettyNum(police_map$population, big.mark = ","))
# Create map
socal_police_map <- leaflet(police_map) %>%
  setView(-117.243, 33.70, zoom = 10) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "CartoDB.PositronOnlyLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.5,
              fillColor = ~poppal(`population`)) 
socal_police_map 

# Saving final product for reuse; police map includes 4 counties + LA Co
saveRDS(police_map,"scripts/rds/police_map.rds")
# Saving the file with only the police districts in the 4 counties besides LA too
saveRDS(socal_places,"scripts/rds/police_map.rds")

rm(socal_police_map)
