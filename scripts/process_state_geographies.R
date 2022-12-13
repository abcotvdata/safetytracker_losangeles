library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.providers)
library(tidyr)
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

counties <- c("Los Angeles County", "Orange County","Ventura County","San Bernardino County","Riverside County")

socal_places <- st_join(cal_places, cal_counties %>% select(5), left = FALSE, largest = TRUE) %>%
  filter(county %in% counties) %>% filter 

#saveRDS(cal_places,"scripts/rds/socal_places.rds")
#saveRDS(cal_places,"scripts/rds/cal_places.rds")
#saveRDS(cal_places,"scripts/rds/cal_counties.rds")

# Creating a singular file for making rural cutouts by county
# make sure to make the resulting file a valid sf file
all_socal_places <- murders_places %>%
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

# Add these rural sheriff's coverage areas back into socal_places
socal_places <- rbind(socal_places, rural_riverside,rural_orange,rural_sanbern,rural_ventura)

# Jurisdiction mapping for testing purposes only
# just processing this for testing map
cal_crime21 <- california_crime_annual %>% filter(Year=="2021")
cal_places2 <- inner_join(socal_places,cal_crime21,by=c("place"="NCICCode")) %>% filter(county!="Los Angeles County")
la_places <- inner_join(socal_places,cal_crime21,by=c("place"="NCICCode")) %>% filter(county=="Los Angeles County" & place!="Los Angeles")


# Set bins for beats pop map
popbins <- c(0,1000,5000,25000,50000,75000,100000,125000,150000,175000,200000,Inf)
poppal <- colorBin("viridis", cal_places2$population, bins = popbins)
poplabel <- paste(sep = "<br>", cal_places2$place,cal_places2$County,cal_places2$Homicide_sum,cal_places2$HomicideClr_sum,prettyNum(cal_places2$population, big.mark = ","))
laclabel <- paste(sep = "<br>", la_places$place,la_places$County,la_places$Homicide_sum,la_places$HomicideClr_sum,prettyNum(la_places$population, big.mark = ","))
# Create map
so_cal_map <- leaflet(cal_places2) %>%
  setView(-117.243, 33.70, zoom = 10) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "CartoDB.PositronOnlyLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.5,
              fillColor = ~poppal(`population`), group="notla") %>%
#  addPolygons(data = rural_riverside, color = "transparent", popup = paste("YO HO HO ALMOST NOBODY LIVES HERE<br>We will be adding rural sheriff data here"), weight = 2, smoothFactor = 0.5,
#              opacity = 0.5, fillOpacity = 0, group="notla") %>%
#  addPolygons(data = rural_orange, color = "transparent", popup = paste("YO HO HO ALMOST NOBODY LIVES HERE<br>We will be adding rural sheriff data here"), weight = 2, smoothFactor = 0.5,
#              opacity = 0.5, fillOpacity = 0, group="notla") %>%
#  addPolygons(data = rural_sanbern, color = "transparent", popup = paste("YO HO HO ALMOST NOBODY LIVES HERE<br>We will be adding rural sheriff data here"), weight = 2, smoothFactor = 0.5,
#              opacity = 0.5, fillOpacity = 0, group="notla") %>%
#  addPolygons(data = rural_ventura, color = "transparent", popup = paste("YO HO HO ALMOST NOBODY LIVES HERE<br>We will be adding rural sheriff data here"), weight = 2, smoothFactor = 0.5,
#              opacity = 0.5, fillOpacity = 0, group="notla") %>%
  addPolygons(data = districts, color = "red", popup = paste("LA area disrict",districts$district,districts$agency,districts$population), weight = 2, smoothFactor = 0.5,
            opacity = 0.5, fillOpacity = 0.2, group="lacounty") %>%
  addPolygons(data = la_places, color = "blue", popup = laclabel, weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.2, group="laother") %>%
  addLayersControl(
    overlayGroups = c("lacounty","laother","notla"),
    options = layersControlOptions(collapsed = FALSE),
    position = 'bottomleft')
so_cal_map





  
  
  