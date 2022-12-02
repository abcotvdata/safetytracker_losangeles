library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.providers)
library(sf)
library(tidyr)

# GEOGRAPHY
# Build out California geography for all places, starting with SoCal here


# Read in geojson and then transform to sf format
# we will use analysis neighborhoods if the crime data comes cleanly that way
#beats <- st_read("data/source/geo/Law_Enforcement_Reporting_Districts.geojson") %>% st_transform(3857) %>% janitor::clean_names()
# beats <- st_read("data/source/sf/geo/sf_police_analysisneighborhoods.geojson") %>% st_transform(3857)


# Get demographic data for Census block groups to aggregate/apportion to precinct geography
# Also transforming to match the planar projection of SFPD's beats spatial file
# This also reduces us down to just the numeric population est and geometry
cal_places <- get_decennial(geography = "place", 
                        year = 2020,
                        output = 'wide',
                        variables = "P1_001N", 
                        state = "CA",
                        geometry = TRUE) %>%
  rename("population"="P1_001N") %>%
  janitor::clean_names()

#beats <- beats %>% st_transform(4326)
#beats <- st_make_valid(beats)

# saving a clean geojson and separate RDS for use in tracker
st_write(cal_places,"data/source/geo/cal_places.geojson",append=FALSE)
saveRDS(cal_places,"scripts/rds/cal_places.rds")
# add line  below when uploading data for pages
# beats <- st_read("data/source/geo/beats.geojson")

california_crime_annual <- read_csv("data/source/california_crime_annual.csv", 
                                    col_types = cols(Year = col_character()))

cal_crime21 <- california_crime_annual %>% filter(Year==2021)

cal_places$place <- cal_places$name
cal_places$place <- str_replace(cal_places$place," CDP, California","")
cal_places$place <- str_replace(cal_places$place," city, California","")
cal_places$place <- str_replace(cal_places$place," town, California","")
cal_places$place <- sub(" CDP.*", "\\1", cal_places$place)

cal_places2 <- left_join(cal_places,cal_crime21,by=c("place"="NCICCode"))

cal_crime <- california_crime_annual %>% janitor::clean_names()
cal_crime_murder <- cal_crime %>% select(year,county,ncic_code,homicide_sum) %>% spread(year,homicide_sum)
cal_crime_rape <- cal_crime %>% select(year,county,ncic_code,for_rape_sum) %>% spread(year,for_rape_sum)
cal_crime_assault <- cal_crime %>% select(year,county,ncic_code,agg_assault_sum) %>% spread(year,agg_assault_sum)
cal_crime_robbery <- cal_crime %>% select(year,county,ncic_code,robbery_sum) %>% spread(year,robbery_sum)
cal_crime_burglary <- cal_crime %>% select(year,county,ncic_code,burglary_sum) %>% spread(year,burglary_sum)
cal_crime_theft <- cal_crime %>% select(year,county,ncic_code,l_ttotal_sum) %>% spread(year,l_ttotal_sum)
cal_crime_autotheft <- cal_crime %>% select(year,county,ncic_code,vehicle_theft_sum) %>% spread(year,vehicle_theft_sum)


# BARE PRECINCT MAP JUST FOR TESTING PURPOSES
# CAN COMMENT OUT ONCE FINALIZED
# Set bins for beats pop map
popbins <- c(0,20000,50000,75000,100000,250000, Inf)
poppal <- colorBin("viridis", cal_places$population, bins = popbins)
poplabel <- paste(sep = "<br>", cal_places$place,prettyNum(cal_places$population, big.mark = ","))

cal_places_map <- leaflet(cal_places) %>%
  setView(-118.243, 34.052, zoom = 8) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "CartoDB.PositronOnlyLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~poppal(`population`))
cal_places_map

popbins <- c(0,200,500,1000,5000,10000,Inf)
poppal <- colorBin("viridis", cal_places2$population, bins = popbins)
poplabel <- paste(sep = "<br>", cal_places2$place,prettyNum(cal_places2$Violent_sum, big.mark = ","))

cal_places_map2 <- leaflet(cal_places2) %>%
  setView(-118.243, 34.052, zoom = 8) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "CartoDB.PositronOnlyLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~poppal(`Violent_sum`))
cal_places_map2
