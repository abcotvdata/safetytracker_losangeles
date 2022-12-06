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

california_crime_annual <- read_csv("data/source/reference/california_crime_annual.csv", 
                                    col_types = cols(Year = col_character()))

cal_crime21 <- california_crime_annual %>% filter(Year==2021)

cal_places$place <- cal_places$name
cal_places$place <- str_replace(cal_places$place," CDP, California","")
cal_places$place <- str_replace(cal_places$place," city, California","")
cal_places$place <- str_replace(cal_places$place," town, California","")
cal_places$place <- sub(" CDP.*", "\\1", cal_places$place)

cal_places2 <- left_join(cal_places,cal_crime21,by=c("place"="NCICCode"))

cal_crime <- california_crime_annual %>% janitor::clean_names()
cal_crime_murder <- cal_crime %>% select(year,county,ncic_code,homicide_sum) %>% spread(year,homicide_sum) %>% select(1,2,18:39)
cal_crime_rape <- cal_crime %>% select(year,county,ncic_code,for_rape_sum) %>% spread(year,for_rape_sum) %>% select(1,2,18:39)
cal_crime_assault <- cal_crime %>% select(year,county,ncic_code,agg_assault_sum) %>% spread(year,agg_assault_sum) %>% select(1,2,18:39)
cal_crime_robbery <- cal_crime %>% select(year,county,ncic_code,robbery_sum) %>% spread(year,robbery_sum) %>% select(1,2,18:39)
cal_crime_burglary <- cal_crime %>% select(year,county,ncic_code,burglary_sum) %>% spread(year,burglary_sum) %>% select(1,2,18:39)
cal_crime_theft <- cal_crime %>% select(year,county,ncic_code,l_ttotal_sum) %>% spread(year,l_ttotal_sum) %>% select(1,2,18:39)
cal_crime_autotheft <- cal_crime %>% select(year,county,ncic_code,vehicle_theft_sum) %>% spread(year,vehicle_theft_sum) %>% select(1,2,18:39)

lac_archive_murder <- cal_crime %>% filter(ncic_code=="Los Angeles" | ncic_code=="Los Angeles Co. Sheriff's Department") %>% 
  select(year,county,ncic_code,homicide_sum) %>% 
  group_by(county,year) %>%
  summarise(homicide_sum=sum(homicide_sum)) %>%
  spread(year,homicide_sum) %>% select(1,17:38) %>% mutate(category="Homicide")
lac_archive_rape <- cal_crime %>% filter(ncic_code=="Los Angeles" | ncic_code=="Los Angeles Co. Sheriff's Department") %>% 
  select(year,county,ncic_code,for_rape_sum) %>% 
  group_by(county,year) %>%
  summarise(for_rape_sum=sum(for_rape_sum)) %>%
  spread(year,for_rape_sum) %>% select(1,17:38) %>% mutate(category="Sexual Assault")
lac_archive_assault <- cal_crime %>% filter(ncic_code=="Los Angeles" | ncic_code=="Los Angeles Co. Sheriff's Department") %>% 
  select(year,county,ncic_code,agg_assault_sum) %>% 
  group_by(county,year) %>%
  summarise(agg_assault_sum=sum(agg_assault_sum)) %>%
  spread(year,agg_assault_sum) %>% select(1,17:38) %>% mutate(category="Aggravated Assault")
lac_archive_robbery <- cal_crime %>% filter(ncic_code=="Los Angeles" | ncic_code=="Los Angeles Co. Sheriff's Department") %>% 
  select(year,county,ncic_code,robbery_sum) %>% 
  group_by(county,year) %>%
  summarise(robbery_sum=sum(robbery_sum)) %>%
  spread(year,robbery_sum) %>% select(1,17:38) %>% mutate(category="Robbery")
lac_archive_burglary <- cal_crime %>% filter(ncic_code=="Los Angeles" | ncic_code=="Los Angeles Co. Sheriff's Department") %>% 
  select(year,county,ncic_code,burglary_sum) %>% 
  group_by(county,year) %>%
  summarise(burglary_sum=sum(burglary_sum)) %>%
  spread(year,burglary_sum) %>% select(1,17:38) %>% mutate(category="Burglary")
lac_archive_theft <- cal_crime %>% filter(ncic_code=="Los Angeles" | ncic_code=="Los Angeles Co. Sheriff's Department") %>% 
  select(year,county,ncic_code,l_ttotal_sum) %>% 
  group_by(county,year) %>%
  summarise(l_ttotal_sum=sum(l_ttotal_sum)) %>%
  spread(year,l_ttotal_sum) %>% select(1,17:38) %>% mutate(category="Larceny")
lac_archive_autotheft <- cal_crime %>% filter(ncic_code=="Los Angeles" | ncic_code=="Los Angeles Co. Sheriff's Department") %>% 
  select(year,county,ncic_code,vehicle_theft_sum) %>% 
  group_by(county,year) %>%
  summarise(vehicle_theft_sum=sum(vehicle_theft_sum)) %>%
  spread(year,vehicle_theft_sum) %>% select(1,17:38) %>% mutate(category="Vehicle Theft")
lac_yearly_archive <- rbind(lac_archive_murder,lac_archive_rape,lac_archive_assault,
                            lac_archive_robbery,lac_archive_burglary,lac_archive_theft,lac_archive_autotheft)


cal_la_murder <- cal_crime_murder %>% filter(ncic_code=="Los Angeles" | ncic_code=="Los Angeles Co. Sheriff's Department") %>% group_by(county) %>% mutate(category="Murder")
cal_la_rape <- cal_crime_murder %>% filter(ncic_code=="Los Angeles" | ncic_code=="Los Angeles Co. Sheriff's Department") %>% group_by(county) %>% mutate(category="Rape")




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
              fillColor = ~poppal(`population`)) %>%
 # addPolygons(data = districts, color = "red", popup = poplabel2, weight = 2, smoothFactor = 0.5,
  #            opacity = 0.5, fillOpacity = 0.0)
cal_places_map

popbins <- c(0,200,500,1000,5000,10000,Inf)
poppal <- colorBin("viridis", cal_places2$Violent_sum, bins = popbins,na.color="blue")
poplabel <- paste(sep = "<br>", cal_places2$place,prettyNum(cal_places2$Violent_sum, big.mark = ","))

cal_places_map2 <- leaflet(cal_places2) %>%
  setView(-118.243, 34.052, zoom = 8) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "CartoDB.PositronOnlyLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~poppal(`Violent_sum`))
cal_places_map2
