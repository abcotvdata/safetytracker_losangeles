library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.providers)
library(leaflet.extras)
library(sf)

download.file("https://services5.arcgis.com/TiBPJbi9Tj52qsV8/ArcGIS/rest/services/Station_Boundaries_Layer/FeatureServer/7/query?where=0%3D0&objectIds=&time=&geometry=&geometryType=esriGeometryEnvelope&inSR=&spatialRel=esriSpatialRelIntersects&resultType=none&distance=0.0&units=esriSRUnit_Meter&relationParam=&returnGeodetic=false&outFields=*&returnGeometry=true&returnCentroid=false&featureEncoding=esriDefault&multipatchOption=xyFootprint&maxAllowableOffset=&geometryPrecision=&outSR=&defaultSR=&datumTransformation=&applyVCSProjection=false&returnIdsOnly=false&returnUniqueIdsOnly=false&returnCountOnly=false&returnExtentOnly=false&returnQueryGeometry=false&returnDistinctValues=false&cacheHint=false&orderByFields=&groupByFieldsForStatistics=&outStatistics=&having=&resultOffset=&resultRecordCount=&returnZ=false&returnM=false&returnExceededLimitFeatures=true&quantizationParameters=&sqlFormat=none&f=pgeojson",
              "data/source/geo/riverside_sheriff_districts.geojson")

# GEOGRAPHY FOR LOS ANGELES MAPS
# OPTIONS INCLUDE: DETAILED REPORTING DISTRICT MAP FOR ALL DEPARTMENTS
districts <- st_read("data/source/geo/riverside_sheriff_districts.geojson") %>% st_transform(3857) %>% janitor::clean_names()

# Create LAPD and Sheriff districts map file
# districts <- beats %>%
#  filter(agency == "LAPD" | agency == "LASD") %>%
#  group_by(agency,st_name) %>%
#  summarise(geometry = sf::st_union(geometry)) %>%
#  ungroup()

# Get demographic data for Census block groups to aggregate/apportion to precinct geography
# Also transforming to match the planar projection of SFPD's beats spatial file
# This also reduces us down to just the numeric population est and geometry
blocks <- get_decennial(geography = "block", 
                        year = 2020,
                        output = 'wide',
                        variables = "P1_001N", 
                        state = "CA",
                        county = c("Riverside"),
                        geometry = TRUE) %>%
  rename("population"="P1_001N") %>% 
  select(3) %>%
  janitor::clean_names() %>%
  st_transform(3857)

districts <- st_make_valid(districts)
# Calculate the estimated population of LAPD/LASD geographies/interpolate with tidycensus bgs
districts_withpop <- st_interpolate_aw(blocks, districts, ext = TRUE)
# Drops geometry so it's not duplicated in the merge
districts_withpop <- st_drop_geometry(districts_withpop)
# Binds that new population column to the table
districts <- cbind(districts,districts_withpop)
# Cleans up unneeded calculation file
# rm(beats_withpop, blocks)

# Check total population assigned/estimated across all districts
sum(districts$population) # result is 6,853,094

# Round the population figure; rounded to nearest thousand
districts$population <- round(districts$population,-2)

districts <- districts %>% st_transform(4326)

#districts <- districts %>% rename("district"="st_name")
#districts$district <- gsub(" Division", "", districts$district)
#districts$district <- gsub(" / ", "/", districts$district)

# saving a clean geojson and separate RDS for use in tracker
file.remove("data/output/geo/riverside_sheriff_districts.geojson")
st_write(districts,"data/output/geo/riverside_sheriff_districts.geojson")
saveRDS(districts,"scripts/rds/riverside_sheriff_districts.rds")

# DISTRICT MAP FOR TESTING PURPOSES
# CAN COMMENT OUT ONCE FINALIZED
# Set bins for beats pop map
popbins <- c(0,5000,50000,100000,125000,200000,Inf)
poppal <- colorBin("viridis", districts$population, bins = popbins)
poplabel <- paste(sep = "<br>", districts$primary,districts$secondary,prettyNum(districts$population, big.mark = ","))

riverside_districts_map <- leaflet(districts) %>%
  setView(-117.243, 33.70, zoom = 10) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "CartoDB.PositronOnlyLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~poppal(`population`)) %>%
  addPolygons(data = cal_counties, color = "blue", popup = paste("YO HO HO NOBODY LIVES HERE"), weight = 5, smoothFactor = 0.5,
              opacity = 0.2, fillOpacity = 0)
riverside_districts_map

riverside_districts_map <- leaflet(cal_places2) %>%
  setView(-117.243, 33.70, zoom = 10) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "CartoDB.PositronOnlyLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~poppal(`population`)) %>%
  addPolygons(data = cal_counties, color = "blue", popup = paste("YO HO HO NOBODY LIVES HERE"), weight = 5, smoothFactor = 0.5,
              opacity = 0.2, fillOpacity = 0)
riverside_districts_map



