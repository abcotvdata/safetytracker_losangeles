library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.providers)
library(sf)

# GEOGRAPHY FOR LOS ANGELES MAPS
# OPTIONS INCLUDE: DETAILED REPORTING DISTRICT MAP FOR ALL DEPARTMENTS
beats <- st_read("data/source/geo/Law_Enforcement_Reporting_Districts.geojson") %>% st_transform(3857) %>% janitor::clean_names()

# Create LAPD and Sheriff districts map file
districts <- beats %>%
  filter(agency == "LAPD" | agency == "LASD") %>%
  group_by(agency,station) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

# Get demographic data for Census block groups to aggregate/apportion to precinct geography
# Also transforming to match the planar projection of SFPD's beats spatial file
# This also reduces us down to just the numeric population est and geometry
blocks <- get_decennial(geography = "block", 
                        year = 2020,
                        output = 'wide',
                        variables = "P1_001N", 
                        state = "CA",
                        county = c("Los Angeles"),
                        geometry = TRUE) %>%
  rename("population"="P1_001N") %>% 
  select(3) %>%
  janitor::clean_names() %>%
  st_transform(3857)

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
districts$population <- round(districts$population,-3)
districts <- districts %>% st_transform(4326)
districts <- st_make_valid(districts)

# saving a clean geojson and separate RDS for use in tracker
file.remove("data/output/geo/la_police_districts.geojson")
st_write(districts,"data/output/geo/la_police_districts.geojson")
saveRDS(districts,"scripts/rds/la_police_districts.rds")

# DISTRICT MAP FOR TESTING PURPOSES
# CAN COMMENT OUT ONCE FINALIZED
# Set bins for beats pop map
popbins <- c(0,50000,100000,125000,150000,200000,Inf)
poppal <- colorBin("viridis", districts$population, bins = popbins)
poplabel <- paste(sep = "<br>", districts$agency,districts$station,prettyNum(districts$population, big.mark = ","))

la_districts_map <- leaflet(districts) %>%
  setView(-118.243, 34.052, zoom = 10) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "CartoDB.PositronOnlyLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~poppal(`population`))
la_districts_map

# FULL LA COUNTYWIDE POLICE REPORTING DISTRICTS MAPS
# Set bins for beats pop map
popbins <- c(0,1000,2000,5000,7500,10000, Inf)
poppal <- colorBin("viridis", beats$population, bins = popbins)
poplabel <- paste(sep = "<br>", beats$omega_name,beats$agency,prettyNum(beats$population, big.mark = ","))
# Create maps
la_beats_map <- leaflet(beats) %>%
  setView(-118.243, 34.052, zoom = 10) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "CartoDB.PositronOnlyLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~poppal(`population`))
la_beats_map

# Subdivide to just LAPD and LASD reporting districts 
beats2 <- beats %>% filter(beats$agency == "LAPD" | beats$agency == "LASD")
poplabel2 <- paste(sep = "<br>", beats2$omega_name,beats2$agency,prettyNum(beats2$population, big.mark = ","))
# Create a map of just PD/SD reporting districts
la_beats_map2 <- leaflet(beats2) %>%
  setView(-118.243, 34.052, zoom = 10) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "CartoDB.PositronOnlyLabels") %>%
  addPolygons(color = "white", popup = poplabel2, weight = 1, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~poppal(`population`))
la_beats_map2


