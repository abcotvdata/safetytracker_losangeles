library(tidyverse)
library(tidycensus)
library(leaflet)
library(leaflet.providers)
library(leaflet.extras)
library(sf)

# Reading in detailed LA Co. police reporting districts
beats <- st_read("data/source/geo/Law_Enforcement_Reporting_Districts.geojson") %>% st_transform(3857) %>% janitor::clean_names()

# Fill in some blank names/commtypes in the raw beats data file
beats$commtype <- ifelse(beats$st_name=="Azusa" & beats$commtype=="Unincorporated","City",beats$commtype)
beats$commtype <- ifelse(beats$st_name=="Avalon" & beats$commtype==" ","Unincorporated",beats$commtype)
beats$commtype <- ifelse(beats$st_name=="Santa Monica" & beats$commtype==" ","City",beats$commtype)
beats$commtype <- ifelse(beats$st_name=="Redondo Beach" & beats$commtype==" ","City",beats$commtype)
beats$commtype <- ifelse(beats$st_name=="Long Beach" & beats$commtype==" ","City",beats$commtype)
beats$name <- ifelse(beats$st_name=="Santa Monica" & beats$name==" ","Santa Monica",beats$name)
beats$name <- ifelse(beats$st_name=="Redondo Beach" & beats$name==" ","Redondo Beach",beats$name)
beats$name <- ifelse(beats$st_name=="Long Beach" & beats$name==" ","Long Beach",beats$name)
beats$name <- ifelse(beats$st_name=="Avalon" & beats$name==" ","Avalon",beats$name)
beats$name <- ifelse(beats$name=="City of Commerce","Commerce",beats$name)
beats <- beats %>% filter(omega_label != "LASD 0593")

# Adding a column to help merge these into districts 
beats$place_name <- ifelse(beats$commtype=="Unincorporated","Unincorporated",beats$name)

# Create LAPD and Sheriff districts map file
districts <- beats %>%
  group_by(agency,s_type,st_name,commtype,place_name) %>%
  summarise(geometry = sf::st_union(geometry)) %>%
  ungroup()

# Standardize some district names to match data files
districts <- districts %>% rename("district"="st_name")
districts$district <- gsub(" Division", "", districts$district)
districts$district <- gsub(" / ", "/", districts$district)

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
districts$population <- round(districts$population,-2)

# la_districts <- districts %>% st_transform(4269)
la_districts <- districts %>% 
  st_transform(4269) %>% 
  st_make_valid
# rm(districts)

# saving a clean geojson and separate RDS for use in tracker
file.remove("data/output/geo/la_county_police_districts.geojson")
st_write(la_districts,"data/output/geo/la_county_police_districts.geojson")
saveRDS(la_districts,"scripts/rds/la_county_police_districts.rds")


# District Map for proofing/testing
# Set bins for beats pop map
popbins <- c(0,50000,100000,125000,150000,200000,Inf)
poppal <- colorBin("viridis", la_districts$population, bins = popbins)
poplabel <- paste(sep = "<br>", la_districts$agency,la_districts$district,la_districts$s_type,la_districts$commtype,la_districts$place_name,prettyNum(la_districts$population, big.mark = ","))
# Make map
la_county_districts_map <- leaflet(la_districts) %>%
  setView(-118.243, 34.052, zoom = 10) %>% 
  addProviderTiles(provider = "Esri.WorldImagery") %>%
  addProviderTiles(provider = "CartoDB.PositronOnlyLabels") %>%
  addPolygons(color = "white", popup = poplabel, weight = 2, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.3,
              fillColor = ~poppal(`population`))
la_county_districts_map

