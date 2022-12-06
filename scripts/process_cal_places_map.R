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


# list of region's counties
counties <- c("Los Angeles County", "Orange County","Ventura County","San Bernardino County","Riverside County")
# filter anything not in the five counties
cal_crime <- california_crime_annual %>% filter(County %in% counties) %>% janitor::clean_names()
cal_crime_murder <- cal_crime %>% select(year,county,ncic_code,homicide_sum) %>% spread(year,homicide_sum) %>% select(1,2,18:39)
cal_crime_rape <- cal_crime %>% select(year,county,ncic_code,for_rape_sum) %>% spread(year,for_rape_sum) %>% select(1,2,18:39)
cal_crime_assault <- cal_crime %>% select(year,county,ncic_code,agg_assault_sum) %>% spread(year,agg_assault_sum) %>% select(1,2,18:39)
cal_crime_robbery <- cal_crime %>% select(year,county,ncic_code,robbery_sum) %>% spread(year,robbery_sum) %>% select(1,2,18:39)
cal_crime_burglary <- cal_crime %>% select(year,county,ncic_code,burglary_sum) %>% spread(year,burglary_sum) %>% select(1,2,18:39)
cal_crime_theft <- cal_crime %>% select(year,county,ncic_code,l_ttotal_sum) %>% spread(year,l_ttotal_sum) %>% select(1,2,18:39)
cal_crime_autotheft <- cal_crime %>% select(year,county,ncic_code,vehicle_theft_sum) %>% spread(year,vehicle_theft_sum) %>% select(1,2,18:39)

# make quick tables that we can use for a quick simple map to be improved later
murders_region <- inner_join(cal_places,cal_crime_murder,by=c("place"="ncic_code")) %>% rename("total19"="2019","total20"="2020","total21"="2021") %>% select(5,6,3,7:28)
sexassaults_region <- inner_join(cal_places,cal_crime_rape,by=c("place"="ncic_code")) %>% rename("total19"="2019","total20"="2020","total21"="2021") %>% select(5,6,3,7:28)
assaults_region <- inner_join(cal_places,cal_crime_assault,by=c("place"="ncic_code")) %>% rename("total19"="2019","total20"="2020","total21"="2021") %>% select(5,6,3,7:28)
robberies_region <- inner_join(cal_places,cal_crime_robbery,by=c("place"="ncic_code")) %>% rename("total19"="2019","total20"="2020","total21"="2021") %>% select(5,6,3,7:28)
burglaries_region <- inner_join(cal_places,cal_crime_burglary,by=c("place"="ncic_code")) %>% rename("total19"="2019","total20"="2020","total21"="2021") %>% select(5,6,3,7:28)
thefts_region <- inner_join(cal_places,cal_crime_theft,by=c("place"="ncic_code")) %>% rename("total19"="2019","total20"="2020","total21"="2021") %>% select(5,6,3,7:28)
autothefts_region <- inner_join(cal_places,cal_crime_autotheft,by=c("place"="ncic_code")) %>% rename("total19"="2019","total20"="2020","total21"="2021") %>% select(5,6,3,7:28)

# for murders_region
# add zeros where there were no crimes tallied that year
murders_region[is.na(murders_region)] <- 0
# add 3-year totals and annualized averages
murders_region$total_prior3years <- murders_region$total19+
  murders_region$total20+
  murders_region$total21
murders_region$avg_prior3years <- round(((murders_region$total19+
                                            murders_region$total20+
                                            murders_region$total21)/3),1)
# now add the increases or change percentages
murders_region$inc_19to21 <- round(murders_region$total21/murders_region$total19*100-100,1)
# murders_region$inc_19tolast12 <- round(murders_region$last12mos/murders_region$total19*100-100,1)
# murders_region$inc_21tolast12 <- round(murders_region$last12mos/murders_region$total21*100-100,1)
# murders_region$inc_prior3yearavgtolast12 <- round((murders_region$last12mos/murders_region$avg_prior3years)*100-100,0)
# add crime rates for each year
murders_region$rate19 <- round((murders_region$total19/murders_region$population)*100000,1)
murders_region$rate20 <- round((murders_region$total20/murders_region$population)*100000,1)
murders_region$rate21 <- round((murders_region$total21/murders_region$population)*100000,1)
# murders_region$rate_last12 <- round((murders_region$last12mos/murders_region$population)*100000,1)
murders_region$rate_prior3years <- 
  round((murders_region$avg_prior3years/murders_region$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
murders_region <- murders_region %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
murders_region <- murders_region %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))


# for sex assaults
# add zeros where there were no crimes tallied that year
sexassaults_region[is.na(sexassaults_region)] <- 0
# add 3-year totals and annualized averages
sexassaults_region$total_prior3years <- sexassaults_region$total19+
  sexassaults_region$total20+
  sexassaults_region$total21
sexassaults_region$avg_prior3years <- round(((sexassaults_region$total19+
                                            sexassaults_region$total20+
                                            sexassaults_region$total21)/3),1)
# now add the increases or change percentages
sexassaults_region$inc_19to21 <- round(sexassaults_region$total21/sexassaults_region$total19*100-100,1)
# sexassaults_region$inc_19tolast12 <- round(sexassaults_region$last12mos/sexassaults_region$total19*100-100,1)
# sexassaults_region$inc_21tolast12 <- round(sexassaults_region$last12mos/sexassaults_region$total21*100-100,1)
# sexassaults_region$inc_prior3yearavgtolast12 <- round((sexassaults_region$last12mos/sexassaults_region$avg_prior3years)*100-100,0)
# add crime rates for each year
sexassaults_region$rate19 <- round((sexassaults_region$total19/sexassaults_region$population)*100000,1)
sexassaults_region$rate20 <- round((sexassaults_region$total20/sexassaults_region$population)*100000,1)
sexassaults_region$rate21 <- round((sexassaults_region$total21/sexassaults_region$population)*100000,1)
# sexassaults_region$rate_last12 <- round((sexassaults_region$last12mos/sexassaults_region$population)*100000,1)
sexassaults_region$rate_prior3years <- 
  round((sexassaults_region$avg_prior3years/sexassaults_region$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
sexassaults_region <- sexassaults_region %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
sexassaults_region <- sexassaults_region %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))




# for murders_region
# add zeros where there were no crimes tallied that year
assaults_region[is.na(assaults_region)] <- 0
# add 3-year totals and annualized averages
assaults_region$total_prior3years <- assaults_region$total19+
  assaults_region$total20+
  assaults_region$total21
assaults_region$avg_prior3years <- round(((assaults_region$total19+
                                            assaults_region$total20+
                                            assaults_region$total21)/3),1)
# now add the increases or change percentages
assaults_region$inc_19to21 <- round(assaults_region$total21/assaults_region$total19*100-100,1)
# assaults_region$inc_19tolast12 <- round(assaults_region$last12mos/assaults_region$total19*100-100,1)
# assaults_region$inc_21tolast12 <- round(assaults_region$last12mos/assaults_region$total21*100-100,1)
# assaults_region$inc_prior3yearavgtolast12 <- round((assaults_region$last12mos/assaults_region$avg_prior3years)*100-100,0)
# add crime rates for each year
assaults_region$rate19 <- round((assaults_region$total19/assaults_region$population)*100000,1)
assaults_region$rate20 <- round((assaults_region$total20/assaults_region$population)*100000,1)
assaults_region$rate21 <- round((assaults_region$total21/assaults_region$population)*100000,1)
# assaults_region$rate_last12 <- round((assaults_region$last12mos/assaults_region$population)*100000,1)
assaults_region$rate_prior3years <- 
  round((assaults_region$avg_prior3years/assaults_region$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
assaults_region <- assaults_region %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
assaults_region <- assaults_region %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# city versions
# saveRDS(regionwide_crime,"scripts/rds/regionwide_crime.rds")

saveRDS(murders_region,"scripts/rds/murders_region.rds")
saveRDS(sexassaults_region,"scripts/rds/sexassaults_region.rds")
saveRDS(robberies_region,"scripts/rds/robberies_region.rds")
saveRDS(assaults_region,"scripts/rds/assaults_region.rds")
saveRDS(burglaries_region,"scripts/rds/burglaries_region.rds")
saveRDS(thefts_region,"scripts/rds/thefts_region.rds")
saveRDS(autothefts_region,"scripts/rds/autothefts_region.rds")




# for robbery_region
# add zeros where there were no crimes tallied that year
robberies_region[is.na(robberies_region)] <- 0
# add 3-year totals and annualized averages
robberies_region$total_prior3years <- robberies_region$total19+
  robberies_region$total20+
  robberies_region$total21
robberies_region$avg_prior3years <- round(((robberies_region$total19+
                                            robberies_region$total20+
                                            robberies_region$total21)/3),1)
# now add the increases or change percentages
robberies_region$inc_19to21 <- round(robberies_region$total21/robberies_region$total19*100-100,1)
# robberies_region$inc_19tolast12 <- round(robberies_region$last12mos/robberies_region$total19*100-100,1)
# robberies_region$inc_21tolast12 <- round(robberies_region$last12mos/robberies_region$total21*100-100,1)
# robberies_region$inc_prior3yearavgtolast12 <- round((robberies_region$last12mos/robberies_region$avg_prior3years)*100-100,0)
# add crime rates for each year
robberies_region$rate19 <- round((robberies_region$total19/robberies_region$population)*100000,1)
robberies_region$rate20 <- round((robberies_region$total20/robberies_region$population)*100000,1)
robberies_region$rate21 <- round((robberies_region$total21/robberies_region$population)*100000,1)
# robberies_region$rate_last12 <- round((robberies_region$last12mos/robberies_region$population)*100000,1)
robberies_region$rate_prior3years <- 
  round((robberies_region$avg_prior3years/robberies_region$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
robberies_region <- robberies_region %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
robberies_region <- robberies_region %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))





# for burglaries_region
# add zeros where there were no crimes tallied that year
burglaries_region[is.na(burglaries_region)] <- 0
# add 3-year totals and annualized averages
burglaries_region$total_prior3years <- burglaries_region$total19+
  burglaries_region$total20+
  burglaries_region$total21
burglaries_region$avg_prior3years <- round(((burglaries_region$total19+
                                            burglaries_region$total20+
                                            burglaries_region$total21)/3),1)
# now add the increases or change percentages
burglaries_region$inc_19to21 <- round(burglaries_region$total21/burglaries_region$total19*100-100,1)
# burglaries_region$inc_19tolast12 <- round(burglaries_region$last12mos/burglaries_region$total19*100-100,1)
# burglaries_region$inc_21tolast12 <- round(burglaries_region$last12mos/burglaries_region$total21*100-100,1)
# burglaries_region$inc_prior3yearavgtolast12 <- round((burglaries_region$last12mos/burglaries_region$avg_prior3years)*100-100,0)
# add crime rates for each year
burglaries_region$rate19 <- round((burglaries_region$total19/burglaries_region$population)*100000,1)
burglaries_region$rate20 <- round((burglaries_region$total20/burglaries_region$population)*100000,1)
burglaries_region$rate21 <- round((burglaries_region$total21/burglaries_region$population)*100000,1)
# burglaries_region$rate_last12 <- round((burglaries_region$last12mos/burglaries_region$population)*100000,1)
burglaries_region$rate_prior3years <- 
  round((burglaries_region$avg_prior3years/burglaries_region$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
burglaries_region <- burglaries_region %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
burglaries_region <- burglaries_region %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))





# for thefts_region
# add zeros where there were no crimes tallied that year
thefts_region[is.na(thefts_region)] <- 0
# add 3-year totals and annualized averages
thefts_region$total_prior3years <- thefts_region$total19+
  thefts_region$total20+
  thefts_region$total21
thefts_region$avg_prior3years <- round(((thefts_region$total19+
                                            thefts_region$total20+
                                            thefts_region$total21)/3),1)
# now add the increases or change percentages
thefts_region$inc_19to21 <- round(thefts_region$total21/thefts_region$total19*100-100,1)
# thefts_region$inc_19tolast12 <- round(thefts_region$last12mos/thefts_region$total19*100-100,1)
# thefts_region$inc_21tolast12 <- round(thefts_region$last12mos/thefts_region$total21*100-100,1)
# thefts_region$inc_prior3yearavgtolast12 <- round((thefts_region$last12mos/thefts_region$avg_prior3years)*100-100,0)
# add crime rates for each year
thefts_region$rate19 <- round((thefts_region$total19/thefts_region$population)*100000,1)
thefts_region$rate20 <- round((thefts_region$total20/thefts_region$population)*100000,1)
thefts_region$rate21 <- round((thefts_region$total21/thefts_region$population)*100000,1)
# thefts_region$rate_last12 <- round((thefts_region$last12mos/thefts_region$population)*100000,1)
thefts_region$rate_prior3years <- 
  round((thefts_region$avg_prior3years/thefts_region$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
thefts_region <- thefts_region %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
thefts_region <- thefts_region %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))



# for autothefts_region
# add zeros where there were no crimes tallied that year
autothefts_region[is.na(autothefts_region)] <- 0
# add 3-year totals and annualized averages
autothefts_region$total_prior3years <- autothefts_region$total19+
  autothefts_region$total20+
  autothefts_region$total21
autothefts_region$avg_prior3years <- round(((autothefts_region$total19+
                                               autothefts_region$total20+
                                               autothefts_region$total21)/3),1)
# now add the increases or change percentages
autothefts_region$inc_19to21 <- round(autothefts_region$total21/autothefts_region$total19*100-100,1)
# autothefts_region$inc_19tolast12 <- round(autothefts_region$last12mos/autothefts_region$total19*100-100,1)
# autothefts_region$inc_21tolast12 <- round(autothefts_region$last12mos/autothefts_region$total21*100-100,1)
# autothefts_region$inc_prior3yearavgtolast12 <- round((autothefts_region$last12mos/autothefts_region$avg_prior3years)*100-100,0)
# add crime rates for each year
autothefts_region$rate19 <- round((autothefts_region$total19/autothefts_region$population)*100000,1)
autothefts_region$rate20 <- round((autothefts_region$total20/autothefts_region$population)*100000,1)
autothefts_region$rate21 <- round((autothefts_region$total21/autothefts_region$population)*100000,1)
# autothefts_region$rate_last12 <- round((autothefts_region$last12mos/autothefts_region$population)*100000,1)
autothefts_region$rate_prior3years <- 
  round((autothefts_region$avg_prior3years/autothefts_region$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
autothefts_region <- autothefts_region %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
autothefts_region <- autothefts_region %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))


cal_crime21 <- california_crime_annual %>% filter(Year==2021) %>% filter(County %in% counties) 
cal_crime19 <- california_crime_annual %>% filter(Year==2019) %>% filter(County %in% counties) 
# make totals file for whole region
murders21 <- sum(cal_crime21$Homicide_sum)
murders19 <- sum(cal_crime19$Homicide_sum)




# create a quick long-term annual table
district_yearly <- district_crime %>% select(1,2,4:7,9) %>% st_drop_geometry()
write_csv(district_yearly,"data/output/yearly/district_yearly.csv")











# this makes the archive year tables for the Los Angeles area
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
