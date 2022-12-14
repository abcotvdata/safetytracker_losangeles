library(tidyverse)
library(sf)
library(tidyr)

# Get the map files we need
# districts <- readRDS("scripts/rds/la_county_police_districts.rds")

# read in socal_places file processed separately
# this features the place names and geography for 5 counties' places
# socal_places <- readRDS("scripts/rds/socal_places.rds")

# Read in the state crime file
california_annual <- read_csv("data/source/reference/california_crime_annual.csv", 
                                    col_types = cols(Year = col_character())) %>% janitor::clean_names()

# list of region's counties
counties <- c("Los Angeles County", "Orange County","Ventura County","San Bernardino County","Riverside County")
sheriffs <- c("Los Angeles Co. Sheriff's Department", "Orange Co. Sheriff's Department","Ventura Co. Sheriff's Department","San Bernardino Co. Sheriff's Department","Riverside Co. Sheriff's Department")

# Filter for crime incident counts in jurisdictions in our five counties 
socal_annual <- california_annual %>% filter(county %in% counties)

socal_murder <- socal_annual %>% select(year,county,ncic_code,homicide_sum) %>% spread(year,homicide_sum) %>% select(1,2,28:39)
socal_rape <- socal_annual %>% select(year,county,ncic_code,for_rape_sum) %>% spread(year,for_rape_sum) %>% select(1,2,28:39)
socal_assault <- socal_annual %>% select(year,county,ncic_code,agg_assault_sum) %>% spread(year,agg_assault_sum) %>% select(1,2,28:39)
socal_robbery <- socal_annual %>% select(year,county,ncic_code,robbery_sum) %>% spread(year,robbery_sum) %>% select(1,2,28:39)
socal_burglary <- socal_annual %>% select(year,county,ncic_code,burglary_sum) %>% spread(year,burglary_sum) %>% select(1,2,28:39)
socal_theft <- socal_annual %>% select(year,county,ncic_code,l_ttotal_sum) %>% spread(year,l_ttotal_sum) %>% select(1,2,28:39)
socal_autotheft <- socal_annual %>% select(year,county,ncic_code,vehicle_theft_sum) %>% spread(year,vehicle_theft_sum) %>% select(1,2,28:39)




# make quick tables that we can use for a quick simple map to be improved later
murders_places <- inner_join(socal_places %>% select(3:6),cal_crime_murder,by=c("place"="ncic_code"))
sexassaults_places <- inner_join(socal_places %>% select(3:6),cal_crime_rape,by=c("place"="ncic_code"))
assaults_places <- inner_join(socal_places %>% select(3:6),cal_crime_assault,by=c("place"="ncic_code"))
robberies_places <- inner_join(socal_places %>% select(3:6),cal_crime_robbery,by=c("place"="ncic_code"))
burglaries_places <- inner_join(socal_places %>% select(3:6),cal_crime_burglary,by=c("place"="ncic_code"))
thefts_places <- inner_join(socal_places %>% select(3:6),cal_crime_theft,by=c("place"="ncic_code"))
autothefts_places <- inner_join(socal_places %>% select(3:6),cal_crime_autotheft,by=c("place"="ncic_code"))

# MURDERS
# By Place add change columns for maps
# add zeros where there were no crimes tallied that year
murders_places[is.na(murders_places)] <- 0
# add 3-year totals and annualized average over three years
murders_places$total_prior3years <- murders_places$`2019` + murders_places$`2020` + murders_places$`2021`
murders_places$avg_prior3years <- round((murders_places$total_prior3years/3),1)
# now add the increases or change percentages vs prepandemic vs last decade
murders_places$inc_19to21 <- round(murders_places$`2021`/murders_places$`2019`*100-100,1)
murders_places$inc_10to21 <- round(murders_places$`2021`/murders_places$`2010`*100-100,1)
# add crime rates for each year
murders_places$rate19 <- round((murders_places$`2019`/murders_places$population)*100000,1)
murders_places$rate20 <- round((murders_places$`2020`/murders_places$population)*100000,1)
murders_places$rate21 <- round((murders_places$`2021`/murders_places$population)*100000,1)
murders_places$rate_prior3years <-round((murders_places$avg_prior3years/murders_places$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
murders_places <- murders_places %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
murders_places <- murders_places %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# SEXUAL ASSAULTS
# By Place add change columns for maps
# add zeros where there were no crimes tallied that year
sexassaults_places[is.na(sexassaults_places)] <- 0
# add 3-year totals and annualized average over three years
sexassaults_places$total_prior3years <- sexassaults_places$`2019` + sexassaults_places$`2020` + sexassaults_places$`2021`
sexassaults_places$avg_prior3years <- round((sexassaults_places$total_prior3years/3),1)
# now add the increases or change percentages vs prepandemic vs last decade
sexassaults_places$inc_19to21 <- round(sexassaults_places$`2021`/sexassaults_places$`2019`*100-100,1)
sexassaults_places$inc_10to21 <- round(sexassaults_places$`2021`/sexassaults_places$`2010`*100-100,1)
# add crime rates for each year
sexassaults_places$rate19 <- round((sexassaults_places$`2019`/sexassaults_places$population)*100000,1)
sexassaults_places$rate20 <- round((sexassaults_places$`2020`/sexassaults_places$population)*100000,1)
sexassaults_places$rate21 <- round((sexassaults_places$`2021`/sexassaults_places$population)*100000,1)
sexassaults_places$rate_prior3years <-round((sexassaults_places$avg_prior3years/sexassaults_places$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
sexassaults_places <- sexassaults_places %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
sexassaults_places <- sexassaults_places %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))


# ROBBERIES
# By Place add change columns for maps
# add zeros where there were no crimes tallied that year
robberies_places[is.na(robberies_places)] <- 0
# add 3-year totals and annualized average over three years
robberies_places$total_prior3years <- robberies_places$`2019` + robberies_places$`2020` + robberies_places$`2021`
robberies_places$avg_prior3years <- round((robberies_places$total_prior3years/3),1)
# now add the increases or change percentages vs prepandemic vs last decade
robberies_places$inc_19to21 <- round(robberies_places$`2021`/robberies_places$`2019`*100-100,1)
robberies_places$inc_10to21 <- round(robberies_places$`2021`/robberies_places$`2010`*100-100,1)
# add crime rates for each year
robberies_places$rate19 <- round((robberies_places$`2019`/robberies_places$population)*100000,1)
robberies_places$rate20 <- round((robberies_places$`2020`/robberies_places$population)*100000,1)
robberies_places$rate21 <- round((robberies_places$`2021`/robberies_places$population)*100000,1)
robberies_places$rate_prior3years <-round((robberies_places$avg_prior3years/robberies_places$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
robberies_places <- robberies_places %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
robberies_places <- robberies_places %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# ASSAULTS
# By Place add change columns for maps
# add zeros where there were no crimes tallied that year
assaults_places[is.na(assaults_places)] <- 0
# add 3-year totals and annualized average over three years
assaults_places$total_prior3years <- assaults_places$`2019` + assaults_places$`2020` + assaults_places$`2021`
assaults_places$avg_prior3years <- round((assaults_places$total_prior3years/3),1)
# now add the increases or change percentages vs prepandemic vs last decade
assaults_places$inc_19to21 <- round(assaults_places$`2021`/assaults_places$`2019`*100-100,1)
assaults_places$inc_10to21 <- round(assaults_places$`2021`/assaults_places$`2010`*100-100,1)
# add crime rates for each year
assaults_places$rate19 <- round((assaults_places$`2019`/assaults_places$population)*100000,1)
assaults_places$rate20 <- round((assaults_places$`2020`/assaults_places$population)*100000,1)
assaults_places$rate21 <- round((assaults_places$`2021`/assaults_places$population)*100000,1)
assaults_places$rate_prior3years <-round((assaults_places$avg_prior3years/assaults_places$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
assaults_places <- assaults_places %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
assaults_places <- assaults_places %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))


# BURGLARIES
# By Place add change columns for maps
# add zeros where there were no crimes tallied that year
burglaries_places[is.na(burglaries_places)] <- 0
# add 3-year totals and annualized average over three years
burglaries_places$total_prior3years <- burglaries_places$`2019` + burglaries_places$`2020` + burglaries_places$`2021`
burglaries_places$avg_prior3years <- round((burglaries_places$total_prior3years/3),1)
# now add the increases or change percentages vs prepandemic vs last decade
burglaries_places$inc_19to21 <- round(burglaries_places$`2021`/burglaries_places$`2019`*100-100,1)
burglaries_places$inc_10to21 <- round(burglaries_places$`2021`/burglaries_places$`2010`*100-100,1)
# add crime rates for each year
burglaries_places$rate19 <- round((burglaries_places$`2019`/burglaries_places$population)*100000,1)
burglaries_places$rate20 <- round((burglaries_places$`2020`/burglaries_places$population)*100000,1)
burglaries_places$rate21 <- round((burglaries_places$`2021`/burglaries_places$population)*100000,1)
burglaries_places$rate_prior3years <-round((burglaries_places$avg_prior3years/burglaries_places$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
burglaries_places <- burglaries_places %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
burglaries_places <- burglaries_places %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))


# VEHICLE THEFTS
# By Place add change columns for maps
# add zeros where there were no crimes tallied that year
autothefts_places[is.na(autothefts_places)] <- 0
# add 3-year totals and annualized average over three years
autothefts_places$total_prior3years <- autothefts_places$`2019` + autothefts_places$`2020` + autothefts_places$`2021`
autothefts_places$avg_prior3years <- round((autothefts_places$total_prior3years/3),1)
# now add the increases or change percentages vs prepandemic vs last decade
autothefts_places$inc_19to21 <- round(autothefts_places$`2021`/autothefts_places$`2019`*100-100,1)
autothefts_places$inc_10to21 <- round(autothefts_places$`2021`/autothefts_places$`2010`*100-100,1)
# add crime rates for each year
autothefts_places$rate19 <- round((autothefts_places$`2019`/autothefts_places$population)*100000,1)
autothefts_places$rate20 <- round((autothefts_places$`2020`/autothefts_places$population)*100000,1)
autothefts_places$rate21 <- round((autothefts_places$`2021`/autothefts_places$population)*100000,1)
autothefts_places$rate_prior3years <-round((autothefts_places$avg_prior3years/autothefts_places$population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
autothefts_places <- autothefts_places %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
autothefts_places <- autothefts_places %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))


# THEFTS
# By Place add change columns for maps
# add zeros where there were no crimes tallied that year
thefts_places[is.na(thefts_places)] <- 0
# add 3-year totals and annualized average over three years
thefts_places$total_prior3years <- thefts_places$`2019` + thefts_places$`2020` + thefts_places$`2021`
thefts_places$avg_prior3years <- round((thefts_places$total_prior3years/3),1)
# now add the increases or change percentages vs prepandemic vs last decade
thefts_places$inc_19to21 <- round(thefts_places$`2021`/thefts_places$`2019`*100-100,1)
thefts_places$inc_10to21 <- round(thefts_places$`2021`/thefts_places$`2010`*100-100,1)
# add crime rates for each year
thefts_places$rate19 <- round((thefts_places$`2019`/thefts_places$population)*100000,1)
thefts_places$rate20 <- round((thefts_places$`2020`/thefts_places$population)*100000,1)
thefts_places$rate21 <- round((thefts_places$`2021`/thefts_places$population)*100000,1)
thefts_places$rate_prior3years <-round((thefts_places$avg_prior3years/thefts_places$population)*100000,1)

## NOW GATHER/PROCESS SHERIFF RURAL/UNINCORPORATED AREAS
# gather the figures for the unincorporated/rural areas covered by sheriffs departments
sheriffs_crime_murder <- cal_crime %>% select(year,county,ncic_code,homicide_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,homicide_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_crime_rape <- cal_crime %>% select(year,county,ncic_code,for_rape_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,for_rape_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_crime_assault <- cal_crime %>% select(year,county,ncic_code,agg_assault_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,agg_assault_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_crime_robbery <- cal_crime %>% select(year,county,ncic_code,robbery_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,robbery_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_crime_burglary <- cal_crime %>% select(year,county,ncic_code,burglary_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,burglary_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_crime_theft <- cal_crime %>% select(year,county,ncic_code,l_ttotal_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,l_ttotal_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_crime_autotheft <- cal_crime %>% select(year,county,ncic_code,vehicle_theft_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,vehicle_theft_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")

# gather the figures for the unincorporated/rural areas covered by sheriffs departments
sheriffs_clearance_murder <- cal_crime %>% select(year,county,ncic_code,homicide_clr_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,homicide_clr_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_clearance_rape <- cal_crime %>% select(year,county,ncic_code,for_rape_clr_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,for_rape_clr_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_clearance_assault <- cal_crime %>% select(year,county,ncic_code,agg_assault_clr_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,agg_assault_clr_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_clearance_robbery <- cal_crime %>% select(year,county,ncic_code,robbery_clr_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,robbery_clr_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_clearance_burglary <- cal_crime %>% select(year,county,ncic_code,burglary_clr_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,burglary_clr_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_clearance_theft <- cal_crime %>% select(year,county,ncic_code,l_ttotal_clr_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,l_ttotal_clr_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")
sheriffs_clearance_autotheft <- cal_crime %>% select(year,county,ncic_code,vehicle_theft_clr_sum) %>% filter(ncic_code %in% sheriffs) %>% spread(year,vehicle_theft_clr_sum) %>% select(1,2,18:39) %>% rename("place"="ncic_code")



## Clearance Rates By Agency
# Filter for crime incident counts in jurisdictions in our five counties 
cal_clearance_murder <- cal_crime %>% select(year,county,ncic_code,homicide_clr_sum) %>% spread(year,homicide_clr_sum) %>% select(1,2,28:39)
cal_clearance_rape <- cal_crime %>% select(year,county,ncic_code,for_rape_clr_sum) %>% spread(year,for_rape_clr_sum) %>% select(1,2,28:39)
cal_clearance_assault <- cal_crime %>% select(year,county,ncic_code,agg_assault_clr_sum) %>% spread(year,agg_assault_clr_sum) %>% select(1,2,28:39)
cal_clearance_robbery <- cal_crime %>% select(year,county,ncic_code,robbery_clr_sum) %>% spread(year,robbery_clr_sum) %>% select(1,2,28:39)
cal_clearance_burglary <- cal_crime %>% select(year,county,ncic_code,burglary_clr_sum) %>% spread(year,burglary_clr_sum) %>% select(1,2,28:39)
cal_clearance_theft <- cal_crime %>% select(year,county,ncic_code,l_ttotal_clr_sum) %>% spread(year,l_ttotal_clr_sum) %>% select(1,2,28:39)
cal_clearance_autotheft <- cal_crime %>% select(year,county,ncic_code,vehicle_theft_clr_sum) %>% spread(year,vehicle_theft_clr_sum) %>% select(1,2,28:39)

# make quick tables that we can use for a quick simple map to be improved later
murders_clearance_places <- inner_join(socal_places %>% st_drop_geometry() %>% select(4),cal_clearance_murder,by=c("place"="ncic_code")) %>% left_join(murders_places %>% st_drop_geometry() %>% select(2,15:26),by="place") %>% mutate(crime="Murder")
sexassaults_clearance_places <- inner_join(socal_places %>% st_drop_geometry() %>% select(4),cal_clearance_rape,by=c("place"="ncic_code")) %>% left_join(sexassaults_places %>% st_drop_geometry() %>% select(2,15:26),by="place") %>% mutate(crime="Sexual Assault")
assaults_clearance_places <- inner_join(socal_places %>% st_drop_geometry() %>% select(4),cal_clearance_assault,by=c("place"="ncic_code")) %>% left_join(assaults_places %>% st_drop_geometry() %>% select(2,15:26),by="place") %>% mutate(crime="Aggravated Assault")
robberies_clearance_places <- inner_join(socal_places %>% st_drop_geometry() %>% select(4),cal_clearance_robbery,by=c("place"="ncic_code")) %>% left_join(robberies_places %>% st_drop_geometry() %>% select(2,15:26),by="place") %>% mutate(crime="Robbery")
burglaries_clearance_places <- inner_join(socal_places %>% st_drop_geometry() %>% select(4),cal_clearance_burglary,by=c("place"="ncic_code")) %>% left_join(burglaries_places %>% st_drop_geometry() %>% select(2,15:26),by="place") %>% mutate(crime="Burglary")
thefts_clearance_places <- inner_join(socal_places %>% st_drop_geometry() %>% select(4),cal_clearance_theft,by=c("place"="ncic_code")) %>% left_join(thefts_places %>% st_drop_geometry() %>% select(2,15:26),by="place") %>% mutate(crime="Theft")
autothefts_clearance_places <- inner_join(socal_places %>% st_drop_geometry() %>% select(4),cal_clearance_autotheft,by=c("place"="ncic_code")) %>% left_join(autothefts_places %>% st_drop_geometry() %>% select(2,15:26),by="place") %>% mutate(crime="Vehicle Theft")

colnames(murders_clearance_places) <- sub(".x", "clr", colnames(murders_clearance_places))
colnames(sexassaults_clearance_places) <- sub(".x", "clr", colnames(sexassaults_clearance_places))
colnames(assaults_clearance_places) <- sub(".x", "clr", colnames(assaults_clearance_places))
colnames(robberies_clearance_places) <- sub(".x", "clr", colnames(robberies_clearance_places))
colnames(burglaries_clearance_places) <- sub(".x", "clr", colnames(burglaries_clearance_places))
colnames(thefts_clearance_places) <- sub(".x", "clr", colnames(thefts_clearance_places))
colnames(autothefts_clearance_places) <- sub(".x", "clr", colnames(autothefts_clearance_places))
colnames(murders_clearance_places) <- sub(".y", "total", colnames(murders_clearance_places))
colnames(sexassaults_clearance_places) <- sub(".y", "total", colnames(sexassaults_clearance_places))
colnames(assaults_clearance_places) <- sub(".y", "total", colnames(assaults_clearance_places))
colnames(robberies_clearance_places) <- sub(".y", "total", colnames(robberies_clearance_places))
colnames(burglaries_clearance_places) <- sub(".y", "total", colnames(burglaries_clearance_places))
colnames(thefts_clearance_places) <- sub(".y", "total", colnames(thefts_clearance_places))
colnames(autothefts_clearance_places) <- sub(".y", "total", colnames(autothefts_clearance_places))
colnames(murders_clearance_places) <- sub("countotal", "county", colnames(murders_clearance_places))
colnames(sexassaults_clearance_places) <- sub("countotal", "county", colnames(sexassaults_clearance_places))
colnames(assaults_clearance_places) <- sub("countotal", "county", colnames(assaults_clearance_places))
colnames(robberies_clearance_places) <- sub("countotal", "county", colnames(robberies_clearance_places))
colnames(burglaries_clearance_places) <- sub("countotal", "county", colnames(burglaries_clearance_places))
colnames(thefts_clearance_places) <- sub("countotal", "county", colnames(thefts_clearance_places))
colnames(autothefts_clearance_places) <- sub("countotal", "county", colnames(autothefts_clearance_places))

clearance_places <- rbind(murders_clearance_places,sexassaults_clearance_places,assaults_clearance_places,robberies_clearance_places,burglaries_clearance_places,thefts_clearance_places,autothefts_clearance_places)

clearance_places$`2010clr_pct` <- round(clearance_places$`2010clr`/clearance_places$`2010total`,3)*100
clearance_places$`2011clr_pct` <- round(clearance_places$`2011clr`/clearance_places$`2011total`,3)*100
clearance_places$`2012clr_pct` <- round(clearance_places$`2012clr`/clearance_places$`2012total`,3)*100
clearance_places$`2013clr_pct` <- round(clearance_places$`2013clr`/clearance_places$`2013total`,3)*100
clearance_places$`2014clr_pct` <- round(clearance_places$`2014clr`/clearance_places$`2014total`,3)*100
clearance_places$`2015clr_pct` <- round(clearance_places$`2015clr`/clearance_places$`2015total`,3)*100
clearance_places$`2016clr_pct` <- round(clearance_places$`2016clr`/clearance_places$`2016total`,3)*100
clearance_places$`2017clr_pct` <- round(clearance_places$`2017clr`/clearance_places$`2017total`,3)*100
clearance_places$`2018clr_pct` <- round(clearance_places$`2018clr`/clearance_places$`2018total`,3)*100
clearance_places$`2019clr_pct` <- round(clearance_places$`2019clr`/clearance_places$`2019total`,3)*100
clearance_places$`2020clr_pct` <- round(clearance_places$`2020clr`/clearance_places$`2020total`,3)*100
clearance_places$`2021clr_pct` <- round(clearance_places$`2021clr`/clearance_places$`2021total`,3)*100

# for map/table making purposes, changing Inf and NaN in calc fields to NA
clearance_places <- clearance_places %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
clearance_places <- clearance_places %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

write_csv(clearance_places,"data/output/clearances_byagency_20102021.csv")
clearance_places %>% filter(county=="Los Angeles County") %>% write_csv(clearance_places,"data/output/clearances_losangeles.csv")
