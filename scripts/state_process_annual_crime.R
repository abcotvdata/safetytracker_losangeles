library(tidyverse)
library(sf)
library(tidyr)

# May need to load the right map file here
# socal_places <- readRDS("scripts/rds/socal_places.rds")

# Import the annual statewide crime and clearance file from California Department of Justice
# This will release every summer and need replacing, followed by edits throughout this script
california_annual <- read_csv("data/source/reference/california_crime_annual_2022.csv", 
                                    col_types = cols(Year = col_character())) %>% janitor::clean_names()

# list of region's counties and sheriff's departments; ensure sheriff's department names match style in DOJ file
counties <- c("Los Angeles County", "Orange County","Ventura County","San Bernardino County","Riverside County")
sheriffs <- c("Los Angeles Co. Sheriff's Department", "Orange Co. Sheriff's Department","Ventura Co. Sheriff's Department","San Bernardino Co. Sheriff's Department","Riverside Co. Sheriff's Department")

# Filter the statewide file for incident counts in jurisdictions in our five counties 
socal_annual <- california_annual %>% filter(county %in% counties)

# Export to rds, storing so we have for geography processing
saveRDS(socal_annual,"scripts/rds/socal_annual.rds")

# Create a baseline count file for so cal agencies, by crime category, for charts
socal_murder <- socal_annual %>% select(year,county,ncic_code,homicide_sum) %>% spread(year,homicide_sum) %>% select(1,2,28:40) %>% rename("place"="ncic_code")
socal_sexassault <- socal_annual %>% select(year,county,ncic_code,for_rape_sum) %>% spread(year,for_rape_sum) %>% select(1,2,28:40) %>% rename("place"="ncic_code")
socal_assault <- socal_annual %>% select(year,county,ncic_code,agg_assault_sum) %>% spread(year,agg_assault_sum) %>% select(1,2,28:40) %>% rename("place"="ncic_code")
socal_robbery <- socal_annual %>% select(year,county,ncic_code,robbery_sum) %>% spread(year,robbery_sum) %>% select(1,2,28:40) %>% rename("place"="ncic_code")
socal_burglary <- socal_annual %>% select(year,county,ncic_code,burglary_sum) %>% spread(year,burglary_sum) %>% select(1,2,28:40) %>% rename("place"="ncic_code")
socal_theft <- socal_annual %>% select(year,county,ncic_code,l_ttotal_sum) %>% spread(year,l_ttotal_sum) %>% select(1,2,28:40) %>% rename("place"="ncic_code")
socal_autotheft <- socal_annual %>% select(year,county,ncic_code,vehicle_theft_sum) %>% spread(year,vehicle_theft_sum) %>% select(1,2,28:40) %>% rename("place"="ncic_code")

# Before we make any more changes for maps, extract comparable countywide totals in same format for charts and tracker text
countywide_murder <- socal_murder %>% group_by(county) %>% summarise(total22=sum(`2022`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_sexassault <- socal_sexassault %>% group_by(county) %>% summarise(total22=sum(`2022`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_assault <- socal_assault %>% group_by(county) %>% summarise(total22=sum(`2022`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_robbery <- socal_robbery %>% group_by(county) %>% summarise(total22=sum(`2022`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_burglary <- socal_burglary %>% group_by(county) %>% summarise(total22=sum(`2022`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_theft <- socal_theft %>% group_by(county) %>% summarise(total22=sum(`2022`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))
countywide_autotheft <- socal_autotheft %>% group_by(county) %>% summarise(total22=sum(`2022`,na.rm=TRUE),total19=sum(`2019`,na.rm=TRUE),total10=sum(`2010`,na.rm=TRUE))

# Create a list of agencies that contract for police services with the sheriff's department
# They're listed within the DOJ data file separately as separate police agencies
sheriff_contracts <- c("Avalon","Carson","Lynwood","Cerritos","Compton",
                       "La Canada Flintridge","Commerce","Cudahy","Maywood",
                       "Industry","La Habra Heights","La Puente","Artesia",
                       "Bellflower","Hawaiian Gardens","Lakewood",
                       "Paramount","Lancaster","Lomita","Rancho Palos Verdes",
                       "Rolling Hills","Rolling Hills Estates","Agoura Hills",
                       "Calabasas","Hidden Hills","Malibu","Westlake Village",
                       "La Mirada","Norwalk","Palmdale","Pico Rivera",
                       "San Dimas","Santa Clarita","Lawndale","Bradbury",
                       "Duarte","Rosemead","South El Monte","Temple City",
                       "Diamond Bar","Walnut","West Hollywood")

## Now that we have totals for the local agencies, we're going to add in similarly-formatted totals for LAPD & LASD districts
# These are previously processed elsewhere and then added here
socal_murder <- bind_rows(socal_murder %>% filter(!place %in% sheriff_contracts),lapd_murder,lasheriff_murder)
socal_sexassault <- bind_rows(socal_sexassault %>% filter(!place %in% sheriff_contracts),lapd_sexassault,lasheriff_sexassault)
socal_assault <- bind_rows(socal_assault %>% filter(!place %in% sheriff_contracts),lapd_assault,lasheriff_assault)
socal_robbery <- bind_rows(socal_robbery %>% filter(!place %in% sheriff_contracts),lapd_robbery,lasheriff_robbery)
socal_burglary <- bind_rows(socal_burglary %>% filter(!place %in% sheriff_contracts),lapd_burglary,lasheriff_burglary)
socal_theft <- bind_rows(socal_theft %>% filter(!place %in% sheriff_contracts),lapd_theft,lasheriff_theft)
socal_autotheft <- bind_rows(socal_autotheft %>% filter(!place %in% sheriff_contracts),lapd_autotheft,lasheriff_autotheft)

# Joining geography to make base tables for each crime category
socal_murder <- inner_join(police_map %>% select(3:5),socal_murder,by="place")
socal_sexassault <- inner_join(police_map %>% select(3:5),socal_sexassault,by="place")
socal_assault <- inner_join(police_map %>% select(3:5),socal_assault,by="place")
socal_robbery <- inner_join(police_map %>% select(3:5),socal_robbery,by="place")
socal_burglary <- inner_join(police_map %>% select(3:5),socal_burglary,by="place")
socal_theft <- inner_join(police_map %>% select(3:5),socal_theft,by="place")
socal_autotheft <- inner_join(police_map %>% select(3:5),socal_autotheft,by="place")

## Crime category by category, we're going to build out the year by year file for every jurisdiction

# MURDERS
# Adding 3-year totals and annualized average over three years
socal_murder$total_prior4years <- socal_murder$`2019` + socal_murder$`2020` + socal_murder$`2021` + socal_murder$`2022`
socal_murder$avg_prior4years <- round((socal_murder$total_prior4years/4),1)
# Adding change percentages vs prepandemic vs last decade
socal_murder$inc_19to22 <- round(socal_murder$`2022`/socal_murder$`2019`*100-100,1)
socal_murder$inc_10to22 <- round(socal_murder$`2022`/socal_murder$`2010`*100-100,1)
# Calculating crime rates for each year
socal_murder$rate19 <- round((socal_murder$`2019`/socal_murder$population)*100000,1)
socal_murder$rate20 <- round((socal_murder$`2020`/socal_murder$population)*100000,1)
socal_murder$rate21 <- round((socal_murder$`2021`/socal_murder$population)*100000,1)
socal_murder$rate22 <- round((socal_murder$`2022`/socal_murder$population)*100000,1)
socal_murder$rate_prior4years <- round((socal_murder$avg_prior4years/socal_murder$population)*100000,1)
# For map/table making purposes, changing Inf and NaN in calculated fields to NA
socal_murder <- socal_murder %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
socal_murder <- socal_murder %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))
# Eliminate rates for districts with less than 1,000 estimated population
socal_murder$rate19 <- ifelse(socal_murder$population<1000,NA,socal_murder$rate19)
socal_murder$rate20 <- ifelse(socal_murder$population<1000,NA,socal_murder$rate20)
socal_murder$rate21 <- ifelse(socal_murder$population<1000,NA,socal_murder$rate21)
socal_murder$rate22 <- ifelse(socal_murder$population<1000,NA,socal_murder$rate22)
socal_murder$rate_prior4years <- ifelse(socal_murder$population<1000,NA,socal_murder$rate_prior4years)

# SEXUAL ASSAULTS
# Adding 3-year totals and annualized average over three years
socal_sexassault$total_prior4years <- socal_sexassault$`2019` + socal_sexassault$`2020` + socal_sexassault$`2021` + socal_sexassault$`2022`
socal_sexassault$avg_prior4years <- round((socal_sexassault$total_prior4years/4),1)
# Adding change percentages vs prepandemic vs last decade
socal_sexassault$inc_19to22 <- round(socal_sexassault$`2022`/socal_sexassault$`2019`*100-100,1)
socal_sexassault$inc_10to22 <- round(socal_sexassault$`2022`/socal_sexassault$`2010`*100-100,1)
# Calculating crime rates for each year
socal_sexassault$rate19 <- round((socal_sexassault$`2019`/socal_sexassault$population)*100000,1)
socal_sexassault$rate20 <- round((socal_sexassault$`2020`/socal_sexassault$population)*100000,1)
socal_sexassault$rate21 <- round((socal_sexassault$`2021`/socal_sexassault$population)*100000,1)
socal_sexassault$rate22 <- round((socal_sexassault$`2022`/socal_sexassault$population)*100000,1)
socal_sexassault$rate_prior4years <-round((socal_sexassault$avg_prior4years/socal_sexassault$population)*100000,1)
# For map/table making purposes, changing Inf and NaN in calculated fields to NA
socal_sexassault <- socal_sexassault %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
socal_sexassault <- socal_sexassault %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))
# Eliminate rates for districts with less than 1,000 estimated population
socal_sexassault$rate19 <- ifelse(socal_sexassault$population<1000,NA,socal_sexassault$rate19)
socal_sexassault$rate20 <- ifelse(socal_sexassault$population<1000,NA,socal_sexassault$rate20)
socal_sexassault$rate21 <- ifelse(socal_sexassault$population<1000,NA,socal_sexassault$rate21)
socal_sexassault$rate22 <- ifelse(socal_sexassault$population<1000,NA,socal_sexassault$rate22)
socal_sexassault$rate_prior4years <- ifelse(socal_sexassault$population<1000,NA,socal_sexassault$rate_prior4years)

# ROBBERIES
# Adding 3-year totals and annualized average over three years
socal_robbery$total_prior4years <- socal_robbery$`2019` + socal_robbery$`2020` + socal_robbery$`2021` + socal_robbery$`2022`
socal_robbery$avg_prior4years <- round((socal_robbery$total_prior4years/4),1)
# Adding change percentages vs prepandemic vs last decade
socal_robbery$inc_19to22 <- round(socal_robbery$`2022`/socal_robbery$`2019`*100-100,1)
socal_robbery$inc_10to22 <- round(socal_robbery$`2022`/socal_robbery$`2010`*100-100,1)
# Calculating crime rates for each year
socal_robbery$rate19 <- round((socal_robbery$`2019`/socal_robbery$population)*100000,1)
socal_robbery$rate20 <- round((socal_robbery$`2020`/socal_robbery$population)*100000,1)
socal_robbery$rate21 <- round((socal_robbery$`2021`/socal_robbery$population)*100000,1)
socal_robbery$rate22 <- round((socal_robbery$`2022`/socal_robbery$population)*100000,1)
socal_robbery$rate_prior4years <-round((socal_robbery$avg_prior4years/socal_robbery$population)*100000,1)
# For map/table making purposes, changing Inf and NaN in calculated fields to NA
socal_robbery <- socal_robbery %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
socal_robbery <- socal_robbery %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))
# Eliminate rates for districts with less than 1,000 estimated population
socal_robbery$rate19 <- ifelse(socal_robbery$population<1000,NA,socal_robbery$rate19)
socal_robbery$rate20 <- ifelse(socal_robbery$population<1000,NA,socal_robbery$rate20)
socal_robbery$rate21 <- ifelse(socal_robbery$population<1000,NA,socal_robbery$rate21)
socal_robbery$rate22 <- ifelse(socal_robbery$population<1000,NA,socal_robbery$rate22)
socal_robbery$rate_prior4years <- ifelse(socal_robbery$population<1000,NA,socal_robbery$rate_prior4years)

# ASSAULTS
# Adding 3-year totals and annualized average over three years
socal_assault$total_prior4years <- socal_assault$`2019` + socal_assault$`2020` + socal_assault$`2021` + socal_assault$`2022`
socal_assault$avg_prior4years <- round((socal_assault$total_prior4years/4),1)
# Adding change percentages vs prepandemic vs last decade
socal_assault$inc_19to22 <- round(socal_assault$`2022`/socal_assault$`2019`*100-100,1)
socal_assault$inc_10to22 <- round(socal_assault$`2022`/socal_assault$`2010`*100-100,1)
# Calculating crime rates for each year
socal_assault$rate19 <- round((socal_assault$`2019`/socal_assault$population)*100000,1)
socal_assault$rate20 <- round((socal_assault$`2020`/socal_assault$population)*100000,1)
socal_assault$rate21 <- round((socal_assault$`2021`/socal_assault$population)*100000,1)
socal_assault$rate22 <- round((socal_assault$`2022`/socal_assault$population)*100000,1)
socal_assault$rate_prior4years <-round((socal_assault$avg_prior4years/socal_assault$population)*100000,1)
# For map/table making purposes, changing Inf and NaN in calculated fields to NA
socal_assault <- socal_assault %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
socal_assault <- socal_assault %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))
# Eliminate rates for districts with less than 1,000 estimated population
socal_assault$rate19 <- ifelse(socal_assault$population<1000,NA,socal_assault$rate19)
socal_assault$rate20 <- ifelse(socal_assault$population<1000,NA,socal_assault$rate20)
socal_assault$rate21 <- ifelse(socal_assault$population<1000,NA,socal_assault$rate21)
socal_assault$rate22 <- ifelse(socal_assault$population<1000,NA,socal_assault$rate22)
socal_assault$rate_prior4years <- ifelse(socal_assault$population<1000,NA,socal_assault$rate_prior4years)

# BURGLARIES
# Adding 3-year totals and annualized average over three years
socal_burglary$total_prior4years <- socal_burglary$`2019` + socal_burglary$`2020` + socal_burglary$`2021` + socal_burglary$`2022`
socal_burglary$avg_prior4years <- round((socal_burglary$total_prior4years/4),1)
# Adding change percentages vs prepandemic vs last decade
socal_burglary$inc_19to22 <- round(socal_burglary$`2022`/socal_burglary$`2019`*100-100,1)
socal_burglary$inc_10to22 <- round(socal_burglary$`2022`/socal_burglary$`2010`*100-100,1)
# Calculating crime rates for each year
socal_burglary$rate19 <- round((socal_burglary$`2019`/socal_burglary$population)*100000,1)
socal_burglary$rate20 <- round((socal_burglary$`2020`/socal_burglary$population)*100000,1)
socal_burglary$rate21 <- round((socal_burglary$`2021`/socal_burglary$population)*100000,1)
socal_burglary$rate22 <- round((socal_burglary$`2022`/socal_burglary$population)*100000,1)
socal_burglary$rate_prior4years <-round((socal_burglary$avg_prior4years/socal_burglary$population)*100000,1)
# For map/table making purposes, changing Inf and NaN in calculated fields to NA
socal_burglary <- socal_burglary %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
socal_burglary <- socal_burglary %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))
# Eliminate rates for districts with less than 1,000 estimated population
socal_burglary$rate19 <- ifelse(socal_burglary$population<1000,NA,socal_burglary$rate19)
socal_burglary$rate20 <- ifelse(socal_burglary$population<1000,NA,socal_burglary$rate20)
socal_burglary$rate21 <- ifelse(socal_burglary$population<1000,NA,socal_burglary$rate21)
socal_burglary$rate22 <- ifelse(socal_burglary$population<1000,NA,socal_burglary$rate22)
socal_burglary$rate_prior4years <- ifelse(socal_burglary$population<1000,NA,socal_burglary$rate_prior4years)

# VEHICLE THEFTS
# Adding 3-year totals and annualized average over three years
socal_autotheft$total_prior4years <- socal_autotheft$`2019` + socal_autotheft$`2020` + socal_autotheft$`2021` + socal_autotheft$`2022`
socal_autotheft$avg_prior4years <- round((socal_autotheft$total_prior4years/4),1)
# Adding change percentages vs prepandemic vs last decade
socal_autotheft$inc_19to22 <- round(socal_autotheft$`2022`/socal_autotheft$`2019`*100-100,1)
socal_autotheft$inc_10to22 <- round(socal_autotheft$`2022`/socal_autotheft$`2010`*100-100,1)
# Calculating crime rates for each year
socal_autotheft$rate19 <- round((socal_autotheft$`2019`/socal_autotheft$population)*100000,1)
socal_autotheft$rate20 <- round((socal_autotheft$`2020`/socal_autotheft$population)*100000,1)
socal_autotheft$rate21 <- round((socal_autotheft$`2021`/socal_autotheft$population)*100000,1)
socal_autotheft$rate22 <- round((socal_autotheft$`2022`/socal_autotheft$population)*100000,1)
socal_autotheft$rate_prior4years <-round((socal_autotheft$avg_prior4years/socal_autotheft$population)*100000,1)
# For map/table making purposes, changing Inf and NaN in calculated fields to NA
socal_autotheft <- socal_autotheft %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
socal_autotheft <- socal_autotheft %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))
# Eliminate rates for districts with less than 1,000 estimated population
socal_autotheft$rate19 <- ifelse(socal_autotheft$population<1000,NA,socal_autotheft$rate19)
socal_autotheft$rate20 <- ifelse(socal_autotheft$population<1000,NA,socal_autotheft$rate20)
socal_autotheft$rate21 <- ifelse(socal_autotheft$population<1000,NA,socal_autotheft$rate21)
socal_autotheft$rate22 <- ifelse(socal_autotheft$population<1000,NA,socal_autotheft$rate22)
socal_autotheft$rate_prior4years <- ifelse(socal_autotheft$population<1000,NA,socal_autotheft$rate_prior4years)

# THEFTS
# Adding 3-year totals and annualized average over three years
socal_theft$total_prior4years <- socal_theft$`2019` + socal_theft$`2020` + socal_theft$`2021` + socal_theft$`2022`
socal_theft$avg_prior4years <- round((socal_theft$total_prior4years/4),1)
# Adding change percentages vs prepandemic vs last decade
socal_theft$inc_19to22 <- round(socal_theft$`2022`/socal_theft$`2019`*100-100,1)
socal_theft$inc_10to22 <- round(socal_theft$`2022`/socal_theft$`2010`*100-100,1)
# Calculating crime rates for each year
socal_theft$rate19 <- round((socal_theft$`2019`/socal_theft$population)*100000,1)
socal_theft$rate20 <- round((socal_theft$`2020`/socal_theft$population)*100000,1)
socal_theft$rate21 <- round((socal_theft$`2021`/socal_theft$population)*100000,1)
socal_theft$rate22 <- round((socal_theft$`2022`/socal_theft$population)*100000,1)
socal_theft$rate_prior4years <-round((socal_theft$avg_prior4years/socal_theft$population)*100000,1)
# For map/table making purposes, changing Inf and NaN in calculated fields to NA
socal_theft <- socal_theft %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
socal_theft <- socal_theft %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))
# Eliminate rates for districts with less than 1,000 estimated population
socal_theft$rate19 <- ifelse(socal_theft$population<1000,NA,socal_theft$rate19)
socal_theft$rate20 <- ifelse(socal_theft$population<1000,NA,socal_theft$rate20)
socal_theft$rate21 <- ifelse(socal_theft$population<1000,NA,socal_theft$rate21)
socal_theft$rate22 <- ifelse(socal_theft$population<1000,NA,socal_theft$rate22)
socal_theft$rate_prior4years <- ifelse(socal_theft$population<1000,NA,socal_theft$rate_prior4years)

# Add notations for LA area departments in markdown, for use in Datawrapper charts
socal_murder$place_chart <- case_when(socal_murder$agency=="LASD" ~ paste0(socal_murder$place,"^Area policed by ",socal_murder$agency,"^"),
                                      socal_murder$agency=="LAPD" ~ paste0(socal_murder$place,"^Division of Los Angeles PD^"),
                                      TRUE ~ socal_murder$place)
socal_sexassault$place_chart <- case_when(socal_sexassault$agency=="LASD" ~ paste0(socal_sexassault$place,"^Area policed by ",socal_sexassault$agency,"^"),
                                          socal_sexassault$agency=="LAPD" ~ paste0(socal_sexassault$place,"^Division of Los Angeles PD^"),
                                          TRUE ~ socal_murder$place)
socal_assault$place_chart <- case_when(socal_assault$agency=="LASD" ~ paste0(socal_assault$place,"^Area policed by ",socal_assault$agency,"^"),
                                       socal_assault$agency=="LAPD" ~ paste0(socal_assault$place,"^Division of Los Angeles PD^"),
                                       TRUE ~ socal_murder$place)
socal_robbery$place_chart <- case_when(socal_robbery$agency=="LASD" ~ paste0(socal_robbery$place,"^Area policed by ",socal_robbery$agency,"^"),
                                       socal_robbery$agency=="LAPD" ~ paste0(socal_robbery$place,"^Division of Los Angeles PD^"),
                                       TRUE ~ socal_murder$place)
socal_burglary$place_chart <- case_when(socal_burglary$agency=="LASD" ~ paste0(socal_burglary$place,"^Area policed by ",socal_burglary$agency,"^"),
                                        socal_burglary$agency=="LAPD" ~ paste0(socal_burglary$place,"^Division of Los Angeles PD^"),
                                        TRUE ~ socal_murder$place)
socal_theft$place_chart <- case_when(socal_theft$agency=="LASD" ~ paste0(socal_theft$place,"^Area policed by ",socal_theft$agency,"^"),
                                     socal_theft$agency=="LAPD" ~ paste0(socal_theft$place,"^Division of Los Angeles PD^"),
                                     TRUE ~ socal_murder$place)
socal_autotheft$place_chart <- case_when(socal_autotheft$agency=="LASD" ~ paste0(socal_autotheft$place,"^Area policed by ",socal_autotheft$agency,"^"),
                                         socal_autotheft$agency=="LAPD" ~ paste0(socal_autotheft$place,"^Division of Los Angeles PD^"),
                                         TRUE ~ socal_murder$place)

# Output county level files, and regionwide files, for each crime category as csv
# RIVERSIDE COUNTY
socal_murder %>% st_drop_geometry() %>% filter(county=="Riverside County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/riverside_murder.csv")
socal_sexassault %>% st_drop_geometry() %>% filter(county=="Riverside County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/riverside_sexassault.csv")
socal_assault %>% st_drop_geometry() %>% filter(county=="Riverside County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/riverside_assault.csv")
socal_robbery %>% st_drop_geometry() %>% filter(county=="Riverside County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/riverside_robbery.csv")
socal_burglary %>% st_drop_geometry() %>% filter(county=="Riverside County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/riverside_burglary.csv")
socal_theft %>% st_drop_geometry() %>% filter(county=="Riverside County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/riverside_theft.csv")
socal_autotheft %>% st_drop_geometry() %>% filter(county=="Riverside County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/riverside_autotheft.csv")
# ORANGE COUNTY
socal_murder %>% st_drop_geometry() %>% filter(county=="Orange County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/orange_murder.csv")
socal_sexassault %>% st_drop_geometry() %>% filter(county=="Orange County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/orange_sexassault.csv")
socal_assault %>% st_drop_geometry() %>% filter(county=="Orange County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/orange_assault.csv")
socal_robbery %>% st_drop_geometry() %>% filter(county=="Orange County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/orange_robbery.csv")
socal_burglary %>% st_drop_geometry() %>% filter(county=="Orange County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/orange_burglary.csv")
socal_theft %>% st_drop_geometry() %>% filter(county=="Orange County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/orange_theft.csv")
socal_autotheft %>% st_drop_geometry() %>% filter(county=="Orange County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/orange_autotheft.csv")
# SAN BERNARDINO COUNTY
socal_murder %>% st_drop_geometry() %>% filter(county=="San Bernardino County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/sb_murder.csv")
socal_sexassault %>% st_drop_geometry() %>% filter(county=="San Bernardino County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/sb_sexassault.csv")
socal_assault %>% st_drop_geometry() %>% filter(county=="San Bernardino County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/sb_assault.csv")
socal_robbery %>% st_drop_geometry() %>% filter(county=="San Bernardino County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/sb_robbery.csv")
socal_burglary %>% st_drop_geometry() %>% filter(county=="San Bernardino County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/sb_burglary.csv")
socal_theft %>% st_drop_geometry() %>% filter(county=="San Bernardino County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/sb_theft.csv")
socal_autotheft %>% st_drop_geometry() %>% filter(county=="San Bernardino County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/sb_autotheft.csv")
# VENTURA COUNTY
socal_murder %>% st_drop_geometry() %>% filter(county=="Ventura County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/ventura_murder.csv")
socal_sexassault %>% st_drop_geometry() %>% filter(county=="Ventura County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/ventura_sexassault.csv")
socal_assault %>% st_drop_geometry() %>% filter(county=="Ventura County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/ventura_assault.csv")
socal_robbery %>% st_drop_geometry() %>% filter(county=="Ventura County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/ventura_robbery.csv")
socal_burglary %>% st_drop_geometry() %>% filter(county=="Ventura County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/ventura_burglary.csv")
socal_theft %>% st_drop_geometry() %>% filter(county=="Ventura County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/ventura_theft.csv")
socal_autotheft %>% st_drop_geometry() %>% filter(county=="Ventura County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/ventura_autotheft.csv")
# LOS ANGELES COUNTY
socal_murder %>% st_drop_geometry() %>% filter(county=="Los Angeles County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/laco_murder.csv")
socal_sexassault %>% st_drop_geometry() %>% filter(county=="Los Angeles County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/laco_sexassault.csv")
socal_assault %>% st_drop_geometry() %>% filter(county=="Los Angeles County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/laco_assault.csv")
socal_robbery %>% st_drop_geometry() %>% filter(county=="Los Angeles County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/laco_robbery.csv")
socal_burglary %>% st_drop_geometry() %>% filter(county=="Los Angeles County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/laco_burglary.csv")
socal_theft %>% st_drop_geometry() %>% filter(county=="Los Angeles County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/laco_theft.csv")
socal_autotheft %>% st_drop_geometry() %>% filter(county=="Los Angeles County") %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/laco_autotheft.csv")
# REGION WIDE
socal_murder %>% st_drop_geometry() %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/socal_murder.csv")
socal_sexassault %>% st_drop_geometry() %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/socal_sexassault.csv")
socal_assault %>% st_drop_geometry() %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/socal_assault.csv")
socal_robbery %>% st_drop_geometry() %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/socal_robbery.csv")
socal_burglary %>% st_drop_geometry() %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/socal_burglary.csv")
socal_theft %>% st_drop_geometry() %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/socal_theft.csv")
socal_autotheft %>% st_drop_geometry() %>% select(27,4:16,20,21,25) %>% rename('Change since 2019'=inc_19to22,'Change since 2010'=inc_10to22,'2022 rate per 100K'=rate22) %>% write_csv("data/output/annual/socal_autotheft.csv")

# Create rds files of tables for each crime category for rendering trackers
socal_murder %>% saveRDS("scripts/rds/socal_murder.rds")
socal_sexassault %>% saveRDS("scripts/rds/socal_sexassault.rds")
socal_assault %>% saveRDS("scripts/rds/socal_assault.rds")
socal_robbery %>% saveRDS("scripts/rds/socal_robbery.rds")
socal_burglary %>% saveRDS("scripts/rds/socal_burglary.rds")
socal_theft %>% saveRDS("scripts/rds/socal_theft.rds")
socal_autotheft %>% saveRDS("scripts/rds/socal_autotheft.rds")



