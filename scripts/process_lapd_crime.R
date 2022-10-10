library(tidyverse)
library(sf)
library(readxl)
library(zoo)

houston_annual <- readRDS("scripts/rds/houston_annual.rds")
houston_monthly <- readRDS("scripts/rds/houston_monthly.rds")
houston_recent_new <- readRDS("scripts/rds/houston_recent_new.rds")

### COMBINE 2019, 2020, 2021 and 2022 TO DATE INTO SINGLE FILE
houston_crime <- bind_rows(houston_annual,houston_monthly)

# REDUCE THE RECENT 30-DAY FILE JUST TO THE DATES THAT NOT ALREADY IN HPD RELEASED 22 TO DATE FILE
houston_recent_new <- houston_recent_new %>% filter(as.Date(date)>max(as.Date(houston_crime$date)))

# COMBINE ANNUAL + MONTHLY WITH TODAY'S RECENT/30DAY FILE
houston_crime <- bind_rows(houston_crime,houston_recent_new)

# FORMATTING FULL DATA FILE FOR USE IN TRACKERS

# Create a year column
houston_crime$year <- substr(houston_crime$date,1,4)
# adds a month reference point for grouping for monthly trend figures
houston_crime$month <- lubridate::floor_date(as.Date(houston_crime$date),"month")

# Read in class-code table to classify offense types and categories
classcodes <- readRDS("scripts/rds/classcodes.rds")

# Add categories,types from classcodes to the individual crime records
houston_crime <- left_join(houston_crime,classcodes %>% select(2,4:7),by=c("nibrs_class","offense_type"))

# If beat is blank, add word Unknown
houston_crime$beat[is.na(houston_crime$beat)] <- "Unknown"
# Fix non-existent beat 15E11, which should be 15E10
houston_crime$beat <- ifelse(houston_crime$beat == "15E11","15E10",houston_crime$beat)
# Fix beats in District 5, 6 and 7 that became District 22
houston_crime$beat <- ifelse(houston_crime$beat == "5F40","22B10",houston_crime$beat)
houston_crime$beat <- ifelse(houston_crime$beat == "6B50","22B30",houston_crime$beat)
houston_crime$beat <- ifelse(houston_crime$beat == "6B60","22B20",houston_crime$beat)
houston_crime$beat <- ifelse(houston_crime$beat == "7C50","22B40",houston_crime$beat)

# clean up premise names throughout file
# the case when is stored once as a value by separate script
houston_crime$premise <- case_when(houston_crime$premise == 'Amusement Park' ~ 'Amusement park',
                                   houston_crime$premise == 'Bank, Savings & Loan' ~ 'Bank',
                                   houston_crime$premise == 'Bar, Nightclub' ~ 'Bar or nightclub',
                                   houston_crime$premise == 'Church, Synagogue, Temple' ~ 'Place of worship',
                                   houston_crime$premise == 'Commercial, Office Building' ~ 'Commercial or office building',
                                   houston_crime$premise == 'Convenience Store' ~ 'Convenience store',
                                   houston_crime$premise == 'Department, Discount Store' ~ 'Store',
                                   houston_crime$premise == 'Drug Store, Doctors Office, Hospital' ~ 'Medical care facility',
                                   houston_crime$premise == 'Field, Woods' ~ 'Field or woods',
                                   houston_crime$premise == 'Gambling Facility/Casino/Race Track' ~ 'Gambling facility',
                                   houston_crime$premise == 'Grocery, Supermarket' ~ 'Grocery',
                                   houston_crime$premise == 'Highway, Road, Street, Alley' ~ 'Highway, street or alley',
                                   houston_crime$premise == 'Hotel, Motel, ETC' ~ 'Hotel',
                                   houston_crime$premise == 'Industrial Site' ~ 'Industrial site',
                                   houston_crime$premise == 'Jail, Prison' ~ 'Jail or prison',
                                   houston_crime$premise == 'Lake, Waterway' ~ 'Lake or waterway',
                                   houston_crime$premise == 'Liquor Store' ~ 'Liquor store',
                                   houston_crime$premise == 'Park/Playground' ~ 'Park',
                                   houston_crime$premise == 'Parking Lot, Garage' ~ 'Parking lot',
                                   houston_crime$premise == 'Residence, Home (Includes Apartment)' ~ 'Residence',
                                   houston_crime$premise == 'Restaurant' ~ 'Restaurant',
                                   houston_crime$premise == 'School-Elementary/Secondary' ~ 'School',
                                   houston_crime$premise == 'Service, Gas Station' ~ 'Gas station',
                                   houston_crime$premise == 'Shopping Mall' ~ 'Mall',
                                   houston_crime$premise == 'Specialty Store' ~ 'Store',
                                   houston_crime$premise == 'Other, Unknown' ~ 'Unknown or other',
                                   TRUE ~ 'Unknown or other')

# Get latest date in our file and save for
# automating the updated date text in building tracker
asofdate <- max(houston_crime$date)
saveRDS(asofdate,"scripts/rds/asofdate.rds")

# write csv of houston crime as a backup
# worthwhile to think through if the full csv is even necessary to save; maybe for redundancy
write_csv(houston_crime,"data/output/houston_crime.csv")
saveRDS(houston_crime,"scripts/rds/houston_crime.rds")

# days total incidents and output for validation
days <- houston_crime %>% group_by(date) %>% summarise(count=n()) %>% arrange(desc(date))
write_csv(days,"data/output/reference/days_incident_check.csv")

# Clean up
rm(houston_annual,houston_monthly,houston_recent_new)

# Extract the last 12 months into a separate file
houston_crime_last12 <- houston_crime %>% filter(date>(max(houston_crime$date)-31536000))

### CITYWIDE CRIME TOTALS AND OUTPUT

# Set variable of Houston population
# likely needs added to the tracker itself
houston_population <- 2304580

# Calculate of each detailed offense type CITYWIDE
citywide_detailed <- houston_crime %>%
  group_by(offense_type,nibrs_class,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_detailed <- citywide_detailed %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
citywide_detailed_last12 <- houston_crime_last12 %>%
  group_by(offense_type,nibrs_class) %>%
  summarise(last12mos = sum(offense_count))
citywide_detailed <- left_join(citywide_detailed,citywide_detailed_last12,by=c("offense_type","nibrs_class"))
# add zeros where there were no crimes tallied that year
citywide_detailed[is.na(citywide_detailed)] <- 0
# Calculate a total across the 3 prior years
citywide_detailed$total_prior3years <- citywide_detailed$total19+citywide_detailed$total20+citywide_detailed$total21
citywide_detailed$avg_prior3years <- round(citywide_detailed$total_prior3years/3,1)
# calculate increases
citywide_detailed$inc_19to21 <- round(citywide_detailed$total21/citywide_detailed$total19*100-100,1)
citywide_detailed$inc_19tolast12 <- round(citywide_detailed$last12mos/citywide_detailed$total19*100-100,1)
citywide_detailed$inc_21tolast12 <- round(citywide_detailed$last12mos/citywide_detailed$total21*100-100,1)
citywide_detailed$inc_prior3yearavgtolast12 <- round((citywide_detailed$last12mos/citywide_detailed$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_detailed$rate19 <- round(citywide_detailed$total19/houston_population*100000,1)
citywide_detailed$rate20 <- round(citywide_detailed$total20/houston_population*100000,1)
citywide_detailed$rate21 <- round(citywide_detailed$total21/houston_population*100000,1)
citywide_detailed$rate_last12 <- round(citywide_detailed$last12mos/houston_population*100000,1)
# calculate a multiyear rate
citywide_detailed$rate_prior3years <- round(citywide_detailed$avg_prior3years/houston_population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
citywide_detailed <- citywide_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
citywide_detailed <- citywide_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# Calculate of each detailed offense type CITYWIDE
citywide_detailed_monthly <- houston_crime %>%
  group_by(offense_type,nibrs_class,month) %>%
  summarise(count = sum(offense_count))
# add rolling average of 3 months for chart trend line & round to clean
citywide_detailed_monthly <- citywide_detailed_monthly %>%
  dplyr::mutate(rollavg_3month = rollsum(count, k = 3, fill = NA, align = "right")/3)
citywide_detailed_monthly$rollavg_3month <- round(citywide_detailed_monthly$rollavg_3month,0)
# write to save for charts for detailed monthly
write_csv(citywide_detailed_monthly,"data/output/monthly/citywide_detailed_monthly.csv")
# and for murders monthly 
citywide_detailed_monthly %>% filter(nibrs_class=="09A") %>% write_csv("data/output/monthly/murders_monthly.csv")

# Calculate of each category of offense CITYWIDE
citywide_category <- houston_crime %>%
  group_by(category_name,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_category <- citywide_category %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
citywide_category_last12 <- houston_crime_last12 %>%
  group_by(category_name) %>%
  summarise(last12mos = sum(offense_count))
citywide_category <- left_join(citywide_category,citywide_category_last12,by=c("category_name"))
# add zeros where there were no crimes tallied that year
citywide_category[is.na(citywide_category)] <- 0
# Calculate a total across the 3 prior years
citywide_category$total_prior3years <- citywide_category$total19+citywide_category$total20+citywide_category$total21
citywide_category$avg_prior3years <- round(citywide_category$total_prior3years/3,1)
# calculate increases
citywide_category$inc_19to21 <- round(citywide_category$total21/citywide_category$total19*100-100,1)
citywide_category$inc_19tolast12 <- round(citywide_category$last12mos/citywide_category$total19*100-100,1)
citywide_category$inc_21tolast12 <- round(citywide_category$last12mos/citywide_category$total21*100-100,1)
citywide_category$inc_prior3yearavgtolast12 <- round((citywide_category$last12mos/citywide_category$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_category$rate19 <- round(citywide_category$total19/houston_population*100000,1)
citywide_category$rate20 <- round(citywide_category$total20/houston_population*100000,1)
citywide_category$rate21 <- round(citywide_category$total21/houston_population*100000,1)
citywide_category$rate_last12 <- round(citywide_category$last12mos/houston_population*100000,1)
# calculate a multiyear rate
citywide_category$rate_prior3years <- round(citywide_category$avg_prior3years/houston_population*100000,1)

# Calculate monthly totals for categories of crimes CITYWIDE
citywide_category_monthly <- houston_crime %>%
  group_by(category_name,month) %>%
  summarise(count = sum(offense_count))
# add rolling average of 3 months for chart trend line & round to clean
citywide_category_monthly <- citywide_category_monthly %>%
  arrange(category_name,month) %>%
  dplyr::mutate(rollavg_3month = rollsum(count, k = 3, fill = NA, align = "right")/3)
citywide_category_monthly$rollavg_3month <- round(citywide_category_monthly$rollavg_3month,0)
# write series of monthly files for charts (NOTE murder is written above in detailed section)
write_csv(citywide_category_monthly,"data/output/monthly/citywide_category_monthly.csv")
citywide_category_monthly %>% filter(category_name=="Sexual Assault") %>% write_csv("data/output/monthly/sexassaults_monthly.csv")
citywide_category_monthly %>% filter(category_name=="Auto Theft") %>% write_csv("data/output/monthly/autothefts_monthly.csv")
citywide_category_monthly %>% filter(category_name=="Theft") %>% write_csv("data/output/monthly/thefts_monthly.csv")
citywide_category_monthly %>% filter(category_name=="Burglary") %>% write_csv("data/output/monthly/burglaries_monthly.csv")
citywide_category_monthly %>% filter(category_name=="Robbery") %>% write_csv("data/output/monthly/robberies_monthly.csv")
citywide_category_monthly %>% filter(category_name=="Assault") %>% write_csv("data/output/monthly/assaults_monthly.csv")
citywide_category_monthly %>% filter(category_name=="Drug Offenses") %>% write_csv("data/output/monthly/drugs_monthly.csv")




# Calculate of each type of crime CITYWIDE
citywide_type <- houston_crime %>%
  group_by(type,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
citywide_type <- citywide_type %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
citywide_type_last12 <- houston_crime_last12 %>%
  group_by(type) %>%
  summarise(last12mos = sum(offense_count))
citywide_type <- left_join(citywide_type,citywide_type_last12,by=c("type"))
# Calculate a total across the 3 prior years
citywide_type$total_prior3years <- citywide_type$total19+citywide_type$total20+citywide_type$total21
citywide_type$avg_prior3years <- round(citywide_type$total_prior3years/3,1)
# add zeros where there were no crimes tallied that year
citywide_type[is.na(citywide_type)] <- 0
# calculate increases
citywide_type$inc_19to21 <- round(citywide_type$total21/citywide_type$total19*100-100,1)
citywide_type$inc_19tolast12 <- round(citywide_type$last12mos/citywide_type$total19*100-100,1)
citywide_type$inc_21tolast12 <- round(citywide_type$last12mos/citywide_type$total21*100-100,1)
citywide_type$inc_prior3yearavgtolast12 <- round((citywide_type$last12mos/citywide_type$avg_prior3years)*100-100,0)
# calculate the citywide rates
citywide_type$rate19 <- round(citywide_type$total19/houston_population*100000,1)
citywide_type$rate20 <- round(citywide_type$total20/houston_population*100000,1)
citywide_type$rate21 <- round(citywide_type$total21/houston_population*100000,1)
citywide_type$rate_last12 <- round(citywide_type$last12mos/houston_population*100000,1)
# calculate a multiyear rate
citywide_type$rate_prior3years <- round(citywide_type$avg_prior3years/houston_population*100000,1)

### HOUSTON POLICE BEAT CRIME TOTALS AND OUTPUT

# MERGE WITH BEATS GEOGRAPHY AND POPULATION
# Geography and populations processed separately in 
# source(process_houston_police_beats.R)
beats <- st_read("data/source/geo/beats.geojson")
# Test that all beats show in data and identify beat #s that do not
# beatsindata <- houston_crime %>% group_by(beat,year) %>% summarise(count=n()) %>% pivot_wider(names_from=year, values_from=count)
# anti_join(beatsindata,beats,by="beat")

# Calculate total of each detailed offense type BY POLICE BEAT
beat_detailed <- houston_crime %>%
  group_by(beat,offense_type,nibrs_class,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
beat_detailed <- beat_detailed %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
beat_detailed_last12 <- houston_crime_last12 %>%
  group_by(beat,offense_type,nibrs_class) %>%
  summarise(last12mos = sum(offense_count))
beat_detailed <- left_join(beat_detailed,beat_detailed_last12,by=c("beat","offense_type","nibrs_class"))
rm(beat_detailed_last12)
# add zeros where there were no crimes tallied that year
beat_detailed[is.na(beat_detailed)] <- 0
# Calculate a total across the 3 prior years
beat_detailed$total_prior3years <- beat_detailed$total19+beat_detailed$total20+beat_detailed$total21
beat_detailed$avg_prior3years <- round(beat_detailed$total_prior3years/3,1)
# calculate increases
beat_detailed$inc_19to21 <- round(beat_detailed$total21/beat_detailed$total19*100-100,1)
beat_detailed$inc_19tolast12 <- round(beat_detailed$last12mos/beat_detailed$total19*100-100,1)
beat_detailed$inc_21tolast12 <- round(beat_detailed$last12mos/beat_detailed$total21*100-100,1)
beat_detailed$inc_prior3yearavgtolast12 <- round((beat_detailed$last12mos/beat_detailed$avg_prior3years)*100-100,0)
# add population for beats
beat_detailed <- full_join(beats,beat_detailed,by="beat") 
# calculate the beat by beat rates PER 1K people
beat_detailed$rate19 <- round(beat_detailed$total19/beat_detailed$population*100000,1)
beat_detailed$rate20 <- round(beat_detailed$total20/beat_detailed$population*100000,1)
beat_detailed$rate21 <- round(beat_detailed$total21/beat_detailed$population*100000,1)
beat_detailed$rate_last12 <- round(beat_detailed$last12mos/beat_detailed$population*100000,1)
# calculate a multiyear rate
beat_detailed$rate_prior3years <- round(beat_detailed$avg_prior3years/beat_detailed$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
beat_detailed <- beat_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
beat_detailed <- beat_detailed %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# Calculate total of each category of offense BY POLICE BEAT
beat_category <- houston_crime %>%
  group_by(beat,category_name,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
beat_category <- beat_category %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
beat_category_last12 <- houston_crime_last12 %>%
  group_by(beat,category_name) %>%
  summarise(last12mos = sum(offense_count))
beat_category <- left_join(beat_category,beat_category_last12,by=c("beat","category_name"))
rm(beat_category_last12)
# add zeros where there were no crimes tallied that year
beat_category[is.na(beat_category)] <- 0
# Calculate a total across the 3 prior years
beat_category$total_prior3years <- beat_category$total19+beat_category$total20+beat_category$total21
beat_category$avg_prior3years <- round(beat_category$total_prior3years/3,1)
# calculate increases
beat_category$inc_19to21 <- round(beat_category$total21/beat_category$total19*100-100,1)
beat_category$inc_19tolast12 <- round(beat_category$last12mos/beat_category$total19*100-100,1)
beat_category$inc_21tolast12 <- round(beat_category$last12mos/beat_category$total21*100-100,1)
beat_category$inc_prior3yearavgtolast12 <- round((beat_category$last12mos/beat_category$avg_prior3years)*100-100,0)
# add population for beats
beat_category <- full_join(beats,beat_category,by="beat") 
# calculate the beat by beat rates PER 1K people
beat_category$rate19 <- round(beat_category$total19/beat_category$population*100000,1)
beat_category$rate20 <- round(beat_category$total20/beat_category$population*100000,1)
beat_category$rate21 <- round(beat_category$total21/beat_category$population*100000,1)
beat_category$rate_last12 <- round(beat_category$last12mos/beat_category$population*100000,1)
# calculate a multiyear rate
beat_category$rate_prior3years <- round(beat_category$avg_prior3years/beat_category$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
beat_category <- beat_category %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
beat_category <- beat_category %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# Calculate total of each type of crime BY POLICE BEAT
beat_type <- houston_crime %>%
  group_by(beat,type,year) %>%
  summarise(count = sum(offense_count)) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
beat_type <- beat_type %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
beat_type_last12 <- houston_crime_last12 %>%
  group_by(beat,type) %>%
  summarise(last12mos = sum(offense_count))
beat_type <- left_join(beat_type,beat_type_last12,by=c("beat","type"))
rm(beat_type_last12)
# add zeros where there were no crimes tallied that year
beat_type[is.na(beat_type)] <- 0
# Calculate a total across the 3 prior years
beat_type$total_prior3years <- beat_type$total19+beat_type$total20+beat_type$total21
beat_type$avg_prior3years <- round(beat_type$total_prior3years/3,1)
# calculate increases
beat_type$inc_19to21 <- round(beat_type$total21/beat_type$total19*100-100,1)
beat_type$inc_19tolast12 <- round(beat_type$last12mos/beat_type$total19*100-100,1)
beat_type$inc_21tolast12 <- round(beat_type$last12mos/beat_type$total21*100-100,1)
beat_type$inc_prior3yearavgtolast12 <- round((beat_type$last12mos/beat_type$avg_prior3years)*100-100,0)
# add population for beats
beat_type <- full_join(beats,beat_type,by="beat") 
# calculate the beat by beat rates PER 1K people
beat_type$rate19 <- round(beat_type$total19/beat_type$population*100000,1)
beat_type$rate20 <- round(beat_type$total20/beat_type$population*100000,1)
beat_type$rate21 <- round(beat_type$total21/beat_type$population*100000,1)
beat_type$rate_last12 <- round(beat_type$last12mos/beat_type$population*100000,1)
# calculate a multiyear rate
beat_type$rate_prior3years <- round(beat_type$avg_prior3years/beat_type$population*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
beat_type <- beat_type %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
beat_type <- beat_type %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# output various csvs for basic tables to be made with crime totals
# we are dropping geometry for beats here because this is just for tables
beat_detailed %>% st_drop_geometry() %>% write_csv("data/output/beat/beat_detailed.csv")
beat_category %>% st_drop_geometry() %>% write_csv("data/output/beat/beat_category.csv")
beat_type %>% st_drop_geometry() %>% write_csv("data/output/beat/beat_type.csv")
citywide_detailed %>% write_csv("data/output/city/citywide_detailed.csv")
citywide_category %>% write_csv("data/output/city/citywide_category.csv")
citywide_type %>% write_csv("data/output/city/citywide_type.csv")

# Create individual spatial tables of crimes by major categories and types
murders_beat <- beat_detailed %>% filter(nibrs_class=="09A")
sexassaults_beat <- beat_category %>% filter(category_name=="Sexual Assault")
autothefts_beat <- beat_category %>% filter(category_name=="Auto Theft")
thefts_beat <- beat_category %>% filter(category_name=="Theft")
burglaries_beat <- beat_category %>% filter(category_name=="Burglary")
robberies_beat <- beat_category %>% filter(category_name=="Robbery")
assaults_beat <- beat_category %>% filter(category_name=="Assault")
drugs_beat <- beat_category %>% filter(category_name=="Drug Offenses")
violence_beat <- beat_type %>% filter(type=="Violent")
property_beat <- beat_type %>% filter(type=="Property")
# Create same set of tables for citywide figures
murders_city <- citywide_detailed %>% filter(nibrs_class=="09A")
sexassaults_city <- citywide_category %>% filter(category_name=="Sexual Assault")
autothefts_city <- citywide_category %>% filter(category_name=="Auto Theft")
thefts_city <- citywide_category %>% filter(category_name=="Theft")
burglaries_city <- citywide_category %>% filter(category_name=="Burglary")
robberies_city <- citywide_category %>% filter(category_name=="Robbery")
assaults_city <- citywide_category %>% filter(category_name=="Assault")
drugs_city <- citywide_category %>% filter(category_name=="Drug Offenses")
violence_city <- citywide_type %>% filter(type=="Violent")
property_city <- citywide_type %>% filter(type=="Property")

# Using premise to identify the kinds of places where murders happen
where_murders_happen <- houston_crime %>%
  filter(nibrs_class=="09A") %>%
  group_by(year,premise) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from=year, values_from=count)
# Using premise to identify the kinds of places where murders happen
where_murders_happen_last12 <- houston_crime_last12 %>%
  filter(nibrs_class=="09A") %>%
  group_by(premise) %>%
  summarise(last12=n())
# merge last 12 into the table
where_murders_happen <- full_join(where_murders_happen,where_murders_happen_last12,by="premise")
# add zeros where there were no crimes tallied that year
where_murders_happen[is.na(where_murders_happen)] <- 0
rm(where_murders_happen_last12)

# Using premise to identify the kinds of places where all violent crimes happen
where_violentcrimes_happen <- houston_crime %>%
  filter(type=="Violent") %>%
  group_by(premise,year) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from=year, values_from=count) %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022") 

# Using premise to identify the kinds of places where all violent crimes happen
where_propertycrimes_happen <- houston_crime %>%
  filter(type=="Property") %>%
  group_by(premise,year) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from=year, values_from=count) %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022") 

# Using hour to identify the hours of day when murders happen
when_murders_happen <- houston_crime %>%
  filter(nibrs_class=="09A") %>%
  group_by(hour) %>%
  summarise(count=n()) %>% 
  arrange(hour)
when_murders_happen$time <- case_when(when_murders_happen$hour == "0" ~ "12 a.m.",
                                      when_murders_happen$hour %in% c("1","2","3","4","5","6","7","8","9","10","11") ~ paste0(when_murders_happen$hour," a.m."),
                                      when_murders_happen$hour %in% c("12") ~ paste0(when_murders_happen$hour," p.m."),
                                      when_murders_happen$hour %in% c("13","14","15","16","17","18","19","20","21","22","23") ~ paste0((as.numeric(when_murders_happen$hour)-12)," p.m."),
                                      TRUE ~ "Other")
when_murders_happen$timeframe <- case_when(when_murders_happen$hour %in% c("0","1","2","3","4","21","22","23") ~ "Overnight from 9 p.m. to 5 a.m.",
                                           when_murders_happen$hour %in% c("5","6","7","8","9","10","11") ~ "Morning from 5 a.m. to 12 p.m.",
                                           when_murders_happen$hour %in% c("12","13","14","15","16","17","18","19","20")  ~ "Afternoon/Evening from 12 p.m. to 9 p.m.",
                                           TRUE ~ "Other")
when_murders_happen <- when_murders_happen %>%
  group_by(timeframe) %>%
  summarise(total=sum(count))

# Create individual spatial tables of crimes by major categories and types
murders_beat %>% st_drop_geometry() %>% write_csv("data/output/beat/murders_beat.csv")
sexassaults_beat %>% st_drop_geometry() %>% write_csv("data/output/beat/sexassaults_beat.csv")
autothefts_beat %>% st_drop_geometry() %>% write_csv("data/output/beat/autothefts_beat.csv")
thefts_beat %>% st_drop_geometry() %>% write_csv("data/output/beat/thefts_beat.csv")
burglaries_beat %>% st_drop_geometry() %>% write_csv("data/output/beat/burglaries_beat.csv")
robberies_beat %>% st_drop_geometry() %>% write_csv("data/output/beat/robberies_beat.csv")
assaults_beat %>% st_drop_geometry() %>% write_csv("data/output/beat/assaults_beat.csv")
drugs_beat %>% st_drop_geometry() %>% write_csv("data/output/beat/drugs_beat.csv")
violence_beat %>% st_drop_geometry() %>% write_csv("data/output/beat/violence_beat.csv")
property_beat %>% st_drop_geometry() %>% write_csv("data/output/beat/property_beat.csv")

# TEST TEST TEST OF WHETHER RDS WILL WORK FOR TRACKERS IN AUTOMATION
saveRDS(murders_city,"scripts/rds/murders_city.rds")
saveRDS(assaults_city,"scripts/rds/assaults_city.rds")
saveRDS(sexassaults_city,"scripts/rds/sexassaults_city.rds")
saveRDS(autothefts_city,"scripts/rds/autothefts_city.rds")
saveRDS(thefts_city,"scripts/rds/thefts_city.rds")
saveRDS(burglaries_city,"scripts/rds/burglaries_city.rds")
saveRDS(robberies_city,"scripts/rds/robberies_city.rds")
saveRDS(drugs_city,"scripts/rds/drugs_city.rds")

saveRDS(murders_beat,"scripts/rds/murders_beat.rds")
saveRDS(assaults_beat,"scripts/rds/assaults_beat.rds")
saveRDS(sexassaults_beat,"scripts/rds/sexassaults_beat.rds")
saveRDS(autothefts_beat,"scripts/rds/autothefts_beat.rds")
saveRDS(thefts_beat,"scripts/rds/thefts_beat.rds")
saveRDS(burglaries_beat,"scripts/rds/burglaries_beat.rds")
saveRDS(robberies_beat,"scripts/rds/robberies_beat.rds")
saveRDS(drugs_beat,"scripts/rds/drugs_beat.rds")


# additional table exports for specific charts
where_murders_happen %>% write_csv("data/output/city/where_murders_happen.csv")
when_murders_happen %>% write_csv("data/output/city/when_murders_happen.csv")

# deaths cause data update for TX specific table
deaths <- read_excel("data/source/health/deaths.xlsx") 
deaths <- deaths %>% filter(state=="TX")
deaths$Homicide <- murders_city$rate_last12
write_csv(deaths,"data/source/health/death_rates.csv")
