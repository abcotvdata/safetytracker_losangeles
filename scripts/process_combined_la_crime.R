library(tidyverse)
library(sf)
library(readxl)
library(zoo)
library(lubridate)

# Read, clean and format LAPD files
lapd_crime <- readRDS("scripts/rds/lapd_crime.rds")
lasd_category <- readRDS("scripts/rds/lasd_category.rds")
lasd_district_category <- readRDS("scripts/rds/lasd_district_category.rds")


# For last 12, we're going to default to the most recent LAPD date available for consistency
# LASD crime lags slightly more, so for the combined stats for the L.A. area, we're going to use the older date
lapd_crime_last12 <- lapd_crime %>% filter(date>(max(lapd_crime$date)-31536000))

###
### LAPD By Category
lapd_category <- lapd_crime %>%
  group_by(category,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
lapd_category <- lapd_category %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# add last 12 months
lapd_category_last12 <- lapd_crime_last12 %>%
  group_by(category) %>%
  summarise(last12mos = n())
lapd_category <- left_join(lapd_category,lapd_category_last12,by=c("category"))
# Clean up: remove category-level last 12 files
rm(lapd_category_last12)

# Combine and merge the yearly file for countywide
citywide_crime <- rbind(lapd_category,lasd_category) %>%
  group_by(category) %>%
  summarise("total19" = sum(total19),
             "total20" = sum(total20),
             "total21" = sum(total21),
             "total22" = sum(total22),
             "last12mos" = sum(last12mos))

# Set variable of LA city population
# and also for the countywide population COVERED by the included agencies
la_population <- 1000000
covered_population <- sum(districts_geo$population)

# add zeros where there were no crimes tallied that year
citywide_crime[is.na(citywide_crime)] <- 0
# add 3-year annualized averages
citywide_crime$total_prior3years <- citywide_crime$total19+
  citywide_crime$total20+
  citywide_crime$total21
citywide_crime$avg_prior3years <- round(((citywide_crime$total19+
                                            citywide_crime$total20+
                                            citywide_crime$total21)/3),1)
# now add the increases or change percentages
citywide_crime$inc_19to21 <- round(citywide_crime$total21/citywide_crime$total19*100-100,1)
citywide_crime$inc_19tolast12 <- round(citywide_crime$last12mos/citywide_crime$total19*100-100,1)
citywide_crime$inc_21tolast12 <- round(citywide_crime$last12mos/citywide_crime$total21*100-100,1)
citywide_crime$inc_prior3yearavgtolast12 <- round((citywide_crime$last12mos/citywide_crime$avg_prior3years)*100-100,0)
# add crime rates for each year
citywide_crime$rate19 <- round((citywide_crime$total19/covered_population)*100000,1)
citywide_crime$rate20 <- round((citywide_crime$total20/covered_population)*100000,1)
citywide_crime$rate21 <- round((citywide_crime$total21/covered_population)*100000,1)
citywide_crime$rate_last12 <- round((citywide_crime$last12mos/covered_population)*100000,1)
# 3 yr rate
citywide_crime$rate_prior3years <- 
  round((citywide_crime$avg_prior3years/covered_population)*100000,1)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
citywide_crime <- citywide_crime %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
citywide_crime <- citywide_crime %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# create a quick long-term annual table
citywide_yearly <- citywide_crime %>% select(1:4,6)
# add additional years from state archive of reported ucr crimes back to 2000
#yearly_archive <- read_csv("data/source/annual/oakland_annual_state.csv")
citywide_yearly <- right_join(citywide_yearly,lac_yearly_archive %>% select(2:24),by="category") %>%
  select(1,7:25,2:5) %>% rename("2019"="total19","2020"="total20","2021"="total21","Last 12 months"="last12mos")
# save for annual charts  
write_csv(citywide_yearly,"data/output/yearly/citywide_yearly.csv")

###
### LAPD By District
lapd_district_category <- lapd_crime %>%
  group_by(area_name,agency,category,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
lapd_district_category <- lapd_district_category %>% 
  rename("total19" = "2019",
         "total20" = "2020",
         "total21" = "2021",
         "total22" = "2022")
# filter out other/part2
lapd_district_category <- lapd_district_category %>% 
  filter(category!="Other or Part 2")
# add last 12 months
lapd_district_category_last12 <- lapd_crime_last12 %>%
  group_by(area_name,agency,category) %>%
  summarise(last12mos = n())
lapd_district_category <- left_join(lapd_district_category,lapd_district_category_last12,by=c("category","area_name","agency"))
# add zeros where there were no crimes tallied that year
lapd_district_category[is.na(lapd_district_category)] <- 0
# rename district column for consistency across code
lapd_district_category <- lapd_district_category %>% rename("district"="area_name")
# Clean up: remove category-level last 12 files
rm(lapd_district_category_last12)

# Combine and merge the yearly file for countywide
district_crime <- rbind(lapd_district_category,lasd_district_category) %>%
  group_by(category,agency,district) %>%
  summarise("total19" = sum(total19),
            "total20" = sum(total20),
            "total21" = sum(total21),
            "total22" = sum(total22),
            "last12mos" = sum(last12mos))

# la county police districts geo file with populations
districts_geo <- readRDS("scripts/rds/la_police_districts.rds")

# Join to combined police districts
district_crime <- full_join(districts_geo,district_crime,by=c("agency","district"))

# add zeros where there were no crimes tallied that year
district_crime[is.na(district_crime)] <- 0

# add 3-year totals and annualized averages
district_crime$total_prior3years <- district_crime$total19+
  district_crime$total20+
  district_crime$total21
district_crime$avg_prior3years <- round(((district_crime$total19+
                                            district_crime$total20+
                                            district_crime$total21)/3),1)
# now add the increases or change percentages
district_crime$inc_19to21 <- round(district_crime$total21/district_crime$total19*100-100,1)
district_crime$inc_19tolast12 <- round(district_crime$last12mos/district_crime$total19*100-100,1)
district_crime$inc_21tolast12 <- round(district_crime$last12mos/district_crime$total21*100-100,1)
district_crime$inc_prior3yearavgtolast12 <- round((district_crime$last12mos/district_crime$avg_prior3years)*100-100,0)
# add crime rates for each year
district_crime$rate19 <- round((district_crime$total19/district_crime$population)*100000,1)
district_crime$rate20 <- round((district_crime$total20/district_crime$population)*100000,1)
district_crime$rate21 <- round((district_crime$total21/district_crime$population)*100000,1)
district_crime$rate_last12 <- round((district_crime$last12mos/district_crime$population)*100000,1)
district_crime$rate_prior3years <- 
  round((district_crime$avg_prior3years/district_crime$population)*100000,1)

# Now reduce the precinct down to just the columns we likely need for the tracker pages
# district_crime <- district_crime %>% select(1,4,5,6,26:28,36:40,44:55,29,42)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
district_crime <- district_crime %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
district_crime <- district_crime %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# create a quick long-term annual table
district_yearly <- district_crime %>% select(1,2,4:7,9) %>% st_drop_geometry()
write_csv(district_yearly,"data/output/yearly/district_yearly.csv")

# Now make individual crime files for trackers
# filter precinct versions - using beat for code consistency
murders_district <- district_crime %>% filter(category=="Homicide")
sexassaults_district <- district_crime %>% filter(category=="Sexual Assault")
robberies_district <- district_crime %>% filter(category=="Robbery")
assaults_district <- district_crime %>% filter(category=="Aggravated Assault")
burglaries_district <- district_crime %>% filter(category=="Burglary")
thefts_district <- district_crime %>% filter(category=="Larceny")
autothefts_district <- district_crime %>% filter(category=="Vehicle Theft")
# filter citywide versions
murders_city <- citywide_crime %>% filter(category=="Homicide")
sexassaults_city <- citywide_crime %>% filter(category=="Sexual Assault")
robberies_city <- citywide_crime %>% filter(category=="Robbery")
assaults_city <- citywide_crime %>% filter(category=="Aggravated Assault")
burglaries_city <- citywide_crime %>% filter(category=="Burglary")
thefts_city <- citywide_crime %>% filter(category=="Larceny")
autothefts_city <- citywide_crime %>% filter(category=="Vehicle Theft")

# make the death rate comparables file unique to this state
deaths <- read_excel("data/source/health/deaths.xlsx") 
deaths <- deaths %>% filter(state=="CA")
deaths$Homicide <- murders_city$rate_last12
write_csv(deaths,"data/source/health/death_rates.csv")

#### 
# Archive latest files as csv and rds store for use in trackers
# First save the weekly files as output csvs for others to use
#write_csv(district_crime,"data/output/weekly/district_crime.csv")
#write_csv(citywide_crime,"data/output/weekly/citywide_crime.csv")
# Archive a year's worth of week-numbered files from the weekly updates
write_csv(district_crime,paste0("data/output/archive/district_crime_week",lapd_asofdate,".csv"))
write_csv(citywide_crime,paste0("data/output/archive/citywide_crime_week",lapd_asofdate,".csv"))
# Now save the files needed for trackers into RDS store in scripts for GH Actions
# district versions
saveRDS(district_crime,"scripts/rds/district_crime.rds")
saveRDS(murders_district,"scripts/rds/murders_district.rds")
saveRDS(sexassaults_district,"scripts/rds/sexassaults_district.rds")
saveRDS(robberies_district,"scripts/rds/robberies_district.rds")
saveRDS(assaults_district,"scripts/rds/assaults_district.rds")
saveRDS(burglaries_district,"scripts/rds/burglaries_district.rds")
saveRDS(thefts_district,"scripts/rds/thefts_district.rds")
saveRDS(autothefts_district,"scripts/rds/autothefts_district.rds")
# city versions
saveRDS(citywide_crime,"scripts/rds/citywide_crime.rds")
saveRDS(murders_city,"scripts/rds/murders_city.rds")
saveRDS(sexassaults_city,"scripts/rds/sexassaults_city.rds")
saveRDS(robberies_city,"scripts/rds/robberies_city.rds")
saveRDS(assaults_city,"scripts/rds/assaults_city.rds")
saveRDS(burglaries_city,"scripts/rds/burglaries_city.rds")
saveRDS(thefts_city,"scripts/rds/thefts_city.rds")
saveRDS(autothefts_city,"scripts/rds/autothefts_city.rds")

### Some tables for charts for our pages
#sf_crime_totals %>% write_csv("data/output/yearly/totals_by_type.csv")
citywide_yearly %>% filter(category=="Homicide") %>% write_csv("data/output/yearly/murders_city.csv")
citywide_yearly %>% filter(category=="Sexual Assault") %>%  write_csv("data/output/yearly/sexassaults_city.csv")
citywide_yearly %>% filter(category=="Vehicle Theft") %>%  write_csv("data/output/yearly/autothefts_city.csv")
citywide_yearly %>% filter(category=="Larceny") %>%  write_csv("data/output/yearly/thefts_city.csv")
citywide_yearly %>% filter(category=="Burglary") %>%  write_csv("data/output/yearly/burglaries_city.csv")
citywide_yearly %>% filter(category=="Robbery") %>%  write_csv("data/output/yearly/robberies_city.csv")
citywide_yearly %>% filter(category=="Aggravated Assault") %>%  write_csv("data/output/yearly/assaults_city.csv")
