library(tidyverse)
library(lubridate)

options(timeout=300)
# Source page for LA Sheriff
# https://lasd.org/transparency/part1and2crimedata/#part1
#LA Sheriff Last 30 Days
download.file("http://shq.lasdnews.net/CrimeStats/CAASS/PART_I_AND_II_CRIMES.csv","data/source/recent/lasd_recent.csv")
#LA Sheriff Year To Date
download.file("http://shq.lasdnews.net/CrimeStats/CAASS/PART_I_AND_II_CRIMES-YTD.csv","data/source/recent/lasd_ytd.csv")

# Read LA Sheriff annual rds files pre-processed
lasd_past <- readRDS("scripts/rds/lasd_past.rds")

# Read in, then merge LA Sheriff newly downloaded year to date and recent files
lasd_recent <- read_csv("data/source/recent/lasd_recent.csv") %>% janitor::clean_names()
lasd_ytd <- read_csv("data/source/recent/lasd_ytd.csv") %>% janitor::clean_names()

#lasd_recent$date <- as.Date(lubridate::mdy_hms(lasd_recent$incident_date))
#lasd_ytd$date <- as.Date(lubridate::mdy_hms(lasd_ytd$incident_date))
#lasd_recent$reported_date <- lubridate::mdy(lasd_recent$incident_reported_date)
#lasd_ytd$reported_date <- lubridate::mdy(lasd_ytd$incident_reported_date)

# Merge these five files for some tightening and clean_up
lasd_crime <- rbind(lasd_ytd,lasd_recent)

# Trim down to base set of columns we'll need
lasd_crime <- lasd_crime %>% select(2,4:7,11,12,15:19) 
lasd_crime <- lasd_crime %>% unique
# removes roughly 3K duplicates/overlaps from ytd file to recent file

# Merge the cleaned recent file with the past years' archive files
lasd_crime <- rbind(lasd_crime,lasd_past)
lasd_crime <- lasd_crime %>% rename("lasd_category" = "category")

# Fix the date fields to match and then filter past file to extract just 2019
lasd_crime$date <- as.Date(lubridate::mdy_hms(lasd_crime$incident_date))
lasd_crime$year <- lubridate::year(lasd_crime$date)
lasd_crime$month <- lubridate::floor_date(as.Date(lasd_crime$date),"month")
lasd_crime$hour <- lubridate::hour(lasd_crime$date)
# Clean out some records with bad dates listed
lasd_crime <- lasd_crime %>% filter(year>2018) # was 596K; about 4K records with bad dates; less than 1% total

lasd_crime$agency <- "LASD"

lasd_crime$unit_name <-  str_to_title(lasd_crime$unit_name, locale = "en")

# rm(lasd_ytd,lasd_recent,lasd_2021,lasd_2020,lasd_2019)

# incident id is unique
# lasd categorization very straightforward; categories of Cat I crimes are unique and can be the filter
# then later renamed to match our needs
lasd_crime$category <- case_when(lasd_crime$lasd_category == 'AGGRAVATED ASSAULT' ~ 'Aggravated Assault',
                                 lasd_crime$lasd_category == 'BURGLARY' ~ 'Burglary',
                                 lasd_crime$lasd_category == 'ROBBERY' ~ 'Robbery',
                                 lasd_crime$lasd_category == 'CRIMINAL HOMICIDE' ~ 'Homicide',
                                 lasd_crime$lasd_category == 'LARCENY THEFT' ~ 'Larceny',
                                 lasd_crime$lasd_category == 'FORCIBLE RAPE' ~ 'Sexual Assault',
                                 lasd_crime$lasd_category == 'GRAND THEFT AUTO' ~ 'Vehicle Theft',
                                 TRUE ~ "Other or Part 2")

# Fix two division names in data to match division names in geo file
lasd_crime$unit_name <- gsub("/", " / ", lasd_crime$unit_name)
lasd_crime$unit_name <- gsub("Walnut", "Walnut / Diamond Bar", lasd_crime$unit_name)
lasd_crime$unit_name <- ifelse(lasd_crime$unit_name=="Lost Hills", "Malibu / Lost Hills", lasd_crime$unit_name)
lasd_crime$unit_name <- str_to_title(lasd_crime$unit_name)

# Create LASD Sheriff as_of_date
lasd_asofdate <- max(lasd_crime$date)
saveRDS(lasd_asofdate,"scripts/rds/lasd_asofdate.rds")

# districts <- readRDS("scripts/rds/la_police_districts.rds")
# check <- anti_join(lasd_area_list,districts,by=c("unit_name"="st_name","agency"="agency"))

saveRDS(lasd_crime,"scripts/rds/lasd_crime.rds")
saveRDS(lasd_crime,"data/output/lasd_crime.rds")
