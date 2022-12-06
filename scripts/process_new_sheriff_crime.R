library(tidyverse)
library(lubridate)
library(readxl)

options(timeout=300)
# Source page for LA Sheriff
# https://lasd.org/transparency/part1and2crimedata/#part1
#LA Sheriff Last 30 Days
download.file("http://shq.lasdnews.net/CrimeStats/CAASS/PART_I_AND_II_CRIMES.csv","data/source/recent/lasd_recent.csv")
#LA Sheriff Year To Date
download.file("http://shq.lasdnews.net/CrimeStats/CAASS/PART_I_AND_II_CRIMES-YTD.csv","data/source/recent/lasd_ytd.csv")

# Read LA Sheriff annual rds files pre-processed
# lasd_past <- readRDS("scripts/rds/lasd_past.rds")
lasd_compstat <- readxl::read_xlsx("data/source/lasd_past_years.xlsx")

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

# For last 12, we're going to default to the most recent LAPD date available for consistency
# LASD crime lags slightly more, so for the combined stats for the L.A. area, we're going to use the older date
# lasd_crime_last12 <- lasd_crime %>% filter(date>(max(lapd_crime$date)-31536000) & date<=(max(lapd_crime$date)))
lasd_crime_ytd22adjust <- lasd_crime %>% filter(date>"2022-10-31" & date<=(max(lapd_crime$date)))
lasd_crime_ytd21adjust <- lasd_crime %>% filter(date>"2021-10-31" & date<(max(lapd_crime$date)-31536000))
lasd_crime_adjust <- rbind(lasd_crime_ytd21adjust,lasd_crime_ytd22adjust)

###
### Sheriff By Category for adjust records only
lasd_category_adjust <- lasd_crime_adjust %>%
  group_by(category,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
lasd_category_adjust <- lasd_category_adjust %>% 
  rename("adjust21" = "2021",
         "adjust22" = "2022")
# filter out other/part2
lasd_category_adjust <- lasd_category_adjust %>% 
  filter(category!="Other or Part 2")

lasd_compstat_countywide <- lasd_compstat %>% filter(district=="Departmentwide")
lasd_category <- left_join(lasd_category_adjust,lasd_compstat_countywide,by=c("category")) 
lasd_category$total22 <- lasd_category$ytd22 + lasd_category$adjust22
lasd_category$last12mos <- lasd_category$total22+(lasd_category$total21-(lasd_category$ytd21+lasd_category$adjust21))
lasd_category <- lasd_category %>% select(1,6,7,8,11,12)

###
### Sheriff By District
lasd_district_category_adjust <- lasd_crime_adjust %>%
  group_by(unit_name,agency,category,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# rename the year columns
lasd_district_category_adjust <- lasd_district_category_adjust %>% 
  rename("adjust21" = "2021",
         "adjust22" = "2022")
# filter out other/part2
lasd_district_category_adjust <- lasd_district_category_adjust %>% 
  filter(category!="Other or Part 2")
# add zeros where there were no crimes tallied that year
lasd_district_category_adjust[is.na(lasd_district_category_adjust)] <- 0
# rename district column for consistency across code
lasd_district_category_adjust <- lasd_district_category_adjust %>% rename("district"="unit_name")

## OPEN WORK
## STOPPED HERE
lasd_compstat_countywide <- lasd_compstat %>% filter(district=="Departmentwide")
lasd_category <- left_join(lasd_category_adjust,lasd_compstat_countywide,by=c("category")) 
lasd_category$total22 <- lasd_category$ytd22 + lasd_category$adjust22
lasd_category$last12mos <- lasd_category$total22+(lasd_category$total21-(lasd_category$ytd21+lasd_category$adjust21))
lasd_category <- lasd_category %>% select(1,6,7,8,11,12)


# districts <- readRDS("scripts/rds/la_police_districts.rds")
# check <- anti_join(lasd_area_list,districts,by=c("unit_name"="st_name","agency"="agency"))

saveRDS(lasd_crime,"scripts/rds/lasd_crime.rds")
saveRDS(lasd_crime,"data/output/lasd_crime.rds")
