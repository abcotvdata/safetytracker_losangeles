library(tidyverse)
library(lubridate)
library(readxl)

# Source page for LA Sheriff: https://lasd.org/transparency/part1and2crimedata/#part1
#LA Sheriff Last 30 Days
download.file("http://shq.lasdnews.net/CrimeStats/CAASS/PART_I_AND_II_CRIMES.csv","data/source/recent/lasd_recent.csv")
#LA Sheriff Year To Date
download.file("http://shq.lasdnews.net/CrimeStats/CAASS/PART_I_AND_II_CRIMES-YTD.csv","data/source/recent/lasd_ytd.csv")
# LASheriff Annuals
# download.file("http://shq.lasdnews.net/CrimeStats/CAASS/2021-PART_I_AND_II_CRIMES.csv","data/source/annual/lasd_2021.csv")
# download.file("http://shq.lasdnews.net/CrimeStats/CAASS/2020-PART_I_AND_II_CRIMES.csv","data/source/annual/lasd_2020.csv")
# download.file("http://shq.lasdnews.net/CrimeStats/CAASS/2019-PART_I_AND_II_CRIMES.csv","data/source/annual/lasd_2019.csv")

# Read LA Sheriff annual rds files pre-processed
# lasd_past <- readRDS("scripts/rds/lasd_past.rds")
lasd_compstat <- readxl::read_xlsx("data/source/recent/lasd_compstat.xlsx")

# Read in, then merge LA Sheriff newly downloaded year to date and recent files
lasd_recent <- read_csv("data/source/recent/lasd_recent.csv") %>% janitor::clean_names()
lasd_ytd <- read_csv("data/source/recent/lasd_ytd.csv") %>% janitor::clean_names()
lasd_past <- read_csv("data/source/annual/lasd_2021.csv") %>% janitor::clean_names()

# Merge these five files for some tightening and clean_up
lasd_crime <- rbind(lasd_ytd,lasd_recent,lasd_past)

# Trim down to base set of columns we'll need and do some consistent col cleanup
lasd_crime <- lasd_crime %>% select(2,4:7,11,12,15:19) 
lasd_crime <- lasd_crime %>% rename("lasd_category" = "category","district" = "unit_name")
lasd_crime$agency <- "LASD"
lasd_crime$district <-  str_to_title(lasd_crime$district, locale = "en")

# Fix the date fields to match and then filter past file to extract just 2019
lasd_crime$date <- as.Date(lubridate::mdy_hms(lasd_crime$incident_date))
lasd_crime$year <- lubridate::year(lasd_crime$date)
lasd_crime$month <- lubridate::floor_date(as.Date(lasd_crime$date),"month")
lasd_crime$hour <- lubridate::hour(lasd_crime$date)

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
lasd_crime$district <- gsub("Walnut", "Walnut/Diamond Bar", lasd_crime$district)
lasd_crime$district <- str_to_title(lasd_crime$district)

# incident id is unique but there is a tiny number of duplicated incidents; keeping most recent update
lasd_crime <- lasd_crime %>% arrange(desc(lasd_crime$date))
lasd_crime <- lasd_crime[!duplicated(lasd_crime$incident_id),]
# clean up
rm(lasd_ytd,lasd_recent,lasd_past)
# Create LASD Sheriff as_of_date
lasd_asofdate <- max(lasd_crime$date)
saveRDS(lasd_asofdate,"scripts/rds/lasd_asofdate.rds")

# For last 12, we're going to default to the most recent LAPD date available for consistency
# LASD crime lags slightly more, so for the combined stats for the L.A. area, we're going to use the older date
# lasd_crime_last12 <- lasd_crime %>% filter(date>(max(lapd_crime$date)-31536000) & date<=(max(lapd_crime$date)))
lasd_crime_ytd22adjust <- lasd_crime %>% filter(date>"2022-10-31" & date<=(max(lapd_crime$date)))
lasd_crime_ytd21adjust <- lasd_crime %>% filter(date>"2021-10-31" & date<(max(lapd_crime$date)-31536000))
lasd_crime_adjust <- rbind(lasd_crime_ytd21adjust,lasd_crime_ytd22adjust)
# clean up
rm(lasd_crime_ytd21adjust,lasd_crime_ytd22adjust)

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

# we need these unique lists for making the beat tables below
# this ensures that we get crime details for beats even with zero
# incidents of certain types over the entirety of the time period
list_district_category <- crossing(district = unique(lasd_crime_adjust$district), category = unique(lasd_crime_adjust$category))
# list_district_type <- crossing(community_area = unique(chicago_crime$community_area), type = unique(chicago_crime$type))

###
### Sheriff By District
lasd_district_adjust <- lasd_crime_adjust %>%
  group_by(district,agency,category,year) %>%
  summarise(count = n()) %>%
  pivot_wider(names_from=year, values_from=count)
# merging with full list so we have data for every beat, every category_name
lasd_district_adjust <- left_join(list_district_category,lasd_district_adjust,by=c("district"="district","category"="category"))
# rename the year columns
lasd_district_adjust <- lasd_district_adjust %>% 
  rename("adjust21" = "2021",
         "adjust22" = "2022")
# filter out other/part2
lasd_district_adjust <- lasd_district_adjust %>% 
  filter(category!="Other or Part 2")
# fill in blank agency
lasd_district_adjust$agency <- "LASD"
# add zeros where there were no crimes tallied that year
lasd_district_adjust[is.na(lasd_district_adjust)] <- 0

## OPEN WORK
## STOPPED HERE
lasd_compstat_district <- lasd_compstat %>% filter(district!="Departmentwide")
lasd_district_category <- left_join(lasd_compstat_district,lasd_district_adjust,by=c("category","district","agency")) 
lasd_district_category$total22 <- lasd_district_category$ytd22 + lasd_district_category$adjust22
lasd_district_category$last12mos <- lasd_district_category$total22+(lasd_district_category$total21-(lasd_district_category$ytd21+lasd_district_category$adjust21))
lasd_district_category <- lasd_district_category %>% select(1:6,11,12)

# Saving lasd category totals countywide and by district
# And archiving a full, but clean raw file of lasd_crime as an rds in output
saveRDS(lasd_district_category,"scripts/rds/lasd_district_category.rds")
saveRDS(lasd_category,"scripts/rds/lasd_category.rds")
saveRDS(lasd_crime,"data/output/lasd_crime.rds")
