library(tidyverse)
library(sf)
library(lubridate)

# LAPD 2020 to now: https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8
download.file("https://data.lacity.org/api/views/2nrs-mtv8/rows.csv","data/source/recent/lapd_recent.csv")

# Load the data
lapd_recent <- read_csv("data/source/recent/lapd_recent.csv") %>% janitor::clean_names()
# Fix the date fields to match and then filter past file to extract just 2019
lapd_recent$date <- lubridate::mdy_hms(lapd_recent$date_occ)
lapd_recent$year <- lubridate::year(lapd_recent$date)
lapd_recent$month <- lubridate::floor_date(as.Date(lapd_recent$date),"month")
lapd_recent$hour <- substr(lapd_recent$time_occ,1,2)

# Load past file and then merge with new for formatting, cleanup
lapd_past <- readRDS("scripts/rds/lapd_past.rds")
lapd_crime <- rbind(lapd_past,lapd_recent)
# dr_no is a unique identifier

# OPEN WORK: skinny file to just what we need - or not??
# lapd_crime <- lasd_crime %>% select(2,4:7,11,12,14:19) 
# lapd_crime <- lapd_crime %>% unique # this is not needed; appear to be no duplicates

lapd_crime$category <- case_when(lapd_crime$crm_cd == '230' ~ 'Aggravated Assault',
                                 lapd_crime$crm_cd == '231' ~ 'Aggravated Assault',
                                 lapd_crime$crm_cd == '235' ~ 'Aggravated Assault',
                                 lapd_crime$crm_cd == '236' ~ 'Aggravated Assault',
                                 lapd_crime$crm_cd == '250' ~ 'Aggravated Assault',
                                 lapd_crime$crm_cd == '251' ~ 'Aggravated Assault',
                                 lapd_crime$crm_cd == '761' ~ 'Aggravated Assault',
                                 lapd_crime$crm_cd == '926' ~ 'Aggravated Assault',
                                 lapd_crime$crm_cd == '310' ~ 'Burglary',
                                 lapd_crime$crm_cd == '320' ~ 'Burglary',
                                 lapd_crime$crm_cd == '110' ~ 'Homicide',
                                 lapd_crime$crm_cd == '113' ~ 'Homicide',
                                 lapd_crime$crm_cd == '330' ~ 'Larceny',
                                 lapd_crime$crm_cd == '331' ~ 'Larceny',
                                 lapd_crime$crm_cd == '410' ~ 'Larceny',
                                 lapd_crime$crm_cd == '420' ~ 'Larceny',
                                 lapd_crime$crm_cd == '421' ~ 'Larceny',
                                 lapd_crime$crm_cd == '350' ~ 'Larceny',
                                 lapd_crime$crm_cd == '351' ~ 'Larceny',
                                 lapd_crime$crm_cd == '352' ~ 'Larceny',
                                 lapd_crime$crm_cd == '353' ~ 'Larceny',
                                 lapd_crime$crm_cd == '450' ~ 'Larceny',
                                 lapd_crime$crm_cd == '451' ~ 'Larceny',
                                 lapd_crime$crm_cd == '452' ~ 'Larceny',
                                 lapd_crime$crm_cd == '453' ~ 'Larceny',
                                 lapd_crime$crm_cd == '341' ~ 'Larceny',
                                 lapd_crime$crm_cd == '343' ~ 'Larceny',
                                 lapd_crime$crm_cd == '345' ~ 'Larceny',
                                 lapd_crime$crm_cd == '440' ~ 'Larceny',
                                 lapd_crime$crm_cd == '441' ~ 'Larceny',
                                 lapd_crime$crm_cd == '442' ~ 'Larceny',
                                 lapd_crime$crm_cd == '443' ~ 'Larceny',
                                 lapd_crime$crm_cd == '444' ~ 'Larceny',
                                 lapd_crime$crm_cd == '445' ~ 'Larceny',
                                 lapd_crime$crm_cd == '470' ~ 'Larceny',
                                 lapd_crime$crm_cd == '471' ~ 'Larceny',
                                 lapd_crime$crm_cd == '472' ~ 'Larceny',
                                 lapd_crime$crm_cd == '473' ~ 'Larceny',
                                 lapd_crime$crm_cd == '474' ~ 'Larceny',
                                 lapd_crime$crm_cd == '475' ~ 'Larceny',
                                 lapd_crime$crm_cd == '480' ~ 'Larceny',
                                 lapd_crime$crm_cd == '485' ~ 'Larceny',
                                 lapd_crime$crm_cd == '487' ~ 'Larceny',
                                 lapd_crime$crm_cd == '491' ~ 'Larceny',
                                 lapd_crime$crm_cd == '210' ~ 'Robbery',
                                 lapd_crime$crm_cd == '220' ~ 'Robbery',
                                 lapd_crime$crm_cd == '121' ~ 'Sexual Assault',
                                 lapd_crime$crm_cd == '122' ~ 'Sexual Assault',
                                 lapd_crime$crm_cd == '815' ~ 'Sexual Assault',
                                 lapd_crime$crm_cd == '820' ~ 'Sexual Assault',
                                 lapd_crime$crm_cd == '821' ~ 'Sexual Assault',
                                 lapd_crime$crm_cd == '510' ~ 'Vehicle Theft',
                                 lapd_crime$crm_cd == '520' ~ 'Vehicle Theft',
                                 TRUE ~ "Other or Part 2")

# Clean address field of stray spacing
lapd_crime$location <- gsub("  "," ",lapd_crime$location)


# OPEN WORK: clean up premise names throughout file
# the case when is stored once as a value by separate script
#lapd_crime$premise <- case_when(lapd_crime$premise == 'Amusement Park' ~ 'Amusement park',
#                                   lapd_crime$premise == 'Bank, Savings & Loan' ~ 'Bank',
#                                   lapd_crime$premise == 'Bar, Nightclub' ~ 'Bar or nightclub',
#                                   lapd_crime$premise == 'Church, Synagogue, Temple' ~ 'Place of worship',
#                                   lapd_crime$premise == 'Commercial, Office Building' ~ 'Commercial or office building',
#                                   lapd_crime$premise == 'Convenience Store' ~ 'Convenience store',
#                                   lapd_crime$premise == 'Department, Discount Store' ~ 'Store',
#                                   lapd_crime$premise == 'Drug Store, Doctors Office, Hospital' ~ 'Medical care facility',
#                                   lapd_crime$premise == 'Field, Woods' ~ 'Field or woods',
#                                   lapd_crime$premise == 'Gambling Facility/Casino/Race Track' ~ 'Gambling facility',
#                                   lapd_crime$premise == 'Grocery, Supermarket' ~ 'Grocery',
#                                   lapd_crime$premise == 'Highway, Road, Street, Alley' ~ 'Highway, street or alley',
#                                   lapd_crime$premise == 'Hotel, Motel, ETC' ~ 'Hotel',
#                                   lapd_crime$premise == 'Industrial Site' ~ 'Industrial site',
#                                   TRUE ~ 'Unknown or other')

saveRDS(lapd_crime,"scripts/rds/lapd_crime.rds")
saveRDS(lapd_crime,"output/lapd_crime.rds")
