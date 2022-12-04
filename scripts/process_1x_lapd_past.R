library(tidyverse)
library(lubridate)

# LAPD Prior to 2020 data
# One time download to capture data and extract just 2019 records we need
download.file("https://data.lacity.org/api/views/63jg-8b9z/rows.csv","data/source/annual/lapd_past.csv")

# Load the data stored on local machine
lapd_past <- read_csv("data/source/annual/lapd_past.csv") %>% janitor::clean_names()

# Fix the date fields to match and then filter past file to extract just 2019
lapd_past$date <- lubridate::mdy_hms(lapd_past$date_occ)
lapd_past$year <- lubridate::year(lapd_past$date)
lapd_past$month <- lubridate::floor_date(as.Date(lapd_past$date),"month")
lapd_past$hour <- substr(lapd_past$time_occ,1,2)
lapd_past <- lapd_past %>% filter(year>2018)

saveRDS(lapd_past,"scripts/rds/lapd_past.rds")