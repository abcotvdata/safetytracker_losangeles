library(tidyverse)
library(lubridate)

lapd_past <- read_csv("data/source/annual/lapd_crime_2010_2019_rollup.csv") %>% janitor::clean_names()

lapd_past <- lapd_past %>% rename("year"="date_occ","number_incidents"="dr_no","district"="area_name")

lapd_past$category <- case_when(lapd_past$crm_cd == '230' ~ 'Aggravated Assault',
                                 lapd_past$crm_cd == '231' ~ 'Aggravated Assault',
                                 lapd_past$crm_cd == '235' ~ 'Aggravated Assault',
                                 lapd_past$crm_cd == '236' ~ 'Aggravated Assault',
                                 lapd_past$crm_cd == '250' ~ 'Aggravated Assault',
                                 lapd_past$crm_cd == '251' ~ 'Aggravated Assault',
                                 lapd_past$crm_cd == '761' ~ 'Aggravated Assault',
                                 lapd_past$crm_cd == '926' ~ 'Aggravated Assault',
                                 lapd_past$crm_cd == '310' ~ 'Burglary',
                                 lapd_past$crm_cd == '320' ~ 'Burglary',
                                 lapd_past$crm_cd == '110' ~ 'Homicide',
                                 lapd_past$crm_cd == '113' ~ 'Homicide',
                                 lapd_past$crm_cd == '330' ~ 'Larceny',
                                 lapd_past$crm_cd == '331' ~ 'Larceny',
                                 lapd_past$crm_cd == '410' ~ 'Larceny',
                                 lapd_past$crm_cd == '420' ~ 'Larceny',
                                 lapd_past$crm_cd == '421' ~ 'Larceny',
                                 lapd_past$crm_cd == '350' ~ 'Larceny',
                                 lapd_past$crm_cd == '351' ~ 'Larceny',
                                 lapd_past$crm_cd == '352' ~ 'Larceny',
                                 lapd_past$crm_cd == '353' ~ 'Larceny',
                                 lapd_past$crm_cd == '450' ~ 'Larceny',
                                 lapd_past$crm_cd == '451' ~ 'Larceny',
                                 lapd_past$crm_cd == '452' ~ 'Larceny',
                                 lapd_past$crm_cd == '453' ~ 'Larceny',
                                 lapd_past$crm_cd == '341' ~ 'Larceny',
                                 lapd_past$crm_cd == '343' ~ 'Larceny',
                                 lapd_past$crm_cd == '345' ~ 'Larceny',
                                 lapd_past$crm_cd == '440' ~ 'Larceny',
                                 lapd_past$crm_cd == '441' ~ 'Larceny',
                                 lapd_past$crm_cd == '442' ~ 'Larceny',
                                 lapd_past$crm_cd == '443' ~ 'Larceny',
                                 lapd_past$crm_cd == '444' ~ 'Larceny',
                                 lapd_past$crm_cd == '445' ~ 'Larceny',
                                 lapd_past$crm_cd == '470' ~ 'Larceny',
                                 lapd_past$crm_cd == '471' ~ 'Larceny',
                                 lapd_past$crm_cd == '472' ~ 'Larceny',
                                 lapd_past$crm_cd == '473' ~ 'Larceny',
                                 lapd_past$crm_cd == '474' ~ 'Larceny',
                                 lapd_past$crm_cd == '475' ~ 'Larceny',
                                 lapd_past$crm_cd == '480' ~ 'Larceny',
                                 lapd_past$crm_cd == '485' ~ 'Larceny',
                                 lapd_past$crm_cd == '487' ~ 'Larceny',
                                 lapd_past$crm_cd == '491' ~ 'Larceny',
                                 lapd_past$crm_cd == '210' ~ 'Robbery',
                                 lapd_past$crm_cd == '220' ~ 'Robbery',
                                 lapd_past$crm_cd == '121' ~ 'Sexual Assault',
                                 lapd_past$crm_cd == '122' ~ 'Sexual Assault',
                                 lapd_past$crm_cd == '815' ~ 'Sexual Assault',
                                 lapd_past$crm_cd == '820' ~ 'Sexual Assault',
                                 lapd_past$crm_cd == '821' ~ 'Sexual Assault',
                                 lapd_past$crm_cd == '510' ~ 'Vehicle Theft',
                                 lapd_past$crm_cd == '520' ~ 'Vehicle Theft',
                                 TRUE ~ "Other or Part 2")

# Fix two division names in data to match division names in geo file
lapd_past$district <- gsub("N Hollywood", "North Hollywood", lapd_past$district)
lapd_past$district <- gsub("West LA", "West Los Angeles", lapd_past$district)

# Remove repetitive fields we no longer need to keep rds file smaller
lapd_past$agency <- "LAPD"

###
### LAPD By Category
lapd_past_category <- lapd_past %>%
  group_by(category,year) %>%
  summarise(count = sum(number_incidents)) %>%
  pivot_wider(names_from=year, values_from=count)
lapd_past_detailed <- lapd_past %>%
  group_by(crm_cd,crm_cd_desc,year) %>%
  summarise(count = sum(number_incidents)) %>%
  pivot_wider(names_from=year, values_from=count)

saveRDS(lapd_past,"scripts/rds/lapd_past.rds")
saveRDS(lapd_past,"data/output/lapd_past.rds")