library(tidyverse)
library(lubridate)

# Read in pre-built archive files drawn from LA's city open data site
lapd_annual19 <- read_csv("data/source/annual/lapd_crime_2010_2019_rollup.csv") %>% janitor::clean_names()
lapd_annual22 <- read_csv("data/source/annual/lapd_crime_2020_2022_rollup.csv") %>% janitor::clean_names()

lapd_annual <- rbind(lapd_annual19,lapd_annual22)
rm(lapd_annual19,lapd_annual22)

lapd_annual <- lapd_annual %>% rename("year"="date_occ","number_incidents"="dr_no","district"="area_name")

lapd_annual$category <- case_when(lapd_annual$crm_cd == '230' ~ 'Aggravated Assault',
                                 lapd_annual$crm_cd == '231' ~ 'Aggravated Assault',
                                 lapd_annual$crm_cd == '235' ~ 'Aggravated Assault',
                                 lapd_annual$crm_cd == '236' ~ 'Aggravated Assault',
                                 lapd_annual$crm_cd == '250' ~ 'Aggravated Assault',
                                 lapd_annual$crm_cd == '251' ~ 'Aggravated Assault',
                                 lapd_annual$crm_cd == '761' ~ 'Aggravated Assault',
                                 lapd_annual$crm_cd == '926' ~ 'Aggravated Assault',
                                 lapd_annual$crm_cd == '310' ~ 'Burglary',
                                 lapd_annual$crm_cd == '320' ~ 'Burglary',
                                 lapd_annual$crm_cd == '110' ~ 'Homicide',
                                 lapd_annual$crm_cd == '113' ~ 'Homicide',
                                 lapd_annual$crm_cd == '330' ~ 'Larceny',
                                 lapd_annual$crm_cd == '331' ~ 'Larceny',
                                 lapd_annual$crm_cd == '410' ~ 'Larceny',
                                 lapd_annual$crm_cd == '420' ~ 'Larceny',
                                 lapd_annual$crm_cd == '421' ~ 'Larceny',
                                 lapd_annual$crm_cd == '350' ~ 'Larceny',
                                 lapd_annual$crm_cd == '351' ~ 'Larceny',
                                 lapd_annual$crm_cd == '352' ~ 'Larceny',
                                 lapd_annual$crm_cd == '353' ~ 'Larceny',
                                 lapd_annual$crm_cd == '450' ~ 'Larceny',
                                 lapd_annual$crm_cd == '451' ~ 'Larceny',
                                 lapd_annual$crm_cd == '452' ~ 'Larceny',
                                 lapd_annual$crm_cd == '453' ~ 'Larceny',
                                 lapd_annual$crm_cd == '341' ~ 'Larceny',
                                 lapd_annual$crm_cd == '343' ~ 'Larceny',
                                 lapd_annual$crm_cd == '345' ~ 'Larceny',
                                 lapd_annual$crm_cd == '440' ~ 'Larceny',
                                 lapd_annual$crm_cd == '441' ~ 'Larceny',
                                 lapd_annual$crm_cd == '442' ~ 'Larceny',
                                 lapd_annual$crm_cd == '443' ~ 'Larceny',
                                 lapd_annual$crm_cd == '444' ~ 'Larceny',
                                 lapd_annual$crm_cd == '445' ~ 'Larceny',
                                 lapd_annual$crm_cd == '470' ~ 'Larceny',
                                 lapd_annual$crm_cd == '471' ~ 'Larceny',
                                 lapd_annual$crm_cd == '472' ~ 'Larceny',
                                 lapd_annual$crm_cd == '473' ~ 'Larceny',
                                 lapd_annual$crm_cd == '474' ~ 'Larceny',
                                 lapd_annual$crm_cd == '475' ~ 'Larceny',
                                 lapd_annual$crm_cd == '480' ~ 'Larceny',
                                 lapd_annual$crm_cd == '485' ~ 'Larceny',
                                 lapd_annual$crm_cd == '487' ~ 'Larceny',
                                 lapd_annual$crm_cd == '491' ~ 'Larceny',
                                 lapd_annual$crm_cd == '210' ~ 'Robbery',
                                 lapd_annual$crm_cd == '220' ~ 'Robbery',
                                 lapd_annual$crm_cd == '121' ~ 'Sexual Assault',
                                 lapd_annual$crm_cd == '122' ~ 'Sexual Assault',
                                 lapd_annual$crm_cd == '815' ~ 'Sexual Assault',
                                 lapd_annual$crm_cd == '820' ~ 'Sexual Assault',
                                 lapd_annual$crm_cd == '821' ~ 'Sexual Assault',
                                 lapd_annual$crm_cd == '510' ~ 'Vehicle Theft',
                                 lapd_annual$crm_cd == '520' ~ 'Vehicle Theft',
                                 TRUE ~ "Other or Part 2")

# Fix two division names in data to match division names in geo file
lapd_annual$district <- gsub("N Hollywood", "North Hollywood", lapd_annual$district)
lapd_annual$district <- gsub("West LA", "West Los Angeles", lapd_annual$district)

# Remove repetitive fields we no longer need to keep rds file smaller
lapd_annual$agency <- "LAPD"

###
### LAPD By Category
lapd_annual <- lapd_annual %>%
  filter(category != "Other or Part 2") %>%
  group_by(agency,district,category,year) %>%
  summarise(count = sum(number_incidents)) %>%
  pivot_wider(names_from=year, values_from=count)

# Create a quick tally of shootings that can be appended as a category
lapd_shootings19 <- read_csv("data/source/annual/lapd_shootings_district_2010_2019.csv") %>% janitor::clean_names()
lapd_shootings22 <- read_csv("data/source/annual/lapd_shootings_district_2020_2022.csv") %>% janitor::clean_names()
lapd_shootings_annual <- rbind(lapd_shootings19,lapd_shootings22)
rm(lapd_shootings19,lapd_shootings22)
# rename some cols for consistency for merging
lapd_shootings_annual <- lapd_shootings_annual %>% rename("year"="date_occ","number_incidents"="dr_no","district"="area_name")
# spread to annual for consistency for merging
lapd_shootings_annual <- lapd_shootings_annual %>% pivot_wider(names_from=year, values_from=number_incidents)
# Fix two division names in data to match division names in geo file
lapd_shootings_annual$district <- gsub("N Hollywood", "North Hollywood", lapd_shootings_annual$district)
lapd_shootings_annual$district <- gsub("West LA", "West Los Angeles", lapd_shootings_annual$district)
# Add fields we need
lapd_shootings_annual$agency <- "LAPD"
lapd_shootings_annual$category <- "Shootings"
#lapd_shootings_annual_citywide <- lapd_shootings_annual %>%
#  group_by(year) %>%
#  summarise(shootings = sum(number_incidents)) %>%
#  pivot_wider(names_from=year, values_from=shootings) %>%
#  mutate(district="Citywide")

# Merge the shootings category into the annual table
lapd_annual <- rbind(lapd_annual,lapd_shootings_annual)

# Add place & county cols that match the new region wide police map file
lapd_annual$place <- paste(lapd_annual$agency,lapd_annual$district)
lapd_annual$county <- "Los Angeles County"

# Create a separate file for each crime to map to statewide file and tables
lapd_murder <- lapd_annual %>% filter(category=="Homicide") %>% ungroup %>% select(17,18,4:16,1)
lapd_sexassault <- lapd_annual %>% filter(category=="Sexual Assault") %>% ungroup %>% select(17,18,4:16,1)
lapd_assault <- lapd_annual %>% filter(category=="Aggravated Assault") %>% ungroup %>% select(17,18,4:16,1)
lapd_robbery <- lapd_annual %>% filter(category=="Robbery") %>% ungroup %>% select(17,18,4:16,1)
lapd_burglary <- lapd_annual %>% filter(category=="Burglary") %>% ungroup %>% select(17,18,4:16,1)
lapd_theft <- lapd_annual %>% filter(category=="Larceny") %>% ungroup %>% select(17,18,4:16,1)
lapd_autotheft <- lapd_annual %>% filter(category=="Vehicle Theft") %>% ungroup %>% select(17,18,4:16,1)
lapd_shootings <- lapd_annual %>% filter(category=="Shootings") %>% ungroup %>% select(17,18,4:16,1)

saveRDS(lapd_annual,"scripts/rds/lapd_annual.rds")
