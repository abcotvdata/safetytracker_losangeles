library(tidyverse)
library(lubridate)
library(readxl)
library(sf)
library(RSocrata)

options(timeout=300)
# Reading SoDA valid URLs
lapd_recent <- read.socrata("https://data.lacity.org/resource/2nrs-mtv8.json")

# Load the data
# lapd_recent <- read_csv("data/source/recent/lapd_recent.csv") %>% janitor::clean_names()
# Fix the date fields to match and then filter past file to extract just 2019
lapd_recent$date <- lapd_recent$date_occ
lapd_recent$year <- lubridate::year(lapd_recent$date)
lapd_recent$month <- lubridate::floor_date(as.Date(lapd_recent$date),"month")
lapd_recent$hour <- substr(lapd_recent$time_occ,1,2)

# For last 12 months using most recent date in LAPD new download
lapd_recent <- lapd_recent %>% filter(date>(max(lapd_recent$date)-31536000)) 

lapd_recent$category <- case_when(lapd_recent$crm_cd == '230' ~ 'Aggravated Assault',
                                 lapd_recent$crm_cd == '231' ~ 'Aggravated Assault',
                                 lapd_recent$crm_cd == '235' ~ 'Aggravated Assault',
                                 lapd_recent$crm_cd == '236' ~ 'Aggravated Assault',
                                 lapd_recent$crm_cd == '250' ~ 'Aggravated Assault',
                                 lapd_recent$crm_cd == '251' ~ 'Aggravated Assault',
                                 lapd_recent$crm_cd == '761' ~ 'Aggravated Assault',
                                 lapd_recent$crm_cd == '926' ~ 'Aggravated Assault',
                                 lapd_recent$crm_cd == '310' ~ 'Burglary',
                                 lapd_recent$crm_cd == '320' ~ 'Burglary',
                                 lapd_recent$crm_cd == '110' ~ 'Homicide',
                                 lapd_recent$crm_cd == '113' ~ 'Homicide',
                                 lapd_recent$crm_cd == '330' ~ 'Larceny',
                                 lapd_recent$crm_cd == '331' ~ 'Larceny',
                                 lapd_recent$crm_cd == '410' ~ 'Larceny',
                                 lapd_recent$crm_cd == '420' ~ 'Larceny',
                                 lapd_recent$crm_cd == '421' ~ 'Larceny',
                                 lapd_recent$crm_cd == '350' ~ 'Larceny',
                                 lapd_recent$crm_cd == '351' ~ 'Larceny',
                                 lapd_recent$crm_cd == '352' ~ 'Larceny',
                                 lapd_recent$crm_cd == '353' ~ 'Larceny',
                                 lapd_recent$crm_cd == '450' ~ 'Larceny',
                                 lapd_recent$crm_cd == '451' ~ 'Larceny',
                                 lapd_recent$crm_cd == '452' ~ 'Larceny',
                                 lapd_recent$crm_cd == '453' ~ 'Larceny',
                                 lapd_recent$crm_cd == '341' ~ 'Larceny',
                                 lapd_recent$crm_cd == '343' ~ 'Larceny',
                                 lapd_recent$crm_cd == '345' ~ 'Larceny',
                                 lapd_recent$crm_cd == '440' ~ 'Larceny',
                                 lapd_recent$crm_cd == '441' ~ 'Larceny',
                                 lapd_recent$crm_cd == '442' ~ 'Larceny',
                                 lapd_recent$crm_cd == '443' ~ 'Larceny',
                                 lapd_recent$crm_cd == '444' ~ 'Larceny',
                                 lapd_recent$crm_cd == '445' ~ 'Larceny',
                                 lapd_recent$crm_cd == '470' ~ 'Larceny',
                                 lapd_recent$crm_cd == '471' ~ 'Larceny',
                                 lapd_recent$crm_cd == '472' ~ 'Larceny',
                                 lapd_recent$crm_cd == '473' ~ 'Larceny',
                                 lapd_recent$crm_cd == '474' ~ 'Larceny',
                                 lapd_recent$crm_cd == '475' ~ 'Larceny',
                                 lapd_recent$crm_cd == '480' ~ 'Larceny',
                                 lapd_recent$crm_cd == '485' ~ 'Larceny',
                                 lapd_recent$crm_cd == '487' ~ 'Larceny',
                                 lapd_recent$crm_cd == '491' ~ 'Larceny',
                                 lapd_recent$crm_cd == '210' ~ 'Robbery',
                                 lapd_recent$crm_cd == '220' ~ 'Robbery',
                                 lapd_recent$crm_cd == '121' ~ 'Sexual Assault',
                                 lapd_recent$crm_cd == '122' ~ 'Sexual Assault',
                                 lapd_recent$crm_cd == '815' ~ 'Sexual Assault',
                                 lapd_recent$crm_cd == '820' ~ 'Sexual Assault',
                                 lapd_recent$crm_cd == '821' ~ 'Sexual Assault',
                                 lapd_recent$crm_cd == '510' ~ 'Vehicle Theft',
                                 lapd_recent$crm_cd == '520' ~ 'Vehicle Theft',
                                 TRUE ~ "Other or Part 2")

# Clean address field of stray spacing
lapd_recent$location <- gsub("\\s+", " ", lapd_recent$location) %>% trimws
lapd_recent$cross_street <- gsub("\\s+", " ", lapd_recent$cross_street) %>% trimws         

# Fix two division names in data to match division names in geo file
lapd_recent$area_name <- gsub("N Hollywood", "North Hollywood", lapd_recent$area_name)
lapd_recent$area_name <- gsub("West LA", "West Los Angeles", lapd_recent$area_name)

# Separate shootings before we start grouping by crime category
lapd_recent_shootings <- lapd_recent %>%
  mutate(vict_shot = case_when(str_detect(mocodes, '0430') == TRUE ~ "YES",
                             TRUE ~ "NO")) %>%
                             filter(vict_shot=="YES")
# Tally shootings by district
recent_shootings_district <- lapd_recent_shootings %>%
  group_by(area_name) %>%
  summarise(last12mos = n())  %>% 
  rename("district"="area_name") %>%
  mutate(category = "Shootings")
# Tally shootings citywide
recent_shootings_citywide <- lapd_recent_shootings %>%
  summarise(last12mos = n()) %>%
  mutate(category = "Shootings")

# BEGIN PROCESSING DATA FILES FOR TRACKER PAGES FOR CRIME CATEGORIES
# Remove repetitive fields we no longer need to keep rds file smaller
lapd_recent <- lapd_recent %>% select(5:10,15,16,25:33)
lapd_recent$agency <- "LAPD"

# Create LAPD as_of_date
lapd_asofdate <- max(lapd_recent$date)
saveRDS(lapd_asofdate,"scripts/rds/lapd_asofdate.rds")

### LAPD last 12 by Category
lapd_recent_citywide <- lapd_recent %>%
  filter(category != "Other or Part 2") %>%
  group_by(category) %>%
  summarise(last12mos = n())
lapd_recent_citywide <- rbind(lapd_recent_citywide,recent_shootings_citywide)
### LAPD Districts last 12 by Category
lapd_recent <- lapd_recent %>%
  filter(category != "Other or Part 2") %>%
  group_by(area_name,category) %>%
  summarise(last12mos = n()) %>% 
  rename("district"="area_name")
lapd_recent <- rbind(lapd_recent,recent_shootings_district)

# Load LAPD annual
lapd_annual <- readRDS("scripts/rds/lapd_annual.rds")
lapd_districts <- readRDS("scripts/rds/lapd_districts.rds")

lapd_crime <- left_join(lapd_annual,lapd_recent,by=c("district","category")) %>%
  select(1:15,19,17,18)
lapd_crime <- left_join(lapd_districts %>% select(3,6,7),lapd_crime,by="district")

# add 3-year totals and annualized averages
lapd_crime$total_prior3years <- lapd_crime$`2019`+
  lapd_crime$`2020`+
  lapd_crime$`2021`
lapd_crime$avg_prior3years <- round(((lapd_crime$`2019`+
                                            lapd_crime$`2020`+
                                            lapd_crime$`2021`)/3),1)
# now add the increases or change percentages
lapd_crime$inc_19to21 <- round(lapd_crime$`2021`/lapd_crime$`2019`*100-100,1)
lapd_crime$inc_19to21 <- round(lapd_crime$`2021`/lapd_crime$`2010`*100-100,1)
lapd_crime$inc_19tolast12 <- round(lapd_crime$last12mos/lapd_crime$`2019`*100-100,1)
lapd_crime$inc_21tolast12 <- round(lapd_crime$last12mos/lapd_crime$`2021`*100-100,1)
lapd_crime$inc_prior3yearavgtolast12 <- round((lapd_crime$last12mos/lapd_crime$avg_prior3years)*100-100,0)
# add crime rates for each year
lapd_crime$rate19 <- round((lapd_crime$`2019`/lapd_crime$population)*100000,1)
lapd_crime$rate20 <- round((lapd_crime$`2020`/lapd_crime$population)*100000,1)
lapd_crime$rate21 <- round((lapd_crime$`2021`/lapd_crime$population)*100000,1)
lapd_crime$rate_last12 <- round((lapd_crime$last12mos/lapd_crime$population)*100000,1)
lapd_crime$rate_prior3years <- 
  round((lapd_crime$avg_prior3years/lapd_crime$population)*100000,1)
# Now reduce the precinct down to just the columns we likely need for the tracker pages
# lapd_crime <- lapd_crime %>% select(1,4,5,6,26:28,36:40,44:55,29,42)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
lapd_crime <- lapd_crime %>%
  mutate(across(where(is.numeric), ~na_if(., Inf)))
lapd_crime <- lapd_crime %>%
  mutate(across(where(is.numeric), ~na_if(., "NaN")))

# store a full version of latest lapd_crime as rds
saveRDS(lapd_recent,"scripts/rds/lapd_recent.rds")
saveRDS(lapd_recent,"data/output/lapd_recent.rds")

# Now make individual crime files for trackers' pages
murders_district <- lapd_crime %>% filter(category=="Homicide")
sexassaults_district <- lapd_crime %>% filter(category=="Sexual Assault")
robberies_district <- lapd_crime %>% filter(category=="Robbery")
assaults_district <- lapd_crime %>% filter(category=="Aggravated Assault")
burglaries_district <- lapd_crime %>% filter(category=="Burglary")
thefts_district <- lapd_crime %>% filter(category=="Larceny")
autothefts_district <- lapd_crime %>% filter(category=="Vehicle Theft")
shootings_district <- lapd_crime %>% filter(category=="Shootings")
# district versions saved as rds
saveRDS(murders_district,"scripts/rds/murders_district.rds")
saveRDS(sexassaults_district,"scripts/rds/sexassaults_district.rds")
saveRDS(robberies_district,"scripts/rds/robberies_district.rds")
saveRDS(assaults_district,"scripts/rds/assaults_district.rds")
saveRDS(burglaries_district,"scripts/rds/burglaries_district.rds")
saveRDS(thefts_district,"scripts/rds/thefts_district.rds")
saveRDS(autothefts_district,"scripts/rds/autothefts_district.rds")
saveRDS(shootings_district,"scripts/rds/shootings_district.rds")


# Form LA citywide data for tables/charts for tracker
citywide_crime <- lapd_annual %>%
  group_by(category) %>%
  summarise(`2010`=sum(`2010`),
            `2011`=sum(`2011`),
            `2012`=sum(`2012`),
            `2013`=sum(`2013`),
            `2014`=sum(`2014`),
            `2015`=sum(`2015`),
            `2016`=sum(`2016`),
            `2017`=sum(`2017`),
            `2018`=sum(`2018`),
            `2019`=sum(`2019`),
            `2020`=sum(`2020`),
            `2021`=sum(`2021`))
citywide_crime <- left_join(citywide_crime,lapd_recent_citywide,by="category")
citywide_yearly <- citywide_crime

# Set variable of LA city population
la_population <- 3849297

# add 3-year totals and annualized averages
citywide_crime$total_prior3years <- citywide_crime$`2019`+
  citywide_crime$`2020`+
  citywide_crime$`2021`
citywide_crime$avg_prior3years <- round(((citywide_crime$`2019`+
                                        citywide_crime$`2020`+
                                        citywide_crime$`2021`)/3),1)
# now add the increases or change percentages
citywide_crime$inc_19to21 <- round(citywide_crime$`2021`/citywide_crime$`2019`*100-100,1)
citywide_crime$inc_19to21 <- round(citywide_crime$`2021`/citywide_crime$`2010`*100-100,1)
citywide_crime$inc_19tolast12 <- round(citywide_crime$last12mos/citywide_crime$`2019`*100-100,1)
citywide_crime$inc_21tolast12 <- round(citywide_crime$last12mos/citywide_crime$`2021`*100-100,1)
citywide_crime$inc_prior3yearavgtolast12 <- round((citywide_crime$last12mos/citywide_crime$avg_prior3years)*100-100,0)
# add crime rates for each year
citywide_crime$rate19 <- round((citywide_crime$`2019`/la_population)*100000,1)
citywide_crime$rate20 <- round((citywide_crime$`2020`/la_population)*100000,1)
citywide_crime$rate21 <- round((citywide_crime$`2021`/la_population)*100000,1)
citywide_crime$rate_last12 <- round((citywide_crime$last12mos/la_population)*100000,1)
citywide_crime$rate_prior3years <- 
  round((citywide_crime$avg_prior3years/la_population)*100000,1)

# filter citywide versions for building LA city tracker pages
murders_city <- citywide_crime %>% filter(category=="Homicide")
sexassaults_city <- citywide_crime %>% filter(category=="Sexual Assault")
robberies_city <- citywide_crime %>% filter(category=="Robbery")
assaults_city <- citywide_crime %>% filter(category=="Aggravated Assault")
burglaries_city <- citywide_crime %>% filter(category=="Burglary")
thefts_city <- citywide_crime %>% filter(category=="Larceny")
autothefts_city <- citywide_crime %>% filter(category=="Vehicle Theft")
shootings_city <- citywide_crime %>% filter(category=="Shootings")
# city versions saved as rds for use in tracker pages
saveRDS(murders_city,"scripts/rds/murders_city.rds")
saveRDS(sexassaults_city,"scripts/rds/sexassaults_city.rds")
saveRDS(robberies_city,"scripts/rds/robberies_city.rds")
saveRDS(assaults_city,"scripts/rds/assaults_city.rds")
saveRDS(burglaries_city,"scripts/rds/burglaries_city.rds")
saveRDS(thefts_city,"scripts/rds/thefts_city.rds")
saveRDS(autothefts_city,"scripts/rds/autothefts_city.rds")
saveRDS(shootings_city,"scripts/rds/shootings_city.rds")
### Some yearly csv tables for charts for our datawrapper charts
citywide_yearly %>% write_csv("data/output/yearly/citywide_yearly.csv")
citywide_yearly %>% filter(category=="Homicide") %>% write_csv("data/output/yearly/murders_city.csv")
citywide_yearly %>% filter(category=="Sexual Assault") %>%  write_csv("data/output/yearly/sexassaults_city.csv")
citywide_yearly %>% filter(category=="Vehicle Theft") %>%  write_csv("data/output/yearly/autothefts_city.csv")
citywide_yearly %>% filter(category=="Larceny") %>%  write_csv("data/output/yearly/thefts_city.csv")
citywide_yearly %>% filter(category=="Burglary") %>%  write_csv("data/output/yearly/burglaries_city.csv")
citywide_yearly %>% filter(category=="Robbery") %>%  write_csv("data/output/yearly/robberies_city.csv")
citywide_yearly %>% filter(category=="Aggravated Assault") %>%  write_csv("data/output/yearly/assaults_city.csv")
citywide_yearly %>% filter(category=="Shootings") %>%  write_csv("data/output/yearly/shootings_city.csv")


# make the death rate comparable file unique to this state vs LAPD homicide
deaths <- read_excel("data/source/health/deaths.xlsx") 
deaths <- deaths %>% filter(state=="CA")
deaths$Homicide <- murders_city$rate_last12
write_csv(deaths,"data/source/health/death_rates.csv")


