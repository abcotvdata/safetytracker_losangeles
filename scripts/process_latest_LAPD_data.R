library(tidyverse)
library(lubridate)
library(sf)

## DOWNLOAD LAPD DATA
# Re-collect the latest version of BOTH crime incident data files from LAPD
# Setting a longer timeout here because the file is large and sometimes the Action fails if the download is slow
options(timeout=300)
# Get the older (2010-2019) file
# Because half gig file will sometimes crash the Action, using try command here; as long as it's successful ~ 1/week, will catch rare updates of older records
# Source: https://data.lacity.org/Public-Safety/Crime-Data-from-2010-to-2019/63jg-8b9z
try(download.file("https://data.lacity.org/api/views/63jg-8b9z/rows.csv?accessType=DOWNLOAD","data/source/lapd_past.csv"))
# Get the latest LAPD crime incidents file (2020-Present)
# Source: https://data.lacity.org/Public-Safety/Crime-Data-from-2020-to-Present/2nrs-mtv8
download.file("https://data.lacity.org/api/views/2nrs-mtv8/rows.csv?accessType=DOWNLOAD","data/source/lapd_recent.csv")

## LOAD THE LAPD DATA FILES
lapd_past <- read_csv("data/source/lapd_past.csv") %>% janitor::clean_names()
lapd_current <- read_csv("data/source/lapd_recent.csv") %>% janitor::clean_names()

## STANDARDIZE NECESSARY DATES
# Format new date field, and create year column from that new date field in each file
lapd_current$date <- as_date(mdy_hms(lapd_current$date_occ))
lapd_current$year <- year(lapd_current$date)
lapd_past$date <- as_date(mdy_hms(lapd_past$date_occ))
lapd_past$year <- year(lapd_past$date)
# Remove some stray newer records from 2010-2019; manual testing confirms they're strays/duplicates
lapd_past <- lapd_past %>% filter(year<2020)

## MERGE INTO ONE FILE FOR PROCESSING
# And rename the area_name field to district for later merge/use with other files
lapd_annual <- rbind(lapd_past,lapd_current) %>% rename("district"="area_name")
rm(lapd_past,lapd_current)
# Edit two division names in data to match division names in geo file
lapd_annual$district <- gsub("N Hollywood", "North Hollywood", lapd_annual$district)
lapd_annual$district <- gsub("West LA", "West Los Angeles", lapd_annual$district)
# Add agency for later distinction from LASD, regional agencies
lapd_annual$agency <- "LAPD"
# Add a flag for incidents occurring within the last 12 months; so we want a date greater than 365 days ago
lapd_annual$last12flag <- ifelse(lapd_annual$date>max(lapd_annual$date)-365,"yes","no")
# Create LAPD as_of_date
lapd_asofdate <- max(lapd_annual$date)
saveRDS(lapd_asofdate,"scripts/rds/lapd_asofdate.rds")

## EXTRACT SHOOTINGS INTO THEIR OWN DF BEFORE PROCESSING/PIVOTING LAPD ANNUAL FILE
# Necessary for tallying as a separate category because they cut across other crime categories
lapd_shootings <- lapd_annual %>%
  mutate(vict_shot = case_when(str_detect(mocodes, '0430') == TRUE ~ "YES",
                               TRUE ~ "NO")) %>% filter(vict_shot=="YES")
# Tally up the annual shootings by district
lapd_shootings_annual <- lapd_shootings %>%
  filter(vict_shot=="YES") %>%
  group_by(agency,district,year) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from=year, values_from=count) %>% 
  mutate(category = "Shootings")
# Separately, tally the annual shootings in records flagged last 12 months
lapd_shootings_last12 <- lapd_shootings %>%
  filter(vict_shot=="YES" & last12flag=="yes") %>%
  group_by(district) %>%
  summarise(last12mos=n())
# Marry into a single df to be used later; 2023 column here will need to changed at the turn of the year
lapd_shootings <- left_join(lapd_shootings_annual %>% select(-'2023'), lapd_shootings_last12,by="district")
rm(lapd_shootings_annual,lapd_shootings_last12)

# PROCESS COMBINED FULL CRIME FILE
# Categorize all other individual crime records
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

# ROLL UP INTO A DF WITH ANNUAL TOTALS BY DISTRICT, CATEGORY AND YEAR
# Eliminating Other/Part 2 crimes
# First calculate for just the last12, using the flag we added earlier
lapd_last12 <- lapd_annual %>%
  filter(category != "Other or Part 2" & last12flag=="yes") %>%
  group_by(district, category) %>%
  summarise(last12mos=n())
# Then calculate and pivot by year for all incidents/records
lapd_annual <- lapd_annual %>%
  filter(category != "Other or Part 2") %>%
  group_by(agency,district,category,year) %>%
  summarise(count=n()) %>%
  pivot_wider(names_from=year, values_from=count)
# Marry into a single df to be used later; 2023 column here will need to changed at the turn of the year
lapd_annual <- left_join(lapd_annual %>% select(-'2023'), lapd_last12,by=c("district","category"))
rm(lapd_last12)

# Create main crime file bringing shootings in as a separate eighth category
# Appending the shootings file we made earlier
# Changing the name here to lapd_crime primary df
lapd_crime <- rbind(lapd_annual,lapd_shootings)
# Add place & county cols that match the new region wide police map file
lapd_crime$place <- paste(lapd_crime$agency,lapd_crime$district)
lapd_crime$county <- "Los Angeles County"


# Roll up citywide data for tracker pages and datawrapper charts
# Will need to be manually updating at year end to add 2023
citywide_crime <- lapd_crime %>%
  group_by(category) %>%
  summarise(`2010`=sum(`2010`,na.rm=TRUE),
            `2011`=sum(`2011`,na.rm=TRUE),
            `2012`=sum(`2012`,na.rm=TRUE),
            `2013`=sum(`2013`,na.rm=TRUE),
            `2014`=sum(`2014`,na.rm=TRUE),
            `2015`=sum(`2015`,na.rm=TRUE),
            `2016`=sum(`2016`,na.rm=TRUE),
            `2017`=sum(`2017`,na.rm=TRUE),
            `2018`=sum(`2018`,na.rm=TRUE),
            `2019`=sum(`2019`,na.rm=TRUE),
            `2020`=sum(`2020`,na.rm=TRUE),
            `2021`=sum(`2021`,na.rm=TRUE),
            `2022`=sum(`2022`,na.rm=TRUE),
            last12mos=sum(last12mos))


# Load LAPD police districts file
lapd_districts <- readRDS("scripts/rds/lapd_districts.rds")
# Then merge the crime file with the districts file
lapd_crime <- left_join(lapd_districts %>% select(3,6,7),lapd_crime,by="district")

# add 4-year totals and annualized averages
lapd_crime$total_prior4years <- lapd_crime$`2019`+
  lapd_crime$`2020`+
  lapd_crime$`2021`+
  lapd_crime$`2022`
lapd_crime$avg_prior4years <- round(((lapd_crime$total_prior4years)/4),1)
# now add the increases or change percentages
lapd_crime$inc_19to22 <- round(lapd_crime$`2022`/lapd_crime$`2019`*100-100,1)
lapd_crime$inc_10to22 <- round(lapd_crime$`2022`/lapd_crime$`2010`*100-100,1)
lapd_crime$inc_19tolast12 <- round(lapd_crime$last12mos/lapd_crime$`2019`*100-100,1)
lapd_crime$inc_22tolast12 <- round(lapd_crime$last12mos/lapd_crime$`2022`*100-100,1)
lapd_crime$inc_prior4yearavgtolast12 <- round((lapd_crime$last12mos/lapd_crime$avg_prior4years)*100-100,0)
# add crime rates for each year
lapd_crime$rate19 <- round((lapd_crime$`2019`/lapd_crime$population)*100000,1)
lapd_crime$rate20 <- round((lapd_crime$`2020`/lapd_crime$population)*100000,1)
lapd_crime$rate21 <- round((lapd_crime$`2021`/lapd_crime$population)*100000,1)
lapd_crime$rate22 <- round((lapd_crime$`2022`/lapd_crime$population)*100000,1)
lapd_crime$rate_last12 <- round((lapd_crime$last12mos/lapd_crime$population)*100000,1)
lapd_crime$rate_prior4years <- 
  round((lapd_crime$avg_prior4years/lapd_crime$population)*100000,1)
# Now reduce the precinct down to just the columns we likely need for the tracker pages
# lapd_crime <- lapd_crime %>% select(1,4,5,6,26:28,36:40,44:55,29,42)
# for map/table making purposes, changing Inf and NaN in calc fields to NA
lapd_crime <- lapd_crime %>%
  mutate_if(is.numeric, ~ifelse(. == Inf, NA, .))
lapd_crime <- lapd_crime %>%
  mutate_if(is.numeric, ~ifelse(. == "NaN", NA, .))

# store a full version of latest lapd_crime as rds
saveRDS(lapd_crime,"scripts/rds/lapd_crime.rds")
saveRDS(lapd_crime,"data/output/lapd_crime.rds")

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

# PROCESS CITYWIDE TOTALS AND CHANGES
# Set variable of LA city population
la_population <- 3849297
# Add 4-year totals and annualized averages
citywide_crime$total_prior4years <- citywide_crime$`2019`+
  citywide_crime$`2020`+
  citywide_crime$`2021`+
  citywide_crime$`2022`
citywide_crime$avg_prior4years <- round((citywide_crime$total_prior4years/4),1)
# Add change percentages
citywide_crime$inc_19to21 <- round(citywide_crime$`2022`/citywide_crime$`2019`*100-100,1)
citywide_crime$inc_10to21 <- round(citywide_crime$`2022`/citywide_crime$`2010`*100-100,1)
citywide_crime$inc_19tolast12 <- round(citywide_crime$last12mos/citywide_crime$`2019`*100-100,1)
citywide_crime$inc_22tolast12 <- round(citywide_crime$last12mos/citywide_crime$`2022`*100-100,1)
citywide_crime$inc_prior4yearavgtolast12 <- round((citywide_crime$last12mos/citywide_crime$avg_prior4years)*100-100,0)
# add crime rates for each year
citywide_crime$rate19 <- round((citywide_crime$`2019`/la_population)*100000,1)
citywide_crime$rate20 <- round((citywide_crime$`2020`/la_population)*100000,1)
citywide_crime$rate21 <- round((citywide_crime$`2021`/la_population)*100000,1)
citywide_crime$rate22 <- round((citywide_crime$`2022`/la_population)*100000,1)
citywide_crime$rate_last12 <- round((citywide_crime$last12mos/la_population)*100000,1)
citywide_crime$rate_prior4years <- 
  round((citywide_crime$avg_prior4years/la_population)*100000,1)

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
citywide_crime %>% select(1:15) %>% write_csv("data/output/yearly/citywide_yearly.csv")
citywide_crime %>% select(1:15) %>% filter(category=="Homicide") %>% write_csv("data/output/yearly/murders_city.csv")
citywide_crime %>% select(1:15) %>% filter(category=="Sexual Assault") %>%  write_csv("data/output/yearly/sexassaults_city.csv")
citywide_crime %>% select(1:15) %>% filter(category=="Vehicle Theft") %>%  write_csv("data/output/yearly/autothefts_city.csv")
citywide_crime %>% select(1:15) %>% filter(category=="Larceny") %>%  write_csv("data/output/yearly/thefts_city.csv")
citywide_crime %>% select(1:15) %>% filter(category=="Burglary") %>%  write_csv("data/output/yearly/burglaries_city.csv")
citywide_crime %>% select(1:15) %>% filter(category=="Robbery") %>%  write_csv("data/output/yearly/robberies_city.csv")
citywide_crime %>% select(1:15) %>% filter(category=="Aggravated Assault") %>%  write_csv("data/output/yearly/assaults_city.csv")
citywide_crime %>% select(1:15) %>% filter(category=="Shootings") %>%  write_csv("data/output/yearly/shootings_city.csv")



