library(tidyverse)
library(RJSONIO)
library(leaflet)
library(leaflet.providers)
library(rgdal)

download.file("https://data.lacity.org/api/views/2nrs-mtv8/rows.csv?accessType=DOWNLOAD","incidents.csv")

# download.file("http://shq.lasdnews.net/CrimeStats/CAASS/2020-PART_I_AND_II_CRIMES.csv","20_lasd_incidents.csv")
# download.file("http://shq.lasdnews.net/CrimeStats/CAASS/2019-PART_I_AND_II_CRIMES.csv","19_lasd_incidents.csv")
# download.file("http://shq.lasdnews.net/CrimeStats/CAASS/2018-PART_I_AND_II_CRIMES.csv","18_lasd_incidents.csv")
# download.file("http://shq.lasdnews.net/CrimeStats/CAASS/2017-PART_I_AND_II_CRIMES.csv","17_lasd_incidents.csv")
# download.file("http://shq.lasdnews.net/CrimeStats/CAASS/2016-PART_I_AND_II_CRIMES.csv","16_lasd_incidents.csv")
download.file("http://shq.lasdnews.net/CrimeStats/CAASS/PART_I_AND_II_CRIMES-YTD.csv","21ytd_lasd_incidents.csv")
download.file("http://shq.lasdnews.net/CrimeStats/CAASS/PART_I_AND_II_CRIMES.csv","21daily_lasd_incidents.csv")

X21daily_lasd_incidents <- read_csv("21daily_lasd_incidents.csv",
                                    col_types = cols(LURN_SAK = col_character(),
                                                     INCIDENT_DATE = col_character(),
                                                     INCIDENT_REPORTED_DATE = col_date(format = "%m/%d/%Y"),
                                                     ZIP = col_character(), PART_CATEGORY = col_character()))


X21ytd_lasd_incidents <- read_csv("21ytd_lasd_incidents.csv",
                                    col_types = cols(LURN_SAK = col_character(),
                                                     INCIDENT_DATE = col_character(),
                                                     INCIDENT_REPORTED_DATE = col_date(format = "%m/%d/%Y"),
                                                     ZIP = col_character(), PART_CATEGORY = col_character()))

lasd_incidents21 <- rbind(X21ytd_lasd_incidents,X21daily_lasd_incidents) %>% janitor::clean_names()
rm(X21ytd_lasd_incidents,X21daily_lasd_incidents)


X20_lasd_incidents <- read_csv("20_lasd_incidents.csv",
                                  col_types = cols(LURN_SAK = col_character(),
                                                   INCIDENT_DATE = col_character(),
                                                   INCIDENT_REPORTED_DATE = col_date(format = "%m/%d/%Y"),
                                                   ZIP = col_character(), PART_CATEGORY = col_character()))
X19_lasd_incidents <- read_csv("19_lasd_incidents.csv",
                               col_types = cols(LURN_SAK = col_character(),
                                                INCIDENT_DATE = col_character(),
                                                INCIDENT_REPORTED_DATE = col_date(format = "%m/%d/%Y"),
                                                ZIP = col_character(), PART_CATEGORY = col_character()))
X18_lasd_incidents <- read_csv("18_lasd_incidents.csv",
                               col_types = cols(LURN_SAK = col_character(),
                                                INCIDENT_DATE = col_character(),
                                                INCIDENT_REPORTED_DATE = col_date(format = "%m/%d/%Y"),
                                                ZIP = col_character(), PART_CATEGORY = col_character()))
X17_lasd_incidents <- read_csv("17_lasd_incidents.csv",
                               col_types = cols(LURN_SAK = col_character(),
                                                INCIDENT_DATE = col_character(),
                                                INCIDENT_REPORTED_DATE = col_date(format = "%m/%d/%Y"),
                                                ZIP = col_character(), PART_CATEGORY = col_character()))
X16_lasd_incidents <- read_csv("16_lasd_incidents.csv",
                               col_types = cols(LURN_SAK = col_character(),
                                                INCIDENT_DATE = col_character(),
                                                INCIDENT_REPORTED_DATE = col_date(format = "%m/%d/%Y"),
                                                ZIP = col_character(), PART_CATEGORY = col_character()))

lasd_incidents <- rbind(X20_lasd_incidents,X19_lasd_incidents,X18_lasd_incidents,X17_lasd_incidents,X16_lasd_incidents) %>% janitor::clean_names()
rm(X20_lasd_incidents,X19_lasd_incidents,X18_lasd_incidents,X17_lasd_incidents,X16_lasd_incidents)

lasd_incidents <- rbind(lasd_incidents,lasd_incidents21) %>% janitor::clean_names()
rm(lasd_incidents21)

# ANALYZE COUNTY

lasd_incidents$date <- substr(lasd_incidents$incident_date,1,10)
lasd_incidents$year <- substr(lasd_incidents$incident_date,7,10)
# remove incidents reported later years
lasd_incidents <- lasd_incidents %>% filter(str_detect(year,"2021|2020|2019|2018|2017|2016"))

sheriff_by_area <- lasd_incidents %>% 
  group_by(unit_name,year) %>%
  summarise(count=n())
sheriff_by_area <- pivot_wider(sheriff_by_area,names_from=year,values_from = "count")
sheriff_by_area$perday2016 <- sheriff_by_area$'2016'/365
sheriff_by_area$perday2017 <- sheriff_by_area$'2017'/365
sheriff_by_area$perday2018 <- sheriff_by_area$'2018'/365
sheriff_by_area$perday2019 <- sheriff_by_area$'2019'/365
sheriff_by_area$perday2020 <- sheriff_by_area$'2020'/365
sheriff_by_area$perday2021 <- sheriff_by_area$'2021'/340
sheriff_by_area <- transform(sheriff_by_area, precovid_avg = rowMeans(sheriff_by_area[,8:11], na.rm = TRUE))
sheriff_by_area$updown <- sheriff_by_area$perday2021/sheriff_by_area$perday2020
sheriff_by_area$vs4yrprepandemicavg <- sheriff_by_area$perday2021/sheriff_by_area$precovid_avg

sheriff_homicide_by_area <- lasd_incidents %>% 
  filter(category=="CRIMINAL HOMICIDE") %>%
  group_by(unit_name,year) %>%
  summarise(count=n())
sheriff_homicide_by_area <- pivot_wider(sheriff_homicide_by_area,names_from=year,values_from = "count")
sheriff_homicide_by_area$perday2016 <- sheriff_homicide_by_area$'2016'/365
sheriff_homicide_by_area$perday2017 <- sheriff_homicide_by_area$'2017'/365
sheriff_homicide_by_area$perday2018 <- sheriff_homicide_by_area$'2018'/365
sheriff_homicide_by_area$perday2019 <- sheriff_homicide_by_area$'2019'/365
sheriff_homicide_by_area$perday2020 <- sheriff_homicide_by_area$'2020'/365
sheriff_homicide_by_area$perday2021 <- sheriff_homicide_by_area$'2021'/340
sheriff_homicide_by_area$updown <- sheriff_homicide_by_area$perday2021/sheriff_homicide_by_area$perday2020
sheriff_homicide_by_area$fiveyearupdown <- sheriff_homicide_by_area$perday2021/sheriff_homicide_by_area$perday2016

sheriff_by_type <- lasd_incidents %>% 
  group_by(stat_desc,year) %>%
  summarise(count=n())
sheriff_by_type <- pivot_wider(sheriff_by_type,names_from=year,values_from = "count")
sheriff_by_type$perday2016 <- sheriff_by_type$'2016'/365
sheriff_by_type$perday2017 <- sheriff_by_type$'2017'/365
sheriff_by_type$perday2018 <- sheriff_by_type$'2018'/365
sheriff_by_type$perday2019 <- sheriff_by_type$'2019'/365
sheriff_by_type$perday2020 <- sheriff_by_type$'2020'/365
sheriff_by_type$perday2021 <- sheriff_by_type$'2021'/340
sheriff_by_type <- transform(sheriff_by_type, precovid_avg = rowMeans(sheriff_by_type[,8:11], na.rm = TRUE))
sheriff_by_type$updown <- sheriff_by_type$perday2021/sheriff_by_type$perday2020
sheriff_by_type$vs4yrprepandemicavg <- sheriff_by_type$perday2021/sheriff_by_type$precovid_avg

sheriff_by_category <- lasd_incidents %>% 
  group_by(category,year) %>%
  summarise(count=n())
sheriff_by_category <- pivot_wider(sheriff_by_category,names_from=year,values_from = "count")
sheriff_by_category$perday2016 <- sheriff_by_category$'2016'/365
sheriff_by_category$perday2017 <- sheriff_by_category$'2017'/365
sheriff_by_category$perday2018 <- sheriff_by_category$'2018'/365
sheriff_by_category$perday2019 <- sheriff_by_category$'2019'/365
sheriff_by_category$perday2020 <- sheriff_by_category$'2020'/365
sheriff_by_category$perday2021 <- sheriff_by_category$'2021'/340
sheriff_by_category <- transform(sheriff_by_category, precovid_avg = rowMeans(sheriff_by_category[,8:11], na.rm = TRUE))
sheriff_by_category$updown <- sheriff_by_category$perday2021/sheriff_by_category$perday2020
sheriff_by_category$vs4yrprepandemicavg <- sheriff_by_category$perday2021/sheriff_by_category$precovid_avg

sheriff_by_category_andarea <- lasd_incidents %>% 
  group_by(category,unit_name,year) %>%
  summarise(count=n())
sheriff_by_category_andarea <- pivot_wider(sheriff_by_category_andarea,names_from=year,values_from = "count")
sheriff_by_category_andarea$perday2016 <- sheriff_by_category_andarea$'2016'/365
sheriff_by_category_andarea$perday2017 <- sheriff_by_category_andarea$'2017'/365
sheriff_by_category_andarea$perday2018 <- sheriff_by_category_andarea$'2018'/365
sheriff_by_category_andarea$perday2019 <- sheriff_by_category_andarea$'2019'/365
sheriff_by_category_andarea$perday2020 <- sheriff_by_category_andarea$'2020'/365
sheriff_by_category_andarea$perday2021 <- sheriff_by_category_andarea$'2021'/340
sheriff_by_category_andarea <- transform(sheriff_by_category_andarea, precovid_avg = rowMeans(sheriff_by_category_andarea[,9:12], na.rm = TRUE))
sheriff_by_category_andarea$updown <- sheriff_by_category_andarea$perday2021/sheriff_by_category_andarea$perday2020
sheriff_by_category_andarea$vs4yrprepandemicavg <- sheriff_by_category_andarea$perday2021/sheriff_by_category_andarea$precovid_avg



# START LAPD

la_incidents <- read_csv("incidents.csv", 
                         col_types = cols(`Date Rptd` = col_character(), 
                                                        `Part 1-2` = col_character(), `Crm Cd` = col_character(), 
                                                        `Premis Cd` = col_character(), `Weapon Used Cd` = col_character(), 
                                                        `Crm Cd 1` = col_character(), `Crm Cd 2` = col_character(), 
                                                        `Crm Cd 3` = col_character(), `Crm Cd 4` = col_character())) %>%
  janitor::clean_names()


la_incidents$year <- substr(la_incidents$date_occ,7,10)

by_area <- la_incidents %>% 
  group_by(area_name,year) %>%
  summarise(count=n())
by_area <- pivot_wider(by_area,names_from=year,values_from = "count")
by_area$perday2020 <- by_area$'2020'/365
by_area$perday2021 <- by_area$'2021'/330
by_area$updown <- by_area$perday2021/by_area$perday2020

homicide_by_area <- la_incidents %>% 
  filter(crm_cd_desc=="CRIMINAL HOMICIDE") %>%
  group_by(area_name,rpt_dist_no,year) %>%
  summarise(count=n())
homicide_by_area <- pivot_wider(homicide_by_area,names_from=year,values_from = "count")
homicide_by_area$perday2020 <- homicide_by_area$'2020'/365
homicide_by_area$perday2021 <- homicide_by_area$'2021'/330
homicide_by_area$updown <- homicide_by_area$perday2021/homicide_by_area$perday2020
homicide_by_area$UPPERNAME <- toupper(homicide_by_area$area_name)

type_year <- la_incidents %>% 
  group_by(crm_cd_desc,year) %>%
  summarise(count=n())
type_year <- pivot_wider(type_year,names_from=year,values_from = "count")
type_year$perday2020 <- type_year$'2020'/365
type_year$perday2021 <- type_year$'2021'/330
type_year$updown <- type_year$perday2021/type_year$perday2020

area_bytype_year <- la_incidents %>% 
  group_by(crm_cd_desc,area_name,year) %>%
  summarise(count=n())
area_bytype_year <- pivot_wider(area_bytype_year,names_from=year,values_from = "count")
area_bytype_year$perday2020 <- area_bytype_year$'2020'/365
area_bytype_year$perday2021 <- area_bytype_year$'2021'/330
area_bytype_year$updown <- area_bytype_year$perday2021/area_bytype_year$perday2020





# ignore the rest for now



districts <- rgdal::readOGR("Law_Enforcement_Reporting_Districts.geojson")


factpal <-   colorFactor(
  palette = c(
    "#b1dbad",
    "#a18f7f",
    "#ffcc80"),
  districts$S_TYPE,
  na.color = "#ff8280"
)

# colorFactor("blues", districts$S_TYPE)

label <- paste(sep = "<br>", districts$LABEL, districts$RD)

la_police_map <- leaflet(districts) %>%
  setView(-118.161229, 33.957379, zoom = 10) %>% 
  addProviderTiles(provider = "CartoDB.Positron") %>%
  addPolygons(color = "#444444", popup = label, weight = 0.4, smoothFactor = 0.5,
              opacity = 0.5, fillOpacity = 0.4,
              fillColor = ~factpal(S_TYPE))
#  addMarkers(data = lasd21, 
#             lat = ~latitude,
#             lng = ~longitude) %>%

la_police_map

# set up to add markers
 #lasd21 <- lasd_incidents %>% 
 # filter(year=="2021") %>%
#  select(1,2,3,6,17,18)
#lapd21 <- la_incidents %>% 
#  filter(year=="2021") %>%
#  select(1,3,6,10,27,28)






  
  