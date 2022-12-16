library(tidyverse)
library(readxl)

# Load manual file built from scrape of LASD annual reports
# Source for most is LASD website; some years scraped from Internet Archive
lasheriff_annual <- read_excel("data/source/annual/lasd_bystation_2010to2021.xlsx")
# Add year to date from PDF scrape of latest mo nthly file
# lasheriff_ytd <- read_excel("data/source/recent/lasd_bystation_ytd.xlsx")

# Add agency column
lasheriff_annual$agency <- "LASD"
# Add place & county cols that match the new region wide police map file
lasheriff_annual$county <- "Los Angeles County"
lasheriff_annual$place <- case_when(lasheriff_annual$area=="Unincorporated" & lasheriff_annual$station!="Walnut" ~ paste(lasheriff_annual$area, lasheriff_annual$station),
                                    lasheriff_annual$area=="Unincorporated" & lasheriff_annual$station=="Walnut" ~ "Unincorporated Walnut/Diamond Bar",
                                TRUE ~ lasheriff_annual$area)

# Create a separate file for each crime to map to statewide file and tables
lasheriff_murder <- lasheriff_annual %>% filter(category=="Criminal Homicide") %>% select(17,18,4:16)
lasheriff_sexassault <- lasheriff_annual %>% filter(category=="Sexual Assault") %>% select(17,18,4:16)
lasheriff_assault <- lasheriff_annual %>% filter(category=="Aggravated Assault") %>% select(17,18,4:16)
lasheriff_robbery <- lasheriff_annual %>% filter(category=="Robbery") %>% select(17,18,4:16)
lasheriff_burglary <- lasheriff_annual %>% filter(category=="Burglary") %>% select(17,18,4:16)
lasheriff_theft <- lasheriff_annual %>% filter(category=="Larceny Theft") %>% select(17,18,4:16)
lasheriff_autotheft <- lasheriff_annual %>% filter(category=="Vehicle Theft") %>% select(17,18,4:16)

saveRDS(lasheriff_annual,"scripts/rds/lasheriff_crime.rds")