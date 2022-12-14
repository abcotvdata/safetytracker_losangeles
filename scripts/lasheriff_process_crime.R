library(tidyverse)
library(readxl)

# Load manual file built from scrape of LASD annual reports
# Source for most is LASD website; some years scraped from Internet Archive
lasheriff_annual <- read_excel("data/source/annual/lasd_bystation_2010to2021.xlsx")
# Add year to date from PDF scrape of latest mo nthly file
# lasheriff_ytd <- read_excel("data/source/recent/lasd_bystation_ytd.xlsx")

# Add agency column
lasheriff_annual$agency <- "LASD"

# Create a separate file for each crime to map to statewide file and tables
lasheriff_murder <- lasheriff_annual %>% filter(category=="Criminal Homicide")
lasheriff_sexassault <- lasheriff_annual %>% filter(category=="Sexual Assault")
lasheriff_assault <- lasheriff_annual %>% filter(category=="Aggravated Assault")
lasheriff_robbery <- lasheriff_annual %>% filter(category=="Robbery")
lasheriff_burglary <- lasheriff_annual %>% filter(category=="Burglary")
lasheriff_theft <- lasheriff_annual %>% filter(category=="Larceny Theft")
lasheriff_autotheft <- lasheriff_annual %>% filter(category=="Vehicle Theft")





saveRDS(lasheriff_crime,"scripts/rds/lasheriff_crime.rds")


