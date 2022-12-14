library(tidyverse)
library(readxl)

# Load manual file built from scrape of LASD annual reports
# Source for most is LASD website; some years scraped from Internet Archive
lasheriff_past <- read_excel("data/source/annual/lasd_bystation_2010to2021.xlsx")
# Add year to date from PDF scrape of latest monthly file
lasheriff_ytd <- read_excel("data/source/recent/lasd_bystation_ytd.xlsx")




saveRDS(lasheriff_crime,"scripts/rds/lasheriff_crime.rds")


