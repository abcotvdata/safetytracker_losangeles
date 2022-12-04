library(tidyverse)

# options(timeout=300)
# Source page for LA Sheriff Data: https://lasd.org/transparency/part1and2crimedata/#part1

# Run one time on local machine to create rds archives
# LASheriff Annuals
# download.file("http://shq.lasdnews.net/CrimeStats/CAASS/2021-PART_I_AND_II_CRIMES.csv","data/source/annual/lasd_2021.csv")
# download.file("http://shq.lasdnews.net/CrimeStats/CAASS/2020-PART_I_AND_II_CRIMES.csv","data/source/annual/lasd_2020.csv")
# download.file("http://shq.lasdnews.net/CrimeStats/CAASS/2019-PART_I_AND_II_CRIMES.csv","data/source/annual/lasd_2019.csv")

lasd_2019 <- read_csv("data/source/annual/lasd_2019.csv") %>% janitor::clean_names() %>% select(2,4:7,11,12,15:19) 
lasd_2020 <- read_csv("data/source/annual/lasd_2020.csv") %>% janitor::clean_names() %>% select(2,4:7,11,12,15:19) 
lasd_2021 <- read_csv("data/source/annual/lasd_2021.csv") %>% janitor::clean_names() %>% select(2,4:7,11,12,15:19) 

lasd_past <- rbind(lasd_2019,lasd_2020,lasd_2021)
rm(lasd_2019,lasd_2020,lasd_2021)

saveRDS(lasd_past,"scripts/rds/lasd_past.rds")


