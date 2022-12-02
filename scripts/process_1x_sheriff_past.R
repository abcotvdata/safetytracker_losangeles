library(tidyverse)
library(lubridate)

# options(timeout=300)
# Source page for LA Sheriff Data: https://lasd.org/transparency/part1and2crimedata/#part1

#LASheriff Annuals
download.file("http://shq.lasdnews.net/CrimeStats/CAASS/2021-PART_I_AND_II_CRIMES.csv","data/source/annual/lasd_2021.csv")
download.file("http://shq.lasdnews.net/CrimeStats/CAASS/2020-PART_I_AND_II_CRIMES.csv","data/source/annual/lasd_2020.csv")
download.file("http://shq.lasdnews.net/CrimeStats/CAASS/2019-PART_I_AND_II_CRIMES.csv","data/source/annual/lasd_2019.csv")

lasd_2019 <- read_csv("data/source/annual/lasd_2019.csv") %>% janitor::clean_names() %>% select(2,4:7,11,12,14:19) 
lasd_2020 <- read_csv("data/source/annual/lasd_2020.csv") %>% janitor::clean_names() %>% select(2,4:7,11,12,14:19) 
lasd_2021 <- read_csv("data/source/annual/lasd_2021.csv") %>% janitor::clean_names() %>% select(2,4:7,11,12,14:19) 

saveRDS(lasd_2019,"scripts/rds/lasd_2019.rds")
saveRDS(lasd_2020,"scripts/rds/lasd_2020.rds")
saveRDS(lasd_2021,"scripts/rds/lasd_2021.rds")