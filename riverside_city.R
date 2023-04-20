library(tidyverse)
library(lubridate)

riverside_city <- read_csv("https://www.riversideca.gov/transparency/data/dataset/csv/27/Crime_Reports")

riverside_city$offenseDate <- ymd(riverside_city$offenseDate)
riverside_city$year <- year(riverside_city$offenseDate)

riverside_burglaries <- riverside_city %>%
  filter(grepl("BURGLARY", crimeType, ignore.case = TRUE))

riverside_COMM_burglaries_21YTD <- riverside_burglaries %>%
  filter(year=="2021")  %>%
  filter(grepl("COMM", premise, ignore.case = TRUE))

riverside_COMM_burglaries_22YTD <- riverside_burglaries %>%
  filter(year=="2022")  %>%
  filter(grepl("COMM", premise, ignore.case = TRUE))

riverside_robberies <- riverside_city %>%
  filter(grepl("ROBBERY", crimeType, ignore.case = TRUE))

riverside_robberies_21YTD <- riverside_robberies %>%
  filter(year=="2021")
riverside_robberies_22YTD <- riverside_robberies %>%
  filter(year=="2022")
