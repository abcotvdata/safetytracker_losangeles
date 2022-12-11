library(pdftools)
library(tidyverse)
library(tidyr)
library(lubridate)
library(zoo)

# extract text from latest file
pdftext <- pdf_text("data/source/lasd_bystation_2020to2021.pdf") %>% strsplit(split = "\n")

# THIS IS KEY TO POSITIONING FIELDS TO GRAB DATA FROM TABLE
df3 <- pdf_data("data/source/lasd_bystation_2020to2021.pdf")[[3]] %>% arrange(y,x)
df37 <- pdf_data("data/source/lasd_bystation_2020to2021.pdf")[[37]] %>% arrange(y,x)



### PAGE BREAK ###

# Grab individual text values for Page 1
rawtext1 <- pdftext[[1]][1] %>% trimws()
rawtext2 <- pdftext[[1]][2] %>% trimws()
rawtext3 <- pdftext[[1]][3] %>% trimws()
rawtext4 <- pdftext[[1]][4] %>% trimws()
rawtext5 <- pdftext[[1]][5] %>% trimws()
rawtext6 <- pdftext[[1]][6] %>% trimws()
rawtext7 <- pdftext[[1]][7] %>% trimws()
rawtext8 <- pdftext[[1]][8] %>% trimws()
rawtext9 <- pdftext[[1]][9] %>% trimws()
rawtext10 <- pdftext[[1]][10] %>% trimws()
rawtext11 <- pdftext[[1]][11] %>% trimws()
rawtext12 <- pdftext[[1]][12] %>% trimws()
rawtext13 <- pdftext[[1]][13] %>% trimws()
rawtext14 <- pdftext[[1]][14] %>% trimws()
rawtext15 <- pdftext[[1]][15] %>% trimws()
rawtext16 <- pdftext[[1]][16] %>% trimws()
rawtext17 <- pdftext[[1]][17] %>% trimws()
rawtext18 <- pdftext[[1]][18] %>% trimws()
rawtext19 <- pdftext[[1]][19] %>% trimws()
rawtext20 <- pdftext[[1]][20] %>% trimws()
rawtext21 <- pdftext[[1]][21] %>% trimws()
rawtext22 <- pdftext[[1]][22] %>% trimws()
rawtext23 <- pdftext[[1]][23] %>% trimws()
rawtext24 <- pdftext[[1]][24] %>% trimws()
rawtext25 <- pdftext[[1]][25] %>% trimws()
rawtext26 <- pdftext[[1]][26] %>% trimws()
rawtext27 <- pdftext[[1]][27] %>% trimws()
rawtext28 <- pdftext[[1]][28] %>% trimws()
rawtext29 <- pdftext[[1]][29] %>% trimws()
rawtext30 <- pdftext[[1]][30] %>% trimws()
rawtext31 <- pdftext[[1]][31] %>% trimws()
rawtext32 <- pdftext[[1]][32] %>% trimws()
rawtext33 <- pdftext[[1]][33] %>% trimws()
rawtext34 <- pdftext[[1]][34] %>% trimws()
rawtext35 <- pdftext[[1]][35] %>% trimws()
rawtext36 <- pdftext[[1]][36] %>% trimws()
rawtext37 <- pdftext[[1]][37] %>% trimws()
rawtext38 <- pdftext[[1]][38] %>% trimws()
rawtext39 <- pdftext[[1]][39] %>% trimws()
rawtext40 <- pdftext[[1]][40] %>% trimws()
rawtext41 <- pdftext[[1]][41] %>% trimws()
rawtext42 <- pdftext[[1]][42] %>% trimws()
rawtext43 <- pdftext[[1]][43] %>% trimws()
rawtext44 <- pdftext[[1]][44] %>% trimws()
rawtext45 <- pdftext[[1]][45] %>% trimws()
rawtext46 <- pdftext[[1]][46] %>% trimws()
rawtext47 <- pdftext[[1]][47] %>% trimws()
rawtext48 <- pdftext[[1]][48] %>% trimws()
rawtext49 <- pdftext[[1]][49] %>% trimws()
rawtext50 <- pdftext[[1]][50] %>% trimws()

# Bind those into a one-column table
past_crime_lasd <- rbind(rawtext1,rawtext2,rawtext3,rawtext4,rawtext5,rawtext6,rawtext7,rawtext8,rawtext9,rawtext10,rawtext11,rawtext12,rawtext13,rawtext14,rawtext15,rawtext16,rawtext17,rawtext18,rawtext19,rawtext20,rawtext21,rawtext22,rawtext23,rawtext24,rawtext25,rawtext26,rawtext27,rawtext28,rawtext29,rawtext30,rawtext31,rawtext32,rawtext33,rawtext34,rawtext35,rawtext36,rawtext37,rawtext38,rawtext39,rawtext40,rawtext41,rawtext42,rawtext43,rawtext44,rawtext45,rawtext46,rawtext47,rawtext48,rawtext49,rawtext50)
# rm(rawtext1,rawtext2,rawtext3,rawtext4,rawtext5,rawtext6,rawtext7,rawtext8,rawtext9,rawtext10,rawtext11,rawtext12,rawtext13,rawtext14,rawtext15,rawtext16,rawtext17,rawtext18,rawtext19,rawtext20,rawtext21,rawtext22,rawtext23,rawtext24,rawtext25,rawtext26,rawtext27,rawtext28,rawtext29,rawtext30,rawtext31,rawtext32,rawtext33,rawtext34,rawtext35,rawtext36,rawtext37,rawtext38,rawtext39,rawtext40,rawtext41,rawtext42,rawtext43,rawtext44,rawtext45,rawtext46,rawtext47,rawtext48,rawtext49,rawtext50)
past_crime_lasd <- as.data.frame(past_crime_lasd)

# name the column temporarily
names(past_crime_lasd) <- c("rawtext")
# remove the long white space on each end of each line
past_crime_lasd$rawtext2 <- strsplit(past_crime_lasd$rawtext, "\\s+\\s+")
# flatten the list this creates in processed column
past_crime_lasd <- past_crime_lasd %>% unnest_wider(rawtext2)
past_crime_lasd <- past_crime_lasd %>% select(-6)
# name the columns temporarily
names(past_crime_lasd) <- c("station","category","total20","total21","change")
# add area col and realign columns
past_crime_lasd$area <- past_crime_lasd$category
past_crime_lasd <- past_crime_lasd %>% select(1,2,6,3,4,5)

# for this version grab the time to flag the "updated through" 
# date to attach to data frame at end
pdf_latest <- past_crime_lasd[3,1]
pdf_latest <- sub(".*\\s+to\\s+", "", pdf_latest)  
pdf_latest <- dmy(pdf_latest)

past_crime_lasd$category <- ifelse(past_crime_lasd$category %in% c("CRIMINAL HOMICIDE",
                         "RAPE","ROBBERY",
                         "AGGRAVATED ASSAULT",
                         "BURGLARY","LARCENY THEFT",
                         "GRAND THEFT AUTO",
                         "ARSON",
                         "PROPERTY CRIMES TOTAL",
                         "VIOLENT CRIMES TOTAL",
                         "PART 1 CRIMES TOTAL"),past_crime_lasd$category,"")

past_crime_lasd$area <- ifelse(past_crime_lasd$area %in% c("Station Total",
                                                           "Unincorporated")
                               ,past_crime_lasd$area,"")

past_crime_lasd$station <- ifelse(grepl(" STATION PART I CRIMES", past_crime_lasd$station),
                               past_crime_lasd$station,"")


past_crime_lasd$station <- gsub(" STATION PART I CRIMES","",past_crime_lasd$station)

past_crime_lasd$station <- past_crime_lasd[2,1]

past_crime_lasd$total20 <- as.numeric(past_crime_lasd$total20)
past_crime_lasd$total21 <- as.numeric(past_crime_lasd$total21)
past_crime_1 <- past_crime_lasd


### PAGE BREAK ###

# Grab individual text values for Page 1
rawtext1 <- pdftext[[2]][1] %>% trimws()
rawtext2 <- pdftext[[2]][2] %>% trimws()
rawtext3 <- pdftext[[2]][3] %>% trimws()
rawtext4 <- pdftext[[2]][4] %>% trimws()
rawtext5 <- pdftext[[2]][5] %>% trimws()
rawtext6 <- pdftext[[2]][6] %>% trimws()
rawtext7 <- pdftext[[2]][7] %>% trimws()
rawtext8 <- pdftext[[2]][8] %>% trimws()
rawtext9 <- pdftext[[2]][9] %>% trimws()
rawtext10 <- pdftext[[2]][10] %>% trimws()
rawtext11 <- pdftext[[2]][11] %>% trimws()
rawtext12 <- pdftext[[2]][12] %>% trimws()
rawtext13 <- pdftext[[2]][13] %>% trimws()
rawtext14 <- pdftext[[2]][14] %>% trimws()
rawtext15 <- pdftext[[2]][15] %>% trimws()
rawtext16 <- pdftext[[2]][16] %>% trimws()
rawtext17 <- pdftext[[2]][17] %>% trimws()
rawtext18 <- pdftext[[2]][18] %>% trimws()
rawtext19 <- pdftext[[2]][19] %>% trimws()
rawtext20 <- pdftext[[2]][20] %>% trimws()
rawtext21 <- pdftext[[2]][21] %>% trimws()
rawtext22 <- pdftext[[2]][22] %>% trimws()
rawtext23 <- pdftext[[2]][23] %>% trimws()
rawtext24 <- pdftext[[2]][24] %>% trimws()
rawtext25 <- pdftext[[2]][25] %>% trimws()
rawtext26 <- pdftext[[2]][26] %>% trimws()
rawtext27 <- pdftext[[2]][27] %>% trimws()
rawtext28 <- pdftext[[2]][28] %>% trimws()
rawtext29 <- pdftext[[2]][29] %>% trimws()
rawtext30 <- pdftext[[2]][30] %>% trimws()
rawtext31 <- pdftext[[2]][31] %>% trimws()
rawtext32 <- pdftext[[2]][32] %>% trimws()
rawtext33 <- pdftext[[2]][33] %>% trimws()
rawtext34 <- pdftext[[2]][34] %>% trimws()
rawtext35 <- pdftext[[2]][35] %>% trimws()
rawtext36 <- pdftext[[2]][36] %>% trimws()
rawtext37 <- pdftext[[2]][37] %>% trimws()
rawtext38 <- pdftext[[2]][38] %>% trimws()
rawtext39 <- pdftext[[2]][39] %>% trimws()
rawtext40 <- pdftext[[2]][40] %>% trimws()
rawtext41 <- pdftext[[2]][41] %>% trimws()
rawtext42 <- pdftext[[2]][42] %>% trimws()
rawtext43 <- pdftext[[2]][43] %>% trimws()
rawtext44 <- pdftext[[2]][44] %>% trimws()
rawtext45 <- pdftext[[2]][45] %>% trimws()
rawtext46 <- pdftext[[2]][46] %>% trimws()
rawtext47 <- pdftext[[2]][47] %>% trimws()
rawtext48 <- pdftext[[2]][48] %>% trimws()
rawtext49 <- pdftext[[2]][49] %>% trimws()
rawtext50 <- pdftext[[2]][50] %>% trimws()

# Bind those into a one-column table
past_crime_lasd <- rbind(rawtext1,rawtext2,rawtext3,rawtext4,rawtext5,rawtext6,rawtext7,rawtext8,rawtext9,rawtext10,rawtext11,rawtext12,rawtext13,rawtext14,rawtext15,rawtext16,rawtext17,rawtext18,rawtext19,rawtext20,rawtext21,rawtext22,rawtext23,rawtext24,rawtext25,rawtext26,rawtext27,rawtext28,rawtext29,rawtext30,rawtext31,rawtext32,rawtext33,rawtext34,rawtext35,rawtext36,rawtext37,rawtext38,rawtext39,rawtext40,rawtext41,rawtext42,rawtext43,rawtext44,rawtext45,rawtext46,rawtext47,rawtext48,rawtext49,rawtext50)
# rm(rawtext1,rawtext2,rawtext3,rawtext4,rawtext5,rawtext6,rawtext7,rawtext8,rawtext9,rawtext10,rawtext11,rawtext12,rawtext13,rawtext14,rawtext15,rawtext16,rawtext17,rawtext18,rawtext19,rawtext20,rawtext21,rawtext22,rawtext23,rawtext24,rawtext25,rawtext26,rawtext27,rawtext28,rawtext29,rawtext30,rawtext31,rawtext32,rawtext33,rawtext34,rawtext35,rawtext36,rawtext37,rawtext38,rawtext39,rawtext40,rawtext41,rawtext42,rawtext43,rawtext44,rawtext45,rawtext46,rawtext47,rawtext48,rawtext49,rawtext50)
past_crime_lasd <- as.data.frame(past_crime_lasd)

# name the column temporarily
names(past_crime_lasd) <- c("rawtext")
# remove the long white space on each end of each line
past_crime_lasd$rawtext2 <- strsplit(past_crime_lasd$rawtext, "\\s+\\s+")
# flatten the list this creates in processed column
past_crime_lasd <- past_crime_lasd %>% unnest_wider(rawtext2)
past_crime_lasd <- past_crime_lasd %>% select(-6)
# name the columns temporarily
names(past_crime_lasd) <- c("station","category","total20","total21","change")
# add area col and realign columns
past_crime_lasd$area <- past_crime_lasd$category
past_crime_lasd <- past_crime_lasd %>% select(1,2,6,3,4,5)

# for this version grab the time to flag the "updated through" 
# date to attach to data frame at end
pdf_latest <- past_crime_lasd[3,1]
pdf_latest <- sub(".*\\s+to\\s+", "", pdf_latest)  
pdf_latest <- dmy(pdf_latest)

past_crime_lasd$category <- ifelse(past_crime_lasd$category %in% c("CRIMINAL HOMICIDE",
                                                                   "RAPE","ROBBERY",
                                                                   "AGGRAVATED ASSAULT",
                                                                   "BURGLARY","LARCENY THEFT",
                                                                   "GRAND THEFT AUTO",
                                                                   "ARSON",
                                                                   "PROPERTY CRIMES TOTAL",
                                                                   "VIOLENT CRIMES TOTAL",
                                                                   "PART 1 CRIMES TOTAL"),past_crime_lasd$category,"")

past_crime_lasd$area <- ifelse(past_crime_lasd$area %in% c("Station Total",
                                                           "Unincorporated")
                               ,past_crime_lasd$area,"")

past_crime_lasd$station <- ifelse(grepl(" STATION PART I CRIMES", past_crime_lasd$station),
                                  past_crime_lasd$station,"")


past_crime_lasd$station <- gsub(" STATION PART I CRIMES","",past_crime_lasd$station)

past_crime_lasd$station <- past_crime_lasd[2,1]

past_crime_lasd$total20 <- as.numeric(past_crime_lasd$total20)
past_crime_lasd$total21 <- as.numeric(past_crime_lasd$total21)
past_crime_2 <- past_crime_lasd


