library(rmarkdown)

# Code to build each of the trackers
# Includes loading pre-processed and stored dfs
# Grouped by each page to allow for individual or mass processing 

# MURDERS
# Load RDS
murders_district <- readRDS("scripts/rds/murders_district.rds")
murders_city <- readRDS("scripts/rds/murders_city.rds")
lapd_asofdate <- readRDS("scripts/rds/lapd_asofdate.rds")
# Render page
rmarkdown::render('scripts/Los_Angeles_Safety_Tracker.Rmd', 
                  output_dir = "docs",
                  output_file = 'Los_Angeles_Safety_Tracker.html')

# BURGLARIES
# Load RDS
burglaries_district <- readRDS("scripts/rds/burglaries_district.rds")
burglaries_city <- readRDS("scripts/rds/burglaries_city.rds")
lapd_asofdate <- readRDS("scripts/rds/lapd_asofdate.rds")
# Render page
rmarkdown::render('scripts/Los_Angeles_Safety_Tracker_Burglaries.Rmd', 
                  output_dir = "docs",
                  output_file = "Los_Angeles_Safety_Tracker_Burglaries.html")

# THEFTS
# Load RDS
thefts_district <- readRDS("scripts/rds/thefts_district.rds")
thefts_city <- readRDS("scripts/rds/thefts_city.rds")
lapd_asofdate <- readRDS("scripts/rds/lapd_asofdate.rds")
# Render page
rmarkdown::render('scripts/Los_Angeles_Safety_Tracker_Thefts.Rmd', 
                  output_dir = "docs",
                  output_file = 'Los_Angeles_Safety_Tracker_Thefts.html')

# AUTO THEFTS
# Load RDS
autothefts_district <- readRDS("scripts/rds/autothefts_district.rds")
autothefts_city <- readRDS("scripts/rds/autothefts_city.rds")
lapd_asofdate <- readRDS("scripts/rds/lapd_asofdate.rds")
# Render page
rmarkdown::render('scripts/Los_Angeles_Safety_Tracker_VehicleThefts.Rmd', 
                  output_dir = "docs",
                  output_file = 'Los_Angeles_Safety_Tracker_VehicleThefts.html')

# ROBBERIES
# Load RDS
robberies_district <- readRDS("scripts/rds/robberies_district.rds")
robberies_city <- readRDS("scripts/rds/robberies_city.rds")
lapd_asofdate <- readRDS("scripts/rds/lapd_asofdate.rds")
# Render page
rmarkdown::render('scripts/Los_Angeles_Safety_Tracker_Robberies.Rmd', 
                  output_dir = "docs",
                  output_file = 'Los_Angeles_Safety_Tracker_Robberies.html')

# ASSAULTS
# Load RDS
assaults_district <- readRDS("scripts/rds/assaults_district.rds")
assaults_city <- readRDS("scripts/rds/assaults_city.rds")
lapd_asofdate <- readRDS("scripts/rds/lapd_asofdate.rds")
# Render page
rmarkdown::render('scripts/Los_Angeles_Safety_Tracker_Assaults.Rmd', 
                  output_dir = "docs",
                  output_file = 'Los_Angeles_Safety_Tracker_Assaults.html')

# SEXUAL ASSAULTS
# Load RDS
sexassaults_district <- readRDS("scripts/rds/sexassaults_district.rds")
sexassaults_city <- readRDS("scripts/rds/sexassaults_city.rds")
lapd_asofdate <- readRDS("scripts/rds/lapd_asofdate.rds")
# Render page
rmarkdown::render('scripts/Los_Angeles_Safety_Tracker_SexualAssaults.Rmd', 
                  output_dir = "docs",
                  output_file = 'Los_Angeles_Safety_Tracker_SexualAssaults.html')


