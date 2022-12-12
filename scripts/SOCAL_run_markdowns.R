library(rmarkdown)

# Code to build each of the trackers
# Includes loading pre-processed and stored dfs
# Grouped by each page to allow for individual or mass processing

# MURDERS
# Load RDS
murders_places <- readRDS("scripts/rds/murders_places.rds")
murders_county <- readRDS("scripts/rds/murders_county.rds")
# Render page
rmarkdown::render('scripts/Orange_County_Safety_Tracker.Rmd', 
                  output_dir = "docs",
                  output_file = 'Orange_County_Safety_Tracker.html')

# MURDERS
# Load RDS
murders_district <- readRDS("scripts/rds/murders_region.rds")
murders_city <- readRDS("scripts/rds/murders_region.rds")
# Render page
rmarkdown::render('scripts/So_Cal_Safety_Tracker.Rmd', 
                  output_dir = "docs",
                  output_file = 'Southern_California_Safety_Tracker.html')

# BURGLARIES
# Load RDS
burglaries_district <- readRDS("scripts/rds/burglaries_region.rds")
burglaries_city <- readRDS("scripts/rds/burglaries_region.rds")
# Render page
rmarkdown::render('scripts/So_Cal_Safety_Tracker_Burglaries.Rmd', 
                  output_dir = "docs",
                  output_file = "Southern_California_Safety_Tracker_Burglaries.html")

# THEFTS
# Load RDS
thefts_district <- readRDS("scripts/rds/thefts_district.rds")
thefts_city <- readRDS("scripts/rds/thefts_city.rds")
# Render page
rmarkdown::render('scripts/So_Cal_Safety_Tracker_Thefts.Rmd', 
                  output_dir = "docs",
                  output_file = 'Southern_California_Safety_Tracker_Thefts.html')

# AUTO THEFTS
# Load RDS
autothefts_district <- readRDS("scripts/rds/autothefts_region.rds")
autothefts_city <- readRDS("scripts/rds/autothefts_region.rds")
# Render page
rmarkdown::render('scripts/So_Cal_Safety_Tracker_VehicleThefts.Rmd', 
                  output_dir = "docs",
                  output_file = 'Southern_California_Safety_Tracker_VehicleThefts.html')

# ROBBERIES
# Load RDS
robberies_district <- readRDS("scripts/rds/robberies_district.rds")
robberies_city <- readRDS("scripts/rds/robberies_city.rds")
# Render page
rmarkdown::render('scripts/So_Cal_Safety_Tracker_Robberies.Rmd', 
                  output_dir = "docs",
                  output_file = 'Southern_California_Safety_Tracker_Robberies.html')

# ASSAULTS
# Load RDS
assaults_district <- readRDS("scripts/rds/assaults_district.rds")
assaults_city <- readRDS("scripts/rds/assaults_city.rds")
# Render page
rmarkdown::render('scripts/So_Cal_Safety_Tracker_Assaults.Rmd', 
                  output_dir = "docs",
                  output_file = 'Southern_California_Safety_Tracker_Assaults.html')

# SEXUAL ASSAULTS
# Load RDS
sexassaults_district <- readRDS("scripts/rds/sexassaults_district.rds")
sexassaults_city <- readRDS("scripts/rds/sexassaults_city.rds")
# Render page
rmarkdown::render('scripts/So_Cal_Safety_Tracker_SexualAssaults.Rmd', 
                  output_dir = "docs",
                  output_file = 'Southern_California_Safety_Tracker_SexualAssaults.html')


