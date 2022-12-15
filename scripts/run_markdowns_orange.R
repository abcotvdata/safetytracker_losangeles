library(rmarkdown)

# Code to build each of the trackers
# Includes loading pre-processed and stored dfs
# Grouped by each page to allow for individual or mass processing

# MURDERS
# Load RDS
# socal_murder <- readRDS("scripts/rds/socal_murder.rds")
# Render page
rmarkdown::render('scripts/Orange_County_Safety_Tracker.Rmd', 
                  output_dir = "docs",
                  output_file = 'Orange_County_Safety_Tracker.html')

# BURGLARIES
# Load RDS
# socal_burglary <- readRDS("scripts/rds/socal_burglary.rds")
# Render page
rmarkdown::render('scripts/Orange_County_Safety_Tracker_Burglaries.Rmd', 
                  output_dir = "docs",
                  output_file = "Orange_County_Safety_Tracker_Burglaries.html")

# THEFTS
# Load RDS
# socal_theft <- readRDS("scripts/rds/socal_theft.rds")
# Render page
rmarkdown::render('scripts/Orange_County_Safety_Tracker_Thefts.Rmd', 
                  output_dir = "docs",
                  output_file = 'Orange_County_Safety_Tracker_Thefts.html')

# AUTO THEFTS
# Load RDS
# socal_autotheft <- readRDS("scripts/rds/socal_autotheft.rds")
# Render page
rmarkdown::render('scripts/Orange_County_Safety_Tracker_VehicleThefts.Rmd', 
                  output_dir = "docs",
                  output_file = 'Orange_County_Safety_Tracker_VehicleThefts.html')

# ROBBERIES
# Load RDS
# socal_robbery <- readRDS("scripts/rds/socal_robbery.rds")
# Render page
rmarkdown::render('scripts/Orange_County_Safety_Tracker_Robberies.Rmd', 
                  output_dir = "docs",
                  output_file = 'Orange_County_Safety_Tracker_Robberies.html')

# ASSAULTS
# Load RDS
# socal_assault <- readRDS("scripts/rds/socal_assault.rds")
# Render page
rmarkdown::render('scripts/Orange_County_Safety_Tracker_Assaults.Rmd', 
                  output_dir = "docs",
                  output_file = 'Orange_County_Safety_Tracker_Assaults.html')

# SEXUAL ASSAULTS
# Load RDS
# socal_sexassault <- readRDS("scripts/rds/socal_sexassault.rds")
# Render page
rmarkdown::render('scripts/Orange_County_Safety_Tracker_SexualAssaults.Rmd', 
                  output_dir = "docs",
                  output_file = 'Orange_County_Safety_Tracker_SexualAssaults.html')


