## ==============================
## 07_combined_scripts.R
## R-frica Predicts – Full Pipeline Runner
##
## - 01_africa_matches_EDA.R:
##     Load and explore original African nations results,
##     filter to AFCON 2025 teams, save african_afcon_matches.csv
##
## - 02_scraping_2000_2025.R:
##     Scrape additional matches (2000–2025) for AFCON nations
##     from national-football-teams.com and save afcon_country_matches_2000-2025.csv
##
## - 03_feature_engineering.R:
##     Join match data with FIFA rankings, engineer model features,
##     and save model_data.rds
##
## - 04_modeling.R:
##     Train multinomial and random forest models on model_data.rds,
##     evaluate them, and save rf_model.rds / multinom_model.rds
##
## - 05_simulation.R:
##     Load rf_model.rds + rankings, simulate AFCON 2025 tournament once,
##     and save the simulation result as afcon_2025_sim_seed123.rds
##
## - 06_plotting_tournament_table.R:
##     Load the saved simulation result and create a combined
##     bracket + group table plot, saving it as afcon_2025_prediction.png
##
## Run this file to execute the entire pipeline end-to-end.
## ==============================

# Make sure the working directory is the project root
# (so that "scripts/" and "data/" paths are correct).
# You can set this manually in RStudio or via setwd() if needed.

# Run the pipeline below to retrieve prediction outcome
source("scripts/01_africa_matches_EDA.R")

# Data is already in the data/ folder. Scraping takes up to half an hour
# But feel free to test it out!
# source("scripts/02_scraping_2000_2025.R")

source("scripts/03_feature_engineering.R")
source("scripts/04_modeling.R")
source("scripts/05_simulation.R")
source("scripts/06_plotting_tournament_table.R")

