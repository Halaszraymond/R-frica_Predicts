source("01_africa_matches_EDA.R")
source("02_scraping_2024_2025.R")
source("03_feature_engineer.R")          # creates ranks + model_data.rds
source("04_modeling.R")                  # creates rf_fit
source("05_simulation.R")                # uses rf_fit + ranks -> creates res
source("06_plotting_tournament_table.R") # uses res -> final_plot