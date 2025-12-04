## ==============================
## 00_main.R
## Master Entry Point for R-frica Predicts
##
## This script is the main entry point for running the entire
## AFCON 2025 prediction pipeline. It ensures that all required
## R packages are installed and loaded before sourcing the
## combined pipeline runner (07_combined_scripts.R).
##
## Steps executed automatically:
##   1. Check + install missing packages
##   2. Load required libraries
##   3. Execute the full pipeline:
##        - EDA
##        - Feature engineering
##        - Model training
##        - Simulation
##        - Final tournament visualization
##
## Usage (RStudio recommended):
##   source("00_main.R")
##
## Note:
##   The heavy web-scraping script is NOT executed by default.
##   The project uses pre-scraped data stored in /data to keep
##   runtime under one minute.
##
## Authors:
##   Chadwa Khmissi (HQQ4WD)
##   Raymond Visser (B7INGB)
## ==============================


## ---- 1. Required packages ----

required_pkgs <- c(
  "readr",
  "tidyverse",
  "lubridate",
  "rvest",
  "dplyr",
  "glue",
  "stringr",
  "sqldf",
  "nnet",
  "tibble",
  "randomForest",
  "ggplot2",
  "cowplot",
  "grid",
  "png"
)


## ---- 2. Install missing packages ----

missing_pkgs <- required_pkgs[!(required_pkgs %in% installed.packages()[, "Package"])]

if (length(missing_pkgs) > 0) {
  message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
  install.packages(missing_pkgs)
}


## ---- 3. Load all packages ----

invisible(lapply(required_pkgs, library, character.only = TRUE))
message("All required packages are loaded.")


## ---- 4. Run full pipeline ----

source("scripts/07_combined_scripts.R")

