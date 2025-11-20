#!/usr/bin/env Rscript
# Main Pipeline Script for AFCON Prediction Project
# This script runs the complete analysis pipeline from data generation to prediction

cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║         AFCON 2025 PREDICTION PROJECT - MAIN PIPELINE     ║\n")
cat("║                    R-frica_Predicts                        ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

# Check if required packages are installed
required_packages <- c("tidyverse", "caret", "randomForest", "pROC")
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(missing_packages) > 0) {
  cat("⚠ Warning: Some required packages are missing.\n")
  cat("Please run 'Rscript requirements.R' first to install dependencies.\n")
  cat("Missing packages:", paste(missing_packages, collapse = ", "), "\n\n")
  cat("Attempting to install now...\n")
  source("requirements.R")
}

# Start timing
start_time <- Sys.time()

# ============================================================================
# STEP 1: Generate Data
# ============================================================================
cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("STEP 1: GENERATING SYNTHETIC AFCON DATA\n")
cat("═══════════════════════════════════════════════════════════════\n")
source("scripts/00_generate_data.R")

# ============================================================================
# STEP 2: Data Cleaning
# ============================================================================
cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("STEP 2: CLEANING AND PROCESSING DATA\n")
cat("═══════════════════════════════════════════════════════════════\n")
source("scripts/01_data_cleaning.R")

# ============================================================================
# STEP 3: Exploratory Data Analysis
# ============================================================================
cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("STEP 3: EXPLORATORY DATA ANALYSIS\n")
cat("═══════════════════════════════════════════════════════════════\n")
source("scripts/02_exploratory_analysis.R")

# ============================================================================
# STEP 4: Build Baseline Models
# ============================================================================
cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("STEP 4: BUILDING BASELINE MODELS\n")
cat("═══════════════════════════════════════════════════════════════\n")
source("scripts/03_baseline_models.R")

# ============================================================================
# STEP 5: Tournament Simulation
# ============================================================================
cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("STEP 5: SIMULATING AFCON 2025 TOURNAMENT\n")
cat("═══════════════════════════════════════════════════════════════\n")
source("scripts/04_tournament_simulation.R")

# ============================================================================
# PIPELINE COMPLETE
# ============================================================================
end_time <- Sys.time()
duration <- as.numeric(difftime(end_time, start_time, units = "secs"))

cat("\n")
cat("╔════════════════════════════════════════════════════════════╗\n")
cat("║                    PIPELINE COMPLETE!                      ║\n")
cat("╚════════════════════════════════════════════════════════════╝\n\n")

cat(sprintf("Total execution time: %.1f seconds (%.1f minutes)\n", 
            duration, duration / 60))

cat("\n📊 Generated Outputs:\n")
cat("   - Raw data: data/raw/\n")
cat("   - Processed data: data/processed/\n")
cat("   - Visualizations: outputs/plots/\n")
cat("   - Models: models/\n")
cat("   - Predictions: outputs/predictions/\n")

cat("\n🔍 Key Files to Review:\n")
cat("   1. outputs/eda_summary_statistics.csv - Data overview\n")
cat("   2. outputs/model_comparison.csv - Model performance\n")
cat("   3. outputs/predictions/afcon_2025_prediction.csv - Tournament winner prediction\n")
cat("   4. outputs/plots/ - All visualizations\n")

cat("\n✅ Project ready for review!\n")
cat("   See README.md for detailed documentation\n\n")
