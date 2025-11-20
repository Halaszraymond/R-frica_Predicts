# R-frica_Predicts Project Configuration
# This file is automatically loaded when R starts in this project directory

# Set options for better output
options(
  width = 120,
  digits = 4,
  scipen = 10,
  max.print = 100
)

# Display welcome message
.First <- function() {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════╗\n")
  cat("║         Welcome to R-frica_Predicts Project!              ║\n")
  cat("║    AFCON 2025 Match Outcome Prediction & Analysis         ║\n")
  cat("╚════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("📚 Quick Start:\n")
  cat("   1. Install packages: source('requirements.R')\n")
  cat("   2. Run full pipeline: source('run_pipeline.R')\n")
  cat("   3. See QUICKSTART.md for more commands\n")
  cat("\n")
  cat("📁 Project Structure:\n")
  cat("   • data/         - Raw and processed AFCON data\n")
  cat("   • scripts/      - Analysis and modeling scripts\n")
  cat("   • models/       - Trained ML models\n")
  cat("   • outputs/      - Plots and predictions\n")
  cat("\n")
  cat("🔧 Useful commands:\n")
  cat("   • list.files('scripts/')      - List all scripts\n")
  cat("   • list.files('outputs/plots') - List generated plots\n")
  cat("   • ?source                     - Get help on running scripts\n")
  cat("\n")
}

# Cleanup function (called when R session ends)
.Last <- function() {
  cat("\n")
  cat("Thank you for using R-frica_Predicts! 🏆⚽\n")
  cat("\n")
}

# Helper function to quickly load project data
load_data <- function() {
  if (file.exists("data/processed/afcon_matches_clean.csv")) {
    data <- read.csv("data/processed/afcon_matches_clean.csv")
    cat("✓ Loaded clean AFCON data:", nrow(data), "matches\n")
    return(data)
  } else {
    cat("⚠ Clean data not found. Run: source('scripts/01_data_cleaning.R')\n")
    return(NULL)
  }
}

# Helper function to quickly load models
load_models <- function() {
  models <- list()
  
  if (file.exists("models/logistic_regression_model.rds")) {
    models$logistic <- readRDS("models/logistic_regression_model.rds")
    cat("✓ Loaded logistic regression model\n")
  }
  
  if (file.exists("models/random_forest_model.rds")) {
    models$random_forest <- readRDS("models/random_forest_model.rds")
    cat("✓ Loaded random forest model\n")
  }
  
  if (length(models) == 0) {
    cat("⚠ No models found. Run: source('scripts/03_baseline_models.R')\n")
    return(NULL)
  }
  
  return(models)
}

# Helper function to view tournament prediction
view_prediction <- function() {
  if (file.exists("outputs/predictions/afcon_2025_prediction.csv")) {
    pred <- read.csv("outputs/predictions/afcon_2025_prediction.csv")
    cat("\n")
    cat("🏆 AFCON 2025 Prediction 🏆\n")
    cat("═════════════════════════════\n")
    print(pred, row.names = FALSE)
    cat("\n")
  } else {
    cat("⚠ Prediction not found. Run: source('scripts/04_tournament_simulation.R')\n")
  }
}

# Make helper functions available
cat("Helper functions loaded: load_data(), load_models(), view_prediction()\n")
