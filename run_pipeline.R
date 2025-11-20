# run_pipeline.R
# Orchestrate the complete African Football Match Prediction Pipeline
# 
# Usage:
#   Rscript run_pipeline.R                # Run full pipeline with Kaggle download attempt
#   Rscript run_pipeline.R --skip-download # Skip Kaggle download, use existing data

cat("================================================\n")
cat("  African Football Match Prediction Pipeline\n")
cat("================================================\n\n")

# Parse command line arguments
args <- commandArgs(trailingOnly = TRUE)
skip_download <- "--skip-download" %in% args

# Set seed for reproducibility
set.seed(42)

# ============================================================================
# Step 0: Check and install requirements
# ============================================================================

cat("=== Checking R package requirements ===\n")
if (file.exists("requirements.R")) {
  cat("Installing/checking required packages...\n")
  source("requirements.R")
} else {
  cat("WARNING: requirements.R not found. Proceeding anyway...\n")
}

# ============================================================================
# Step 1: Download and prepare data
# ============================================================================

cat("\n================================================\n")
cat("Step 1: Download and Prepare Data\n")
cat("================================================\n")

if (skip_download) {
  cat("Skipping Kaggle download (--skip-download flag set).\n")
  cat("Assuming data files are already in data/raw/\n\n")
  Sys.setenv(SKIP_KAGGLE_DOWNLOAD = "TRUE")
}

script_path <- "scripts/00_download_and_prepare_data.R"
if (!file.exists(script_path)) {
  stop("ERROR: Script not found: ", script_path)
}

start_time <- Sys.time()
tryCatch({
  source(script_path)
  cat("\n✓ Data preparation completed successfully.\n")
}, error = function(e) {
  cat("\n✗ Error in data preparation:\n")
  cat(conditionMessage(e), "\n")
  stop("Pipeline failed at Step 1: Data Preparation")
})
end_time <- Sys.time()
cat("Duration:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")

# ============================================================================
# Step 2: Train models
# ============================================================================

cat("\n================================================\n")
cat("Step 2: Train Models\n")
cat("================================================\n")

script_path <- "scripts/01_train_models.R"
if (!file.exists(script_path)) {
  stop("ERROR: Script not found: ", script_path)
}

start_time <- Sys.time()
tryCatch({
  source(script_path)
  cat("\n✓ Model training completed successfully.\n")
}, error = function(e) {
  cat("\n✗ Error in model training:\n")
  cat(conditionMessage(e), "\n")
  stop("Pipeline failed at Step 2: Model Training")
})
end_time <- Sys.time()
cat("Duration:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")

# ============================================================================
# Step 3: Evaluate models and generate plots
# ============================================================================

cat("\n================================================\n")
cat("Step 3: Evaluate Models and Generate Plots\n")
cat("================================================\n")

script_path <- "scripts/02_evaluate_and_plots.R"
if (!file.exists(script_path)) {
  stop("ERROR: Script not found: ", script_path)
}

start_time <- Sys.time()
tryCatch({
  source(script_path)
  cat("\n✓ Model evaluation completed successfully.\n")
}, error = function(e) {
  cat("\n✗ Error in model evaluation:\n")
  cat(conditionMessage(e), "\n")
  stop("Pipeline failed at Step 3: Model Evaluation")
})
end_time <- Sys.time()
cat("Duration:", round(difftime(end_time, start_time, units = "mins"), 2), "minutes\n")

# ============================================================================
# Pipeline complete
# ============================================================================

cat("\n================================================\n")
cat("  Pipeline Completed Successfully!\n")
cat("================================================\n\n")

cat("Output locations:\n")
cat("  - Processed data: data/processed/\n")
cat("  - Trained models: models/\n")
cat("  - Evaluation results: outputs/\n")
cat("  - Plots and visualizations: plots/\n\n")

cat("Key outputs:\n")
cat("  - outputs/summary_model_performance.csv\n")
cat("  - plots/model_comparison.png\n")
cat("  - plots/confusion_matrix_*.png\n")
cat("  - plots/roc_curves_*.png\n")
cat("  - plots/variable_importance_rf.png\n")
cat("  - plots/decision_tree.png\n\n")

cat("Next steps:\n")
cat("  1. Review model performance in outputs/summary_model_performance.csv\n")
cat("  2. Examine plots in plots/ directory\n")
cat("  3. Use trained models from models/ for predictions\n\n")
