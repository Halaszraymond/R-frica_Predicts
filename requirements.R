# R Package Requirements for AFCON Prediction Project
# Run this script to install all required packages

# List of required packages
required_packages <- c(
  "tidyverse",      # Data manipulation and visualization
  "readr",          # Reading CSV files
  "dplyr",          # Data manipulation
  "ggplot2",        # Data visualization
  "caret",          # Machine learning framework
  "randomForest",   # Random Forest algorithm
  "glmnet",         # Logistic regression with regularization
  "pROC",           # ROC curve analysis
  "knitr",          # Report generation
  "rmarkdown",      # Markdown documents
  "lubridate",      # Date manipulation
  "scales"          # Plot scaling
)

# Function to install missing packages
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages) > 0) {
    cat("Installing missing packages:", paste(new_packages, collapse = ", "), "\n")
    install.packages(new_packages, dependencies = TRUE)
  } else {
    cat("All required packages are already installed.\n")
  }
}

# Install packages
install_if_missing(required_packages)

# Load packages to verify installation
cat("\nVerifying package installation...\n")
for(pkg in required_packages) {
  if(require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat("✓", pkg, "loaded successfully\n")
  } else {
    cat("✗", pkg, "failed to load\n")
  }
}

cat("\nSetup complete!\n")
