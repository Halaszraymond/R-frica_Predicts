# requirements.R
# Install required packages for the African Football Match Prediction Pipeline
# Run this script before running the main pipeline

# Function to install packages if not already installed
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages) > 0) {
    message("Installing missing packages: ", paste(new_packages, collapse = ", "))
    install.packages(new_packages, dependencies = TRUE, repos = "https://cloud.r-project.org/")
  } else {
    message("All required packages are already installed.")
  }
}

# List of required packages
required_packages <- c(
  "tidyverse",      # Data manipulation and visualization
  "lubridate",      # Date/time handling
  "data.table",     # Fast data manipulation
  "caret",          # Machine learning framework
  "nnet",           # Multinomial logistic regression
  "klaR",           # Naive Bayes
  "e1071",          # Additional ML algorithms
  "naivebayes",     # Alternative Naive Bayes implementation
  "ranger",         # Fast random forest
  "randomForest",   # Alternative random forest
  "rpart",          # Decision trees
  "rpart.plot",     # Decision tree plotting
  "pROC",           # ROC curve analysis
  "cowplot",        # Plot arrangement
  "ggplot2",        # Plotting (included in tidyverse but listed explicitly)
  "recipes",        # Feature engineering framework
  "zoo"             # Rolling averages and time series
)

# Install missing packages
install_if_missing(required_packages)

# Print session info
message("\n=== Package Installation Complete ===")
message("Session Info:")
print(sessionInfo())
