# scripts/01_train_models.R
# Purpose: Train machine learning models for match outcome prediction
# Models: Logistic Regression, Naive Bayes, Random Forest, and Decision Tree

# Set seed for reproducibility
set.seed(42)

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(caret)
  library(nnet)
  library(naivebayes)
  library(e1071)
  library(ranger)
  library(randomForest)
  library(rpart)
})

cat("=== Step 1: Train Models ===\n")

# Create directories if they don't exist
dir.create("models", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# Step 1: Load processed data
# ============================================================================

cat("\n=== Loading processed data ===\n")

data_file <- "data/processed/matches_features.csv"
if (!file.exists(data_file)) {
  stop("ERROR: Processed data file not found: ", data_file, "\n",
       "Please run scripts/00_download_and_prepare_data.R first.")
}

matches_features <- read_csv(data_file, show_col_types = FALSE)

cat("Loaded data:", nrow(matches_features), "rows,", ncol(matches_features), "columns\n")
cat("Target variable distribution:\n")
print(table(matches_features$result))

# ============================================================================
# Step 2: Data preprocessing
# ============================================================================

cat("\n=== Preprocessing data ===\n")

# Convert result to factor with explicit levels
matches_features <- matches_features %>%
  mutate(result = factor(result, levels = c("home", "draw", "away")))

# Remove date and team name columns for modeling
modeling_data <- matches_features %>%
  select(-any_of(c("match_date", "home_team", "away_team", "home_score", "away_score")))

# Convert match_importance to numeric if it exists
if ("match_importance" %in% names(modeling_data)) {
  modeling_data <- modeling_data %>%
    mutate(match_importance = as.numeric(match_importance))
}

# Handle missing values by removing rows with too many NAs
# Keep rows with at least 80% non-NA values
na_threshold <- 0.8
modeling_data <- modeling_data %>%
  filter(rowSums(!is.na(.)) / ncol(.) >= na_threshold)

cat("After removing incomplete rows:", nrow(modeling_data), "rows\n")

# For remaining NAs, impute with median
for (col in names(modeling_data)) {
  if (col != "result" && any(is.na(modeling_data[[col]]))) {
    median_val <- median(modeling_data[[col]], na.rm = TRUE)
    modeling_data[[col]][is.na(modeling_data[[col]])] <- median_val
    cat("Imputed", sum(is.na(modeling_data[[col]])), "missing values in", col, "\n")
  }
}

# Check for any remaining NAs
if (any(is.na(modeling_data))) {
  cat("WARNING: Some NA values remain. Removing rows with NAs.\n")
  modeling_data <- na.omit(modeling_data)
}

cat("Final dataset for modeling:", nrow(modeling_data), "rows\n")

# ============================================================================
# Step 3: Train/Test Split
# ============================================================================

cat("\n=== Creating train/test split ===\n")

# Stratified split 80/20
train_index <- createDataPartition(modeling_data$result, p = 0.8, list = FALSE)
train_data <- modeling_data[train_index, ]
test_data <- modeling_data[-train_index, ]

cat("Training set:", nrow(train_data), "rows\n")
cat("Test set:", nrow(test_data), "rows\n")
cat("\nTraining set distribution:\n")
print(table(train_data$result))
cat("\nTest set distribution:\n")
print(table(test_data$result))

# Save test data for evaluation script
saveRDS(test_data, "data/processed/test_data.rds")
cat("\nTest data saved to: data/processed/test_data.rds\n")

# ============================================================================
# Step 4: Set up training control
# ============================================================================

cat("\n=== Setting up cross-validation ===\n")

# 5-fold cross-validation, repeated 2 times
train_control <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 2,
  classProbs = TRUE,
  savePredictions = "final",
  summaryFunction = multiClassSummary,
  verboseIter = FALSE,
  seeds = set.seed(42)
)

# ============================================================================
# Step 5: Train Logistic Regression (Multinomial)
# ============================================================================

cat("\n=== Training Logistic Regression ===\n")

set.seed(42)
model_logistic <- train(
  result ~ .,
  data = train_data,
  method = "multinom",
  trControl = train_control,
  metric = "Accuracy",
  trace = FALSE,
  MaxNWts = 2000
)

cat("Logistic Regression trained.\n")
cat("Best accuracy:", max(model_logistic$results$Accuracy), "\n")

# Save model
saveRDS(model_logistic, "models/logistic_model.rds")
cat("Model saved to: models/logistic_model.rds\n")

# ============================================================================
# Step 6: Train Naive Bayes
# ============================================================================

cat("\n=== Training Naive Bayes ===\n")

set.seed(42)
model_nb <- train(
  result ~ .,
  data = train_data,
  method = "naive_bayes",
  trControl = train_control,
  metric = "Accuracy",
  tuneGrid = expand.grid(
    laplace = c(0, 0.5, 1),
    usekernel = c(TRUE, FALSE),
    adjust = c(0.5, 1, 1.5)
  )
)

cat("Naive Bayes trained.\n")
cat("Best accuracy:", max(model_nb$results$Accuracy), "\n")

# Save model
saveRDS(model_nb, "models/naivebayes_model.rds")
cat("Model saved to: models/naivebayes_model.rds\n")

# ============================================================================
# Step 7: Train Random Forest
# ============================================================================

cat("\n=== Training Random Forest ===\n")

set.seed(42)
model_rf <- train(
  result ~ .,
  data = train_data,
  method = "ranger",
  trControl = train_control,
  metric = "Accuracy",
  tuneGrid = expand.grid(
    mtry = c(2, 4, 6),
    splitrule = c("gini", "extratrees"),
    min.node.size = c(5, 10)
  ),
  importance = "impurity"
)

cat("Random Forest trained.\n")
cat("Best accuracy:", max(model_rf$results$Accuracy), "\n")

# Save model
saveRDS(model_rf, "models/rf_model.rds")
cat("Model saved to: models/rf_model.rds\n")

# ============================================================================
# Step 8: Train Decision Tree (rpart)
# ============================================================================

cat("\n=== Training Decision Tree (rpart) ===\n")

set.seed(42)
model_rpart <- train(
  result ~ .,
  data = train_data,
  method = "rpart",
  trControl = train_control,
  metric = "Accuracy",
  tuneGrid = expand.grid(
    cp = c(0.001, 0.01, 0.05, 0.1)
  )
)

cat("Decision Tree trained.\n")
cat("Best accuracy:", max(model_rpart$results$Accuracy), "\n")

# Save model
saveRDS(model_rpart, "models/rpart_model.rds")
cat("Model saved to: models/rpart_model.rds\n")

# ============================================================================
# Step 9: Compare models on cross-validation results
# ============================================================================

cat("\n=== Cross-validation results ===\n")

results <- resamples(list(
  Logistic = model_logistic,
  NaiveBayes = model_nb,
  RandomForest = model_rf,
  DecisionTree = model_rpart
))

cat("\nModel comparison summary:\n")
print(summary(results))

# Save results summary
results_summary <- summary(results)
write_csv(
  as.data.frame(results_summary$statistics$Accuracy),
  "outputs/cv_accuracy_summary.csv"
)
cat("\nCV results saved to: outputs/cv_accuracy_summary.csv\n")

cat("\n=== Model training complete ===\n")
cat("All models saved to models/ directory.\n")
cat("Next step: Run scripts/02_evaluate_and_plots.R to evaluate on test set.\n")
