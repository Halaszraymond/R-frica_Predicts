## ==============================
## 04_modeling.R
## - Load engineered data
## - Train/test split
## - Train models
## - Save models to models/
## ==============================

library(tidyverse)
library(nnet)         # multinomial logistic regression (multinom)
library(randomForest) # Random Forest classifier

# ---------- Load engineered dataset ----------

# "data/model_data.rds" is produced by 03_feature_engineering.R
# It should contain one row per match with at least:
#  - result                    : factor ("H", "D", "A")
#  - rank_diff, points_diff    : FIFA ranking-based features
#  - is_afcon, is_qual,
#    is_friendly               : match type indicators
#  - year                      : match year
#  - h2h_matches_before_home,
#    h2h_goal_diff_before_home,
#    h2h_home_winrate_before  : head-to-head features (home perspective)
model_data <- readRDS("data/model_data.rds")

# If you ever want to drop/add predictors for the models,
# do it HERE (not in feature_engineering).
# This keeps feature engineering and model design separate.
model_data <- model_data |>
  dplyr::select(
    result,
    rank_diff,
    points_diff,
    is_afcon,
    is_qual,
    is_friendly,
    year,
    h2h_matches_before_home,
    h2h_goal_diff_before_home,
    h2h_home_winrate_before
  )

# Make sure result is a factor with expected levels
model_data <- model_data |>
  mutate(
    result = factor(result, levels = c("H", "D", "A"))
  )

# ---------- Train/test split ----------

# Time-based split: train on "past", test on "future".
# Adjust the cutoffs if you want:
#  - Training: matches up to and including 2022
#  - Test    : matches from 2024 onward
# (2023 is implicitly ignored here and can be used as a validation year.)
train_data <- model_data |> dplyr::filter(year <= 2022)
test_data  <- model_data |> dplyr::filter(year >= 2024)

# Target vectors
train_result <- train_data$result
test_result  <- test_data$result

# Predictor matrices (no 'result')
train_data_x <- train_data |> dplyr::select(-result)
test_data_x  <- test_data  |> dplyr::select(-result)

cat("Training set size:", nrow(train_data), "matches\n")
cat("Test set size    :", nrow(test_data), "matches\n\n")

# ---------- Multinomial logistic regression ----------

# Multinomial logistic regression models:
#   P(result = class | predictors)
# where class ∈ {H, D, A}.
set.seed(123)  # for reproducibility of optimization init
multinom_fit <- multinom(result ~ ., data = train_data, trace = FALSE)

# Class predictions on the test set
pred_multinom_class <- predict(multinom_fit, newdata = test_data)

# Class probabilities for each outcome on the test set
pred_multinom_prob  <- predict(multinom_fit, newdata = test_data, type = "prob")

# Simple evaluation: accuracy and confusion matrix
cat("Multinomial accuracy:\n")
print(mean(pred_multinom_class == test_result, na.rm = TRUE))

cat("Multinomial confusion matrix:\n")
print(table(Predicted = pred_multinom_class, Actual = test_result))

# ---------- Random Forest (used for simulation) ----------

# Random Forest classifier on the same predictors.
# Hyperparameters kept simple:
#  - ntree = 500 : number of trees
#  - mtry  = 3   : number of variables tried at each split
#  - importance = TRUE : store variable importance
set.seed(123)
rf_fit <- randomForest(
  result ~ .,
  data       = train_data,
  ntree      = 500,
  mtry       = 3,
  importance = TRUE
)

# Class predictions on the test set
pred_rf_class <- predict(rf_fit, newdata = test_data, type = "response")

# Predicted probabilities for each class
pred_rf_prob  <- predict(rf_fit, newdata = test_data, type = "prob")

# Evaluation: accuracy and confusion matrix
cat("\nRandom Forest accuracy:\n")
print(mean(pred_rf_class == test_result, na.rm = TRUE))

cat("Random Forest confusion matrix:\n")
print(table(Predicted = pred_rf_class, Actual = test_result))

# Variable importance: which predictors the RF relied on most
cat("\nRandom Forest variable importance:\n")
print(importance(rf_fit))

# This will draw a bar plot in the current graphics device
varImpPlot(rf_fit)

# ---------- Save trained models to models/ folder ----------

if (!dir.exists("models")) {
  dir.create("models", recursive = TRUE)
}

saveRDS(multinom_fit, file = "models/multinom_model.rds")
saveRDS(rf_fit,       file = "models/rf_model.rds")

cat("\nModels saved to:\n")
cat("  - models/multinom_model.rds\n")
cat("  - models/rf_model.rds\n\n")

print("Making Models: Done")


