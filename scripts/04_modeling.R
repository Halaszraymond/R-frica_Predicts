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

# The file "data/model_data.rds" is assumed to be the *final* cleaned
# and feature-engineered dataset produced by feature_engineer.R.
# It should already contain one row per match with:
#  - 'result'       : factor with levels like "H", "D", "A"
#  - 'rank_diff'    : numeric difference in FIFA ranking
#  - 'points_diff'  : numeric difference in FIFA points
#  - 'is_afcon'     : logical/0-1 indicator
#  - 'is_qual'      : logical/0-1 indicator
#  - 'is_friendly'  : logical/0-1 indicator
#  - 'year'         : match year (used for time-based train/test split)
model_data <- readRDS("data/model_data.rds")

# If you ever want to drop/add features for the model,
# do it HERE (not in feature_engineer.R).
# This keeps feature engineering and model design clearly separated.
model_data <- model_data |>
  dplyr::select(
    result,       # target variable (match outcome)
    rank_diff,    # difference in FIFA rank between teams
    points_diff,  # difference in FIFA points between teams
    is_afcon,     # AFCON tournament indicator
    is_qual,      # qualification match indicator
    is_friendly,  # friendly match indicator
    year          # calendar year, used for splitting into train/test
  )

# ---------- Train/test split ----------

# We use a *time-based* split instead of random:
#  - Training data: matches up to and including 2005
#  - Test data    : matches from 2022 onward
# This mimics "training on the past and testing on the future",
# which is more realistic for prediction tasks.
train_data <- model_data |> dplyr::filter(year <= 2022)
test_data  <- model_data |> dplyr::filter(year >= 2024)

# For convenience, keep the target (result) separately as vectors.
train_result <- train_data$result
test_result  <- test_data$result

# Also keep versions of the data that only contain predictors (no 'result').
# These are not strictly necessary for the models below,
# but can be useful for manual inspection or other experiments.
train_data_x <- train_data |> dplyr::select(-result)
test_data_x  <- test_data  |> dplyr::select(-result)

# ---------- Multinomial logistic regression ----------

# Multinomial logistic regression models P(result = class | predictors)
# for more than two classes (here likely "H", "D", "A").
# The formula 'result ~ .' means:
#   result is predicted from all other columns in train_data.
set.seed(123)  # set seed for reproducibility (mainly for internal init)
multinom_fit <- multinom(result ~ ., data = train_data)

# Class predictions on the test set
pred_multinom_class <- predict(multinom_fit, newdata = test_data)

# Class *probabilities* for each outcome on the test set
# (one column per class, e.g. H / D / A).
pred_multinom_prob  <- predict(multinom_fit, newdata = test_data, type = "prob")

# Simple evaluation: overall accuracy and confusion matrix.
cat("Multinomial accuracy:\n")
print(mean(pred_multinom_class == test_result))

cat("Multinomial confusion matrix:\n")
print(table(Predicted = pred_multinom_class, Actual = test_result))

# ---------- Random Forest (this is used for simulation) ----------

# Random Forest is an ensemble of decision trees. It can capture
# non-linear relationships and interactions between predictors.
# We keep its hyperparameters simple here:
#   - ntree = 500 : number of trees in the forest
#   - mtry  = 3   : number of variables tried at each split
#   - importance = TRUE : store variable importance measures
set.seed(123)  # Random Forest is stochastic; seed ensures reproducibility
rf_fit <- randomForest(
  result ~ .,
  data       = train_data,
  ntree      = 500,
  mtry       = 3,
  importance = TRUE
)

# Class predictions for the test set
pred_rf_class <- predict(rf_fit, newdata = test_data, type = "response")

# Predicted *probabilities* for each class on the test set.
# This is important later for simulations, where we will sample
# match outcomes according to these probabilities.
pred_rf_prob  <- predict(rf_fit, newdata = test_data, type = "prob")

# Evaluation: accuracy and confusion matrix for the Random Forest model.
cat("\nRandom Forest accuracy:\n")
print(mean(pred_rf_class == test_result))

cat("Random Forest confusion matrix:\n")
print(table(Predicted = pred_rf_class, Actual = test_result))

# Variable importance gives an indication of which predictors
# the Random Forest used most to split the data.
cat("\nRandom Forest variable importance:\n")
print(importance(rf_fit))

# varImpPlot shows a bar plot of variable importance.
# This opens a plot in the current graphics device.
varImpPlot(rf_fit)

# ---------- Save trained models to models/ folder ----------

# Ensure the 'models' directory exists before saving.
# This avoids errors if the folder hasn't been created manually.
if (!dir.exists("models")) {
  dir.create("models", recursive = TRUE)
}

# Save both models to disk as .rds files so they can be loaded later
# (e.g. in simulation.R). This makes the modeling step reproducible
# and decouples "training" from "using the model".
saveRDS(multinom_fit, file = "models/multinom_model.rds")
saveRDS(rf_fit,       file = "models/rf_model.rds")

cat("\nModels saved to models/multinom_model.rds and models/rf_model.rds\n")

# rf_fit (and multinom_fit) stay in the R environment after this script runs,
# so other scripts (like simulation.R) can either:
#   - use them directly if modeling.R was sourced in the same session, OR
#   - reload them from disk via readRDS("models/rf_model.rds") etc.

print("Making Models: Done")

