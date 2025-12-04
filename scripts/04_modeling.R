## ==============================
## modeling.R
## - Load engineered data
## - Train/test split
## - Train models
## ==============================

library(tidyverse)
library(nnet)         # multinomial logistic
library(randomForest) # Random Forest

# ---------- Load engineered dataset ----------

model_data <- readRDS("data/model_data.rds")

# If you ever want to drop/add features for the model,
# do it HERE (not in feature_engineer.R)
model_data <- model_data |>
  select(result, rank_diff, points_diff, is_afcon, is_qual, is_friendly, year)

# ---------- Train/test split ----------

train_data <- model_data |> filter(year <= 2005)
test_data  <- model_data |> filter(year >= 2022)

train_result <- train_data$result
test_result  <- test_data$result

train_data_x <- train_data |> select(-result)
test_data_x  <- test_data  |> select(-result)

# ---------- Multinomial logistic regression ----------

set.seed(123)
multinom_fit <- multinom(result ~ ., data = train_data)

pred_multinom_class <- predict(multinom_fit, newdata = test_data)
pred_multinom_prob  <- predict(multinom_fit, newdata = test_data, type = "prob")

mean(pred_multinom_class == test_result)
table(Predicted = pred_multinom_class, Actual = test_result)

# ---------- Random Forest (this is used for simulation) ----------

set.seed(123)
rf_fit <- randomForest(
  result ~ .,
  data       = train_data,
  ntree      = 500,
  mtry       = 3,
  importance = TRUE
)

pred_rf_class <- predict(rf_fit, newdata = test_data, type = "response")
pred_rf_prob  <- predict(rf_fit, newdata = test_data, type = "prob")

mean(pred_rf_class == test_result)
table(Predicted = pred_rf_class, Actual = test_result)

importance(rf_fit)
varImpPlot(rf_fit)

# rf_fit stays in the environment -> used by simulation.R
