#!/usr/bin/env Rscript
# Baseline Models for AFCON Match Prediction
# This script builds and evaluates logistic regression and random forest models

library(tidyverse)
library(caret)
library(randomForest)
library(pROC)

set.seed(42)  # For reproducibility

cat("=== AFCON Match Prediction - Baseline Models ===\n\n")

# Load cleaned data
cat("Loading cleaned data...\n")
data <- read_csv("data/processed/afcon_matches_clean.csv", 
                 show_col_types = FALSE)

cat(sprintf("Loaded %d matches\n\n", nrow(data)))

# ============================================================================
# DATA PREPARATION
# ============================================================================
cat("Preparing data for modeling...\n")

# Select features for modeling
model_data <- data %>%
  select(
    # Target variable
    home_win,
    
    # Predictor features
    is_knockout,
    is_final,
    is_semifinal,
    home_team_fifa_rank,
    away_team_fifa_rank,
    fifa_rank_diff,
    home_strength,
    away_strength,
    strength_difference,
    match_importance,
    tournament_year
  ) %>%
  drop_na()

cat(sprintf("Prepared %d observations with %d features\n", 
            nrow(model_data), ncol(model_data) - 1))

# Split data into training and testing sets (80/20 split)
# Use stratified sampling to maintain class balance
train_index <- createDataPartition(model_data$home_win, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

cat(sprintf("Training set: %d observations (%.1f%%)\n", 
            nrow(train_data), 100 * nrow(train_data) / nrow(model_data)))
cat(sprintf("Test set: %d observations (%.1f%%)\n", 
            nrow(test_data), 100 * nrow(test_data) / nrow(model_data)))

# Check class balance
cat("\nClass distribution in training set:\n")
print(table(train_data$home_win))
cat("\nClass distribution in test set:\n")
print(table(test_data$home_win))

# ============================================================================
# MODEL 1: LOGISTIC REGRESSION
# ============================================================================
cat("\n=== Building Logistic Regression Model ===\n")

# Train logistic regression model
logistic_model <- glm(home_win ~ ., 
                      data = train_data, 
                      family = binomial(link = "logit"))

cat("✓ Model trained\n")

# Model summary
cat("\nModel coefficients:\n")
print(summary(logistic_model)$coefficients)

# Make predictions on test set
logistic_probs <- predict(logistic_model, test_data, type = "response")
logistic_preds <- ifelse(logistic_probs > 0.5, 1, 0)

# Evaluate model performance
logistic_conf_matrix <- confusionMatrix(
  factor(logistic_preds, levels = c(0, 1)), 
  factor(test_data$home_win, levels = c(0, 1))
)

cat("\n=== Logistic Regression Performance ===\n")
print(logistic_conf_matrix)

# ROC curve and AUC
logistic_roc <- roc(test_data$home_win, logistic_probs, quiet = TRUE)
logistic_auc <- auc(logistic_roc)

cat(sprintf("\nAUC: %.4f\n", logistic_auc))

# Save ROC curve plot
png("outputs/plots/10_logistic_roc_curve.png", width = 800, height = 600, res = 100)
plot(logistic_roc, main = "Logistic Regression ROC Curve", 
     col = "#1976D2", lwd = 2)
text(0.5, 0.3, sprintf("AUC = %.4f", logistic_auc), cex = 1.5)
dev.off()
cat("✓ Saved: outputs/plots/10_logistic_roc_curve.png\n")

# Feature importance (based on absolute coefficient values)
logistic_importance <- data.frame(
  feature = names(coef(logistic_model))[-1],  # Exclude intercept
  importance = abs(coef(logistic_model)[-1])
) %>%
  arrange(desc(importance))

cat("\nFeature importance (Logistic Regression):\n")
print(logistic_importance)

# Save logistic regression model
saveRDS(logistic_model, "models/logistic_regression_model.rds")
cat("✓ Model saved: models/logistic_regression_model.rds\n")

# ============================================================================
# MODEL 2: RANDOM FOREST
# ============================================================================
cat("\n=== Building Random Forest Model ===\n")

# Train random forest model
rf_model <- randomForest(
  factor(home_win) ~ ., 
  data = train_data,
  ntree = 500,
  mtry = 3,
  importance = TRUE
)

cat("✓ Model trained\n")

cat("\nRandom Forest Details:\n")
print(rf_model)

# Make predictions on test set
rf_probs <- predict(rf_model, test_data, type = "prob")[, 2]
rf_preds <- predict(rf_model, test_data, type = "class")

# Evaluate model performance
rf_conf_matrix <- confusionMatrix(
  rf_preds, 
  factor(test_data$home_win, levels = c(0, 1))
)

cat("\n=== Random Forest Performance ===\n")
print(rf_conf_matrix)

# ROC curve and AUC
rf_roc <- roc(test_data$home_win, rf_probs, quiet = TRUE)
rf_auc <- auc(rf_roc)

cat(sprintf("\nAUC: %.4f\n", rf_auc))

# Save ROC curve plot
png("outputs/plots/11_random_forest_roc_curve.png", width = 800, height = 600, res = 100)
plot(rf_roc, main = "Random Forest ROC Curve", 
     col = "#2E7D32", lwd = 2)
text(0.5, 0.3, sprintf("AUC = %.4f", rf_auc), cex = 1.5)
dev.off()
cat("✓ Saved: outputs/plots/11_random_forest_roc_curve.png\n")

# Feature importance
rf_importance <- importance(rf_model)
rf_importance_df <- data.frame(
  feature = rownames(rf_importance),
  importance = rf_importance[, "MeanDecreaseGini"]
) %>%
  arrange(desc(importance))

cat("\nFeature importance (Random Forest):\n")
print(rf_importance_df)

# Plot feature importance
png("outputs/plots/12_rf_feature_importance.png", width = 800, height = 600, res = 100)
varImpPlot(rf_model, main = "Random Forest Feature Importance")
dev.off()
cat("✓ Saved: outputs/plots/12_rf_feature_importance.png\n")

# Save random forest model
saveRDS(rf_model, "models/random_forest_model.rds")
cat("✓ Model saved: models/random_forest_model.rds\n")

# ============================================================================
# MODEL COMPARISON
# ============================================================================
cat("\n=== Model Comparison ===\n")

comparison <- data.frame(
  Model = c("Logistic Regression", "Random Forest"),
  Accuracy = c(
    logistic_conf_matrix$overall["Accuracy"],
    rf_conf_matrix$overall["Accuracy"]
  ),
  Precision = c(
    logistic_conf_matrix$byClass["Precision"],
    rf_conf_matrix$byClass["Precision"]
  ),
  Recall = c(
    logistic_conf_matrix$byClass["Recall"],
    rf_conf_matrix$byClass["Recall"]
  ),
  F1_Score = c(
    logistic_conf_matrix$byClass["F1"],
    rf_conf_matrix$byClass["F1"]
  ),
  AUC = c(logistic_auc, rf_auc)
)

print(comparison)

# Save comparison results
write_csv(comparison, "outputs/model_comparison.csv")
cat("✓ Saved: outputs/model_comparison.csv\n")

# Visualize comparison
comparison_long <- comparison %>%
  pivot_longer(cols = -Model, names_to = "Metric", values_to = "Value")

png("outputs/plots/13_model_comparison.png", width = 1000, height = 600, res = 100)
ggplot(comparison_long, aes(x = Metric, y = Value, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = c("Logistic Regression" = "#1976D2", 
                                "Random Forest" = "#2E7D32")) +
  scale_y_continuous(limits = c(0, 1), labels = scales::percent) +
  labs(title = "Model Performance Comparison",
       subtitle = "Comparing Logistic Regression and Random Forest",
       x = "Metric", y = "Value") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14),
        axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()
cat("✓ Saved: outputs/plots/13_model_comparison.png\n")

# ============================================================================
# SAVE PREDICTIONS
# ============================================================================
cat("\nSaving predictions...\n")

predictions <- data.frame(
  actual = test_data$home_win,
  logistic_prob = logistic_probs,
  logistic_pred = logistic_preds,
  rf_prob = rf_probs,
  rf_pred = as.integer(as.character(rf_preds))
)

write_csv(predictions, "outputs/predictions/baseline_model_predictions.csv")
cat("✓ Saved: outputs/predictions/baseline_model_predictions.csv\n")

# ============================================================================
# SUMMARY
# ============================================================================
cat("\n=== Modeling Summary ===\n")
cat(sprintf("Training samples: %d\n", nrow(train_data)))
cat(sprintf("Test samples: %d\n", nrow(test_data)))
cat(sprintf("Number of features: %d\n", ncol(model_data) - 1))
cat("\nBest performing model: ")
if (rf_auc > logistic_auc) {
  cat("Random Forest\n")
  cat(sprintf("  - Accuracy: %.2f%%\n", 100 * rf_conf_matrix$overall["Accuracy"]))
  cat(sprintf("  - AUC: %.4f\n", rf_auc))
} else {
  cat("Logistic Regression\n")
  cat(sprintf("  - Accuracy: %.2f%%\n", 100 * logistic_conf_matrix$overall["Accuracy"]))
  cat(sprintf("  - AUC: %.4f\n", logistic_auc))
}

cat("\n✓ Baseline modeling complete!\n")
cat("Models saved in: models/\n")
cat("Results saved in: outputs/\n")
