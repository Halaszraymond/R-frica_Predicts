# scripts/02_evaluate_and_plots.R
# Purpose: Evaluate trained models on test set and generate plots and reports

# Set seed for reproducibility
set.seed(42)

# Load required libraries
suppressPackageStartupMessages({
  library(tidyverse)
  library(caret)
  library(pROC)
  library(cowplot)
  library(rpart.plot)
})

cat("=== Step 2: Evaluate Models and Generate Plots ===\n")

# Create directories if they don't exist
dir.create("outputs", recursive = TRUE, showWarnings = FALSE)
dir.create("plots", recursive = TRUE, showWarnings = FALSE)

# ============================================================================
# Step 1: Load test data and models
# ============================================================================

cat("\n=== Loading test data and models ===\n")

# Load test data
test_data_file <- "data/processed/test_data.rds"
if (!file.exists(test_data_file)) {
  stop("ERROR: Test data not found. Please run scripts/01_train_models.R first.")
}
test_data <- readRDS(test_data_file)
cat("Test data loaded:", nrow(test_data), "rows\n")

# Load models
models <- list(
  Logistic = readRDS("models/logistic_model.rds"),
  NaiveBayes = readRDS("models/naivebayes_model.rds"),
  RandomForest = readRDS("models/rf_model.rds"),
  DecisionTree = readRDS("models/rpart_model.rds")
)

cat("All models loaded successfully.\n")

# ============================================================================
# Step 2: Generate predictions
# ============================================================================

cat("\n=== Generating predictions ===\n")

predictions <- list()
pred_probs <- list()

for (model_name in names(models)) {
  cat("Predicting with", model_name, "...\n")
  predictions[[model_name]] <- predict(models[[model_name]], newdata = test_data)
  pred_probs[[model_name]] <- predict(models[[model_name]], newdata = test_data, type = "prob")
}

# ============================================================================
# Step 3: Calculate performance metrics
# ============================================================================

cat("\n=== Calculating performance metrics ===\n")

# Function to calculate comprehensive metrics
calc_metrics <- function(actual, predicted, probs) {
  # Confusion matrix
  cm <- confusionMatrix(predicted, actual)
  
  # Overall accuracy
  accuracy <- cm$overall["Accuracy"]
  
  # Per-class metrics
  class_metrics <- cm$byClass
  
  # Multi-class ROC AUC (one-vs-rest)
  roc_auc <- list()
  for (class in levels(actual)) {
    binary_actual <- ifelse(actual == class, 1, 0)
    binary_prob <- probs[[class]]
    roc_obj <- roc(binary_actual, binary_prob, quiet = TRUE)
    roc_auc[[class]] <- auc(roc_obj)
  }
  
  list(
    confusion_matrix = cm,
    accuracy = accuracy,
    class_metrics = class_metrics,
    roc_auc = roc_auc
  )
}

# Calculate metrics for each model
metrics <- list()
for (model_name in names(models)) {
  cat("Calculating metrics for", model_name, "...\n")
  metrics[[model_name]] <- calc_metrics(
    actual = test_data$result,
    predicted = predictions[[model_name]],
    probs = pred_probs[[model_name]]
  )
}

# ============================================================================
# Step 4: Create summary tables
# ============================================================================

cat("\n=== Creating summary tables ===\n")

# Overall accuracy summary
accuracy_df <- data.frame(
  Model = names(metrics),
  Accuracy = sapply(metrics, function(m) m$accuracy),
  row.names = NULL
)

# Add AUC for each class
for (class in c("home", "draw", "away")) {
  accuracy_df[[paste0("AUC_", class)]] <- sapply(
    metrics,
    function(m) as.numeric(m$roc_auc[[class]])
  )
}

# Mean AUC
accuracy_df$AUC_mean <- rowMeans(accuracy_df[, grep("AUC_", names(accuracy_df))])

# Sort by accuracy
accuracy_df <- accuracy_df %>% arrange(desc(Accuracy))

cat("\nModel Performance Summary:\n")
print(accuracy_df)

# Save summary
write_csv(accuracy_df, "outputs/summary_model_performance.csv")
cat("\nPerformance summary saved to: outputs/summary_model_performance.csv\n")

# Per-class metrics for best model
best_model_name <- accuracy_df$Model[1]
best_metrics <- metrics[[best_model_name]]

cat("\nBest model:", best_model_name, "\n")
cat("Confusion Matrix:\n")
print(best_metrics$confusion_matrix$table)

# Extract per-class metrics
class_metrics_df <- as.data.frame(best_metrics$class_metrics)
class_metrics_df$Class <- rownames(class_metrics_df)
class_metrics_df <- class_metrics_df %>%
  select(Class, Sensitivity, Specificity, Precision = `Pos Pred Value`, 
         F1, `Balanced Accuracy`)

write_csv(class_metrics_df, paste0("outputs/", best_model_name, "_class_metrics.csv"))
cat("Class metrics saved to: outputs/", best_model_name, "_class_metrics.csv\n")

# ============================================================================
# Step 5: Plot confusion matrices
# ============================================================================

cat("\n=== Generating confusion matrix plots ===\n")

for (model_name in names(metrics)) {
  cm <- metrics[[model_name]]$confusion_matrix$table
  cm_df <- as.data.frame(cm)
  names(cm_df) <- c("Predicted", "Actual", "Freq")
  
  p <- ggplot(cm_df, aes(x = Actual, y = Predicted, fill = Freq)) +
    geom_tile(color = "white") +
    geom_text(aes(label = Freq), color = "white", size = 6) +
    scale_fill_gradient(low = "#3B4371", high = "#E63946") +
    labs(
      title = paste("Confusion Matrix:", model_name),
      x = "Actual Outcome",
      y = "Predicted Outcome"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text = element_text(size = 11),
      legend.position = "right"
    )
  
  ggsave(
    filename = paste0("plots/confusion_matrix_", tolower(gsub(" ", "_", model_name)), ".png"),
    plot = p,
    width = 7,
    height = 6,
    dpi = 300
  )
}

cat("Confusion matrix plots saved to plots/ directory.\n")

# ============================================================================
# Step 6: Plot model comparison
# ============================================================================

cat("\n=== Generating model comparison plot ===\n")

accuracy_long <- accuracy_df %>%
  select(Model, Accuracy, AUC_mean) %>%
  pivot_longer(cols = c(Accuracy, AUC_mean), names_to = "Metric", values_to = "Value")

p_comparison <- ggplot(accuracy_long, aes(x = Model, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  scale_fill_manual(values = c("Accuracy" = "#2A9D8F", "AUC_mean" = "#E76F51")) +
  labs(
    title = "Model Performance Comparison",
    x = "Model",
    y = "Score",
    fill = "Metric"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  ) +
  ylim(0, 1)

ggsave(
  filename = "plots/model_comparison.png",
  plot = p_comparison,
  width = 8,
  height = 6,
  dpi = 300
)

cat("Model comparison plot saved to: plots/model_comparison.png\n")

# ============================================================================
# Step 7: Plot ROC curves (one-vs-rest)
# ============================================================================

cat("\n=== Generating ROC curves ===\n")

for (model_name in names(models)) {
  # Create ROC curve data for each class
  roc_data <- list()
  
  for (class in c("home", "draw", "away")) {
    binary_actual <- ifelse(test_data$result == class, 1, 0)
    binary_prob <- pred_probs[[model_name]][[class]]
    roc_obj <- roc(binary_actual, binary_prob, quiet = TRUE)
    
    roc_df <- data.frame(
      FPR = 1 - roc_obj$specificities,
      TPR = roc_obj$sensitivities,
      Class = paste(class, "(AUC =", round(auc(roc_obj), 3), ")")
    )
    roc_data[[class]] <- roc_df
  }
  
  roc_combined <- bind_rows(roc_data)
  
  p_roc <- ggplot(roc_combined, aes(x = FPR, y = TPR, color = Class)) +
    geom_line(size = 1) +
    geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
    labs(
      title = paste("ROC Curves (One-vs-Rest):", model_name),
      x = "False Positive Rate",
      y = "True Positive Rate",
      color = "Class"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      legend.position = "bottom"
    ) +
    coord_fixed()
  
  ggsave(
    filename = paste0("plots/roc_curves_", tolower(gsub(" ", "_", model_name)), ".png"),
    plot = p_roc,
    width = 8,
    height = 7,
    dpi = 300
  )
}

cat("ROC curve plots saved to plots/ directory.\n")

# ============================================================================
# Step 8: Variable importance for Random Forest
# ============================================================================

cat("\n=== Generating variable importance plot ===\n")

if ("RandomForest" %in% names(models)) {
  # Extract variable importance
  rf_model <- models$RandomForest
  importance_df <- varImp(rf_model)$importance
  importance_df$Variable <- rownames(importance_df)
  importance_df <- importance_df %>%
    arrange(desc(Overall)) %>%
    head(15)
  
  p_importance <- ggplot(importance_df, aes(x = reorder(Variable, Overall), y = Overall)) +
    geom_bar(stat = "identity", fill = "#264653") +
    coord_flip() +
    labs(
      title = "Top 15 Variable Importance (Random Forest)",
      x = "Variable",
      y = "Importance"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    )
  
  ggsave(
    filename = "plots/variable_importance_rf.png",
    plot = p_importance,
    width = 8,
    height = 6,
    dpi = 300
  )
  
  # Save importance table
  write_csv(importance_df, "outputs/variable_importance_rf.csv")
  cat("Variable importance plot and table saved.\n")
}

# ============================================================================
# Step 9: Decision tree visualization
# ============================================================================

cat("\n=== Generating decision tree plot ===\n")

if ("DecisionTree" %in% names(models)) {
  png("plots/decision_tree.png", width = 1200, height = 800, res = 100)
  rpart.plot(
    models$DecisionTree$finalModel,
    main = "Decision Tree for Match Outcome Prediction",
    extra = 104,
    box.palette = "RdYlGn",
    shadow.col = "gray",
    nn = TRUE
  )
  dev.off()
  cat("Decision tree plot saved to: plots/decision_tree.png\n")
}

# ============================================================================
# Step 10: Save all predictions
# ============================================================================

cat("\n=== Saving predictions ===\n")

predictions_df <- data.frame(
  Actual = test_data$result
)

for (model_name in names(predictions)) {
  predictions_df[[paste0("Pred_", model_name)]] <- predictions[[model_name]]
}

write_csv(predictions_df, "outputs/test_predictions.csv")
cat("Test predictions saved to: outputs/test_predictions.csv\n")

cat("\n=== Evaluation complete ===\n")
cat("\nSummary of outputs:\n")
cat("  - Performance summary: outputs/summary_model_performance.csv\n")
cat("  - Confusion matrices: plots/confusion_matrix_*.png\n")
cat("  - Model comparison: plots/model_comparison.png\n")
cat("  - ROC curves: plots/roc_curves_*.png\n")
cat("  - Variable importance: plots/variable_importance_rf.png\n")
cat("  - Decision tree: plots/decision_tree.png\n")
cat("  - Predictions: outputs/test_predictions.csv\n")
