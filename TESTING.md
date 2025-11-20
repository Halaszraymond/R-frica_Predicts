# Testing Guide

This document provides guidance for testing the R-frica_Predicts project.

## Prerequisites

Before running tests, ensure you have:
1. R (version 4.0 or higher) installed
2. All required packages installed (run `source("requirements.R")`)

## Manual Testing Steps

### 1. Verify Package Installation

```r
# Run this in R console
source("requirements.R")
```

Expected output:
- All packages should install successfully
- Each package should load with a ✓ checkmark

### 2. Test Data Generation

```r
# Generate synthetic data
source("scripts/00_generate_data.R")
```

Expected outputs:
- File created: `data/raw/afcon_matches_2000_2023.csv`
- Console output showing ~520 matches generated
- Summary statistics displayed

Validation checks:
```r
# Load and verify data
data <- read.csv("data/raw/afcon_matches_2000_2023.csv")
nrow(data)  # Should be around 520
ncol(data)  # Should be 12
summary(data)  # Check for reasonable ranges
```

### 3. Test Data Cleaning

```r
# Clean the data
source("scripts/01_data_cleaning.R")
```

Expected outputs:
- File created: `data/processed/afcon_matches_clean.csv`
- File created: `data/processed/team_statistics.csv`
- Console output showing cleaning steps
- No missing values
- No duplicates

Validation checks:
```r
# Load and verify cleaned data
clean_data <- read.csv("data/processed/afcon_matches_clean.csv")
ncol(clean_data) > 12  # Should have more columns (engineered features)
sum(is.na(clean_data))  # Should be 0
```

### 4. Test Exploratory Analysis

```r
# Run EDA
source("scripts/02_exploratory_analysis.R")
```

Expected outputs:
- 9 PNG files in `outputs/plots/` directory
- File created: `outputs/eda_summary_statistics.csv`
- Console output with summary statistics

Validation checks:
```r
# Check plots exist
list.files("outputs/plots", pattern = "*.png")  # Should show 9 files
file.size("outputs/plots/01_match_outcomes.png") > 0  # Should be > 0
```

### 5. Test Model Training

```r
# Train models
source("scripts/03_baseline_models.R")
```

Expected outputs:
- 2 model files in `models/` directory:
  - `logistic_regression_model.rds`
  - `random_forest_model.rds`
- 4 additional plots (ROC curves, importance, comparison)
- File created: `outputs/model_comparison.csv`
- File created: `outputs/predictions/baseline_model_predictions.csv`
- Console output with model performance metrics

Validation checks:
```r
# Load and verify models
log_model <- readRDS("models/logistic_regression_model.rds")
rf_model <- readRDS("models/random_forest_model.rds")
class(log_model)  # Should be "glm"
class(rf_model)   # Should be "randomForest"

# Check predictions
preds <- read.csv("outputs/predictions/baseline_model_predictions.csv")
nrow(preds) > 0  # Should have predictions
all(preds$logistic_prob >= 0 & preds$logistic_prob <= 1)  # Should be TRUE
```

### 6. Test Tournament Simulation

```r
# Simulate tournament
source("scripts/04_tournament_simulation.R")
```

Expected outputs:
- File created: `outputs/predictions/afcon_2025_prediction.csv`
- File created: `outputs/predictions/afcon_2025_qualified_teams.csv`
- Console output showing:
  - Group stage results
  - Knockout bracket
  - Final winner

Validation checks:
```r
# Load and verify predictions
prediction <- read.csv("outputs/predictions/afcon_2025_prediction.csv")
nrow(prediction)  # Should be 3 (Champion, Runner-up, Third Place)
prediction$Position  # Should have all three positions
```

### 7. Test Full Pipeline

```r
# Run entire pipeline
source("run_pipeline.R")
```

Expected outputs:
- All outputs from steps 2-6
- Console showing progress through all 5 steps
- Total execution time displayed
- Success message at the end

Validation checks:
```r
# Verify all key outputs exist
files_to_check <- c(
  "data/raw/afcon_matches_2000_2023.csv",
  "data/processed/afcon_matches_clean.csv",
  "data/processed/team_statistics.csv",
  "models/logistic_regression_model.rds",
  "models/random_forest_model.rds",
  "outputs/eda_summary_statistics.csv",
  "outputs/model_comparison.csv",
  "outputs/predictions/afcon_2025_prediction.csv"
)

all(file.exists(files_to_check))  # Should be TRUE
```

## Common Issues and Solutions

### Issue: Package installation fails
**Solution**: 
- Check internet connection
- Try installing packages individually
- Update R to latest version
- Check CRAN mirror: `chooseCRANmirror()`

### Issue: Data generation produces different results
**Solution**: 
- This is expected due to randomness
- Check that set.seed() is called (it is in the scripts)
- Results should be similar but not identical

### Issue: Plots are not generated
**Solution**: 
- Ensure graphics device is available
- Try: `png("test.png"); plot(1:10); dev.off()`
- Check write permissions in outputs/plots/

### Issue: Model training is very slow
**Solution**: 
- Random forest can take 1-2 minutes - this is normal
- Reduce ntree in random forest for faster testing
- Ensure sufficient RAM available (>2GB recommended)

### Issue: Scripts fail with "object not found"
**Solution**: 
- Ensure scripts are run in order (00, 01, 02, 03, 04)
- Check working directory is project root
- Verify previous scripts completed successfully

## Performance Benchmarks

On a typical modern computer (4+ cores, 8GB+ RAM):

| Script | Expected Time |
|--------|--------------|
| 00_generate_data.R | 5-10 seconds |
| 01_data_cleaning.R | 2-5 seconds |
| 02_exploratory_analysis.R | 15-30 seconds |
| 03_baseline_models.R | 30-90 seconds |
| 04_tournament_simulation.R | 5-10 seconds |
| **Total Pipeline** | **1-3 minutes** |

If your execution time is significantly longer, consider:
- Closing other applications
- Reducing plot resolution
- Reducing random forest trees (ntree parameter)

## Automated Testing (Advanced)

For those familiar with R testing frameworks:

```r
# Install testing packages
install.packages("testthat")
install.packages("assertthat")

# Example test structure (not included in project)
library(testthat)

test_that("Data generation produces valid output", {
  source("scripts/00_generate_data.R")
  data <- read.csv("data/raw/afcon_matches_2000_2023.csv")
  
  expect_true(nrow(data) > 500)
  expect_equal(ncol(data), 12)
  expect_true(all(data$home_goals >= 0))
  expect_true(all(data$away_goals >= 0))
})

test_that("Models train successfully", {
  source("scripts/03_baseline_models.R")
  
  expect_true(file.exists("models/logistic_regression_model.rds"))
  expect_true(file.exists("models/random_forest_model.rds"))
})
```

## Visual Validation Checklist

After running the pipeline, manually inspect these items:

- [ ] All 13 plots are generated and look reasonable
- [ ] Match outcomes plot shows sensible distribution (home wins > away wins)
- [ ] Goals distribution is centered around 2-3 goals
- [ ] Team rankings make sense (known strong teams at top)
- [ ] ROC curves show AUC > 0.6
- [ ] Model comparison shows metrics between 0.5-0.8
- [ ] Tournament simulation produces a winner

## Data Quality Checks

```r
# Run these checks after data cleaning
clean_data <- read.csv("data/processed/afcon_matches_clean.csv")

# 1. No missing values
stopifnot(sum(is.na(clean_data)) == 0)

# 2. Logical consistency
stopifnot(all(clean_data$home_goals >= 0))
stopifnot(all(clean_data$away_goals >= 0))
stopifnot(all(clean_data$total_goals == clean_data$home_goals + clean_data$away_goals))

# 3. Feature ranges
stopifnot(all(clean_data$home_win %in% c(0, 1)))
stopifnot(all(clean_data$is_knockout %in% c(0, 1)))

# 4. Date ranges
dates <- as.Date(clean_data$match_date)
stopifnot(all(dates >= as.Date("2000-01-01")))
stopifnot(all(dates <= as.Date("2023-12-31")))

cat("All data quality checks passed!\n")
```

## Model Validation

```r
# Load model comparison results
comparison <- read.csv("outputs/model_comparison.csv")

# Check reasonable performance
stopifnot(all(comparison$Accuracy > 0.5))  # Better than random
stopifnot(all(comparison$Accuracy < 1.0))  # Not suspiciously perfect
stopifnot(all(comparison$AUC > 0.5))       # Better than random

# Check predictions
preds <- read.csv("outputs/predictions/baseline_model_predictions.csv")
stopifnot(all(preds$logistic_prob >= 0 & preds$logistic_prob <= 1))
stopifnot(all(preds$rf_prob >= 0 & preds$rf_prob <= 1))

cat("All model validation checks passed!\n")
```

## Reporting Issues

If you encounter issues:

1. Check the error message carefully
2. Verify you ran scripts in order
3. Check the "Common Issues" section above
4. Ensure all dependencies are installed
5. Try running scripts individually to isolate the problem
6. Open a GitHub issue with:
   - Error message
   - Output from `sessionInfo()`
   - Steps to reproduce

## Success Criteria

The project is working correctly if:

✅ All scripts run without errors  
✅ All expected output files are created  
✅ Plots are generated and look reasonable  
✅ Models achieve AUC > 0.65  
✅ Tournament simulation completes and produces a winner  
✅ No warnings about missing packages  
✅ Data quality checks pass  

Congratulations! Your R-frica_Predicts project is working correctly! 🎉
