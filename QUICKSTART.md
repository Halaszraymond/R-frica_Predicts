# Quick Start Guide

This quick start guide will help you get the African Football Match Prediction pipeline up and running in minutes.

## Prerequisites

- R version 4.0 or higher installed
- (Optional) Python with pip for Kaggle API

## Installation Steps

### 1. Install R Packages

Open R or RStudio and run:

```r
source("requirements.R")
```

This will automatically install all required packages. It may take 5-10 minutes depending on your system.

### 2. Set Up Data

Choose one of the following options:

#### Option A: Kaggle API (Automatic Download)

1. Install Kaggle CLI:
   ```bash
   pip install kaggle
   ```

2. Get your Kaggle API token:
   - Go to https://www.kaggle.com/settings
   - Scroll to "API" section
   - Click "Create New API Token"
   - This downloads `kaggle.json`

3. Place the credentials file:
   ```bash
   # Option 1: In project directory
   mkdir -p credentials
   cp ~/Downloads/kaggle.json credentials/
   
   # Option 2: In home directory
   mkdir -p ~/.kaggle
   cp ~/Downloads/kaggle.json ~/.kaggle/
   chmod 600 ~/.kaggle/kaggle.json
   ```

#### Option B: Manual Download

1. Download datasets from Kaggle:
   - African football: https://www.kaggle.com/datasets/oussamalariouch/african-national-football-from-2010-2024
   - FIFA ranking: https://www.kaggle.com/datasets/cashncarry/fifaworldranking/code

2. Extract CSV files and place them in `data/raw/` directory:
   ```bash
   mkdir -p data/raw
   # Copy your downloaded CSV files here
   ```

## Running the Pipeline

### Option 1: Full Pipeline (Recommended)

Run all three steps automatically:

```bash
Rscript run_pipeline.R
```

If using manual data download:

```bash
Rscript run_pipeline.R --skip-download
```

### Option 2: Step-by-Step

Run each script individually:

```r
# Step 1: Download and prepare data
source("scripts/00_download_and_prepare_data.R")

# Step 2: Train models
source("scripts/01_train_models.R")

# Step 3: Evaluate and create plots
source("scripts/02_evaluate_and_plots.R")
```

## Expected Runtime

- Step 1 (Data preparation): 2-5 minutes
- Step 2 (Model training): 5-15 minutes (depends on dataset size)
- Step 3 (Evaluation): 1-3 minutes

**Total: 8-23 minutes**

## Checking Results

After the pipeline completes, check these key files:

### 1. Model Performance Summary
```r
results <- read.csv("outputs/summary_model_performance.csv")
print(results)
```

### 2. View Best Model's Confusion Matrix
Open: `plots/confusion_matrix_randomforest.png` (or the best performing model)

### 3. Check Feature Importance
Open: `plots/variable_importance_rf.png`

### 4. Compare All Models
Open: `plots/model_comparison.png`

## Making Predictions with Trained Models

```r
# Load a trained model
model <- readRDS("models/rf_model.rds")

# Load the processed data to see feature names
features <- read.csv("data/processed/matches_features.csv")
colnames(features)

# Create new data (example)
new_match <- data.frame(
  team_rank_home = 15,
  team_rank_away = 25,
  rank_difference = -10,
  days_since_home = 14,
  days_since_away = 10,
  home_goals_avg = 1.8,
  home_conceded_avg = 1.2,
  away_goals_avg = 1.4,
  away_conceded_avg = 1.6,
  home_wins_last5 = 3,
  home_draws_last5 = 1,
  home_losses_last5 = 1,
  away_wins_last5 = 2,
  away_draws_last5 = 2,
  away_losses_last5 = 1,
  home_form = 10,
  away_form = 8,
  form_difference = 2,
  home_advantage = 1,
  match_importance = 2
)

# Make prediction
prediction <- predict(model, newdata = new_match)
probabilities <- predict(model, newdata = new_match, type = "prob")

cat("Predicted outcome:", as.character(prediction), "\n")
print(probabilities)
```

## Troubleshooting

### Problem: Package installation fails

**Solution**: Update R to version 4.0+ and try installing packages individually:
```r
install.packages("tidyverse")
install.packages("caret")
# etc.
```

### Problem: "No match data CSV file found"

**Solution**: 
- Check that CSV files are in `data/raw/` directory
- Files must have `.csv` extension
- Run with `--skip-download` if using manual data

### Problem: Kaggle CLI not found

**Solution**:
- Install: `pip install kaggle`
- Or use manual download method (Option B above)

### Problem: Memory errors during training

**Solution**:
```r
# Increase memory limit (Windows)
memory.limit(size = 16000)

# Or reduce dataset size for testing
data <- data[1:1000, ]  # Use first 1000 rows
```

### Problem: Models taking too long to train

**Solution**: Reduce cross-validation folds or tuning parameters in `scripts/01_train_models.R`:
```r
train_control <- trainControl(
  method = "cv",
  number = 3,  # Reduced from 5
  repeats = 1,  # Reduced from 2
  # ...
)
```

## Next Steps

1. **Analyze Results**: Review model performance metrics and plots
2. **Interpret Features**: Check variable importance to understand key predictors
3. **Make Predictions**: Use trained models for new match predictions
4. **Fine-tune**: Adjust hyperparameters in training script for better performance
5. **Deploy**: Export best model for production use

## Getting Help

- Check `README.md` for detailed documentation
- Review `OUTPUTS.md` for output file descriptions
- Open an issue on GitHub for bugs or questions

## Tips for Best Results

1. **Use complete data**: More historical matches = better predictions
2. **Keep rankings updated**: Fresh FIFA rankings improve accuracy
3. **Feature engineering**: Experiment with new features in the data preparation script
4. **Model ensemble**: Combine predictions from multiple models for better accuracy
5. **Regular retraining**: Update models periodically with new match results

## Summary of Output Locations

```
data/processed/     - Cleaned and feature-engineered data
models/             - Trained model objects (.rds files)
outputs/            - Performance metrics (CSV files)
plots/              - Visualizations (PNG files)
```

That's it! You're ready to predict African football match outcomes. Good luck! ⚽
