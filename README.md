# R-frica_Predicts

R-based analysis of historical AFCON data to model match outcomes and predict the AFCON 2025 winner. Combining statistics and machine learning to explore trends, improve forecast accuracy, and bring data-driven insight to Africa's biggest football tournament.

## Overview

This project integrates two Kaggle datasets covering African national football matches (2010-2024) and FIFA world rankings to build predictive models for match outcomes. The pipeline includes data download, feature engineering, model training, and comprehensive evaluation.

## Features

- **Data Integration**: Combines match results with FIFA rankings
- **Feature Engineering**: Rolling averages, team form, days since last match, rank differences
- **Multiple Models**: Logistic regression, naive Bayes, random forest, and decision tree
- **Multiclass Classification**: Predicts home win, draw, or away win
- **Reproducible**: Set seeds and versioned model artifacts
- **Comprehensive Evaluation**: Confusion matrices, ROC curves, variable importance

## Datasets

This project uses two Kaggle datasets:

1. **African National Football (2010-2024)**  
   https://www.kaggle.com/datasets/oussamalariouch/african-national-football-from-2010-2024

2. **FIFA World Ranking**  
   https://www.kaggle.com/datasets/cashncarry/fifaworldranking/code

## Installation

### Prerequisites

- R (version 4.0 or higher)
- RStudio (optional, but recommended)

### Install Required Packages

Run the following command to install all required R packages:

```r
source("requirements.R")
```

This will install: tidyverse, lubridate, data.table, caret, nnet, klaR, e1071, naivebayes, ranger, randomForest, rpart, rpart.plot, pROC, cowplot, ggplot2, recipes, and zoo.

## Data Setup

You have two options for obtaining the datasets:

### Option 1: Kaggle API (Recommended)

1. **Install Kaggle CLI**:
   ```bash
   pip install kaggle
   ```

2. **Obtain Kaggle API credentials**:
   - Go to your Kaggle account settings: https://www.kaggle.com/settings
   - Click "Create New API Token" to download `kaggle.json`

3. **Place credentials**:
   - Option A: Copy `kaggle.json` to `credentials/` directory in this project
   - Option B: Copy `kaggle.json` to `~/.kaggle/` directory

4. **Run the pipeline** (it will automatically download the datasets):
   ```r
   source("run_pipeline.R")
   ```

### Option 2: Manual Download

If you prefer to download datasets manually or don't have Kaggle API access:

1. **Download datasets manually** from Kaggle:
   - African national football: https://www.kaggle.com/datasets/oussamalariouch/african-national-football-from-2010-2024
   - FIFA world ranking: https://www.kaggle.com/datasets/cashncarry/fifaworldranking/code

2. **Extract and place CSV files** in the `data/raw/` directory:
   ```
   data/
   └── raw/
       ├── african_football_matches.csv (or similar name)
       └── fifa_ranking.csv (or similar name)
   ```

3. **Run the pipeline** with skip-download flag:
   ```r
   source("run_pipeline.R")
   # Or from command line:
   Rscript run_pipeline.R --skip-download
   ```

## Usage

### Running the Complete Pipeline

The simplest way to run the entire pipeline:

```r
source("run_pipeline.R")
```

Or from the command line:

```bash
Rscript run_pipeline.R
```

To skip Kaggle download and use existing data:

```bash
Rscript run_pipeline.R --skip-download
```

### Running Individual Scripts

You can also run scripts individually:

```r
# Step 1: Download and prepare data
source("scripts/00_download_and_prepare_data.R")

# Step 2: Train models
source("scripts/01_train_models.R")

# Step 3: Evaluate models and generate plots
source("scripts/02_evaluate_and_plots.R")
```

## Pipeline Overview

### Step 1: Data Download and Preparation (`scripts/00_download_and_prepare_data.R`)

- Downloads datasets via Kaggle API (if configured)
- Loads match and FIFA ranking data
- Merges rankings with matches using date-based matching
- Engineers features:
  - FIFA rank difference (home - away)
  - Days since last match for each team
  - Rolling averages (last 5 matches): goals scored/conceded
  - Recent form: wins, draws, losses counts
  - Home advantage indicator
  - Match importance classification
- Creates target variable: `result` (home/draw/away)
- Saves processed data to `data/processed/matches_features.csv`

### Step 2: Model Training (`scripts/01_train_models.R`)

- Loads processed feature data
- Performs 80/20 stratified train/test split
- Sets up 5-fold cross-validation with 2 repeats
- Trains four models:
  1. **Logistic Regression** (multinomial via nnet)
  2. **Naive Bayes** (with tuning)
  3. **Random Forest** (via ranger, with hyperparameter tuning)
  4. **Decision Tree** (rpart, for interpretability)
- Saves trained models to `models/` directory
- Compares cross-validation performance

### Step 3: Model Evaluation (`scripts/02_evaluate_and_plots.R`)

- Loads test set and trained models
- Generates predictions and class probabilities
- Calculates performance metrics:
  - Overall accuracy
  - Per-class precision, recall, F1-score
  - Multi-class ROC AUC (one-vs-rest)
- Creates visualizations:
  - Confusion matrices for all models
  - Model comparison bar chart
  - ROC curves (one-vs-rest for each class)
  - Variable importance plot (Random Forest)
  - Decision tree diagram
- Saves results to `outputs/` and plots to `plots/`

## Output Structure

After running the pipeline, the following directories will be populated:

```
data/
├── raw/                    # Raw CSV files from Kaggle
└── processed/              # Processed feature data and test set
    ├── matches_features.csv
    └── test_data.rds

models/                     # Trained model objects
├── logistic_model.rds
├── naivebayes_model.rds
├── rf_model.rds
└── rpart_model.rds

outputs/                    # Performance metrics and results
├── summary_model_performance.csv
├── cv_accuracy_summary.csv
├── variable_importance_rf.csv
└── test_predictions.csv

plots/                      # Visualizations
├── confusion_matrix_*.png
├── model_comparison.png
├── roc_curves_*.png
├── variable_importance_rf.png
└── decision_tree.png
```

## Key Results

After running the pipeline, check these files:

- **`outputs/summary_model_performance.csv`**: Overall model accuracy and AUC scores
- **`plots/model_comparison.png`**: Visual comparison of model performance
- **`plots/confusion_matrix_*.png`**: Confusion matrices for each model
- **`plots/roc_curves_*.png`**: ROC curves showing class-wise performance
- **`plots/variable_importance_rf.png`**: Top features for prediction

## Model Details

### Target Variable

- **Type**: Multiclass classification
- **Classes**: 
  - `home`: Home team wins
  - `draw`: Match ends in a draw
  - `away`: Away team wins

### Features

Core features used for prediction:

- FIFA rank difference (home - away)
- Days since last match for each team
- Rolling average goals scored (last 5 matches)
- Rolling average goals conceded (last 5 matches)
- Recent form scores (wins × 3 + draws × 1)
- Form difference (home form - away form)
- Win/draw/loss counts in last 5 matches
- Home advantage indicator
- Match importance level

### Models

1. **Logistic Regression**: Multinomial logistic regression for interpretable probability estimates
2. **Naive Bayes**: Probabilistic classifier assuming feature independence
3. **Random Forest**: Ensemble of decision trees with feature importance
4. **Decision Tree**: Single interpretable tree for understanding decision rules

## Reproducibility

- All scripts use `set.seed(42)` for reproducibility
- Cross-validation uses fixed random seeds
- Train/test splits are stratified by target variable
- Model hyperparameters are saved with trained models

## Security Note

**Important**: Never commit Kaggle credentials to version control!

- The `.gitignore` file excludes `credentials/`, `kaggle.json`, and `.kaggle/`
- Always keep API keys and credentials private

## Troubleshooting

### "No match data CSV file found"

- Ensure CSV files are placed in `data/raw/`
- Check that files have `.csv` extension
- Verify file names match expected patterns (match, result, game, or african)

### "Kaggle CLI not found"

- Install Kaggle CLI: `pip install kaggle`
- Or use Option 2 (manual download)

### Package installation errors

- Update R to version 4.0 or higher
- Run `source("requirements.R")` again
- Check for specific package installation errors and install manually if needed

### Memory issues with large datasets

- Close other R sessions
- Increase R memory limit: `memory.limit(size = 16000)` (Windows)
- Consider using a subset of data for initial testing

## Contributing

Contributions are welcome! Please feel free to submit pull requests or open issues for bugs and feature requests.

## License

This project is open source and available for educational and research purposes.

## Acknowledgments

- Dataset authors on Kaggle:
  - Oussama Lariouch (African national football data)
  - Cash N Carry (FIFA world ranking data)
- R community for excellent machine learning packages (caret, ranger, etc.)
