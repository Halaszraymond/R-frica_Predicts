# R-frica Predicts – AFCON 2025 Simulation Project

**Authors:**  
- **Chadwa Khmissi (HQQ4WD)**  
- **Raymond Visser (B7INGB)**  

---

## 📘 Project Description

R-frica Predicts is an R-based pipeline developed to simulate the **AFCON 2025** football tournament.  
The project uses:

- Historical African international match results  
- FIFA monthly rankings  
- Engineered statistical features  
- Machine-learning models (Random Forest + Multinomial Logistic Regression)

The pipeline generates:

- Group stage standings  
- Knockout bracket prediction  
- Simulated tournament champion  
- A final combined visualization saved as a PNG  

All necessary data is already included in the `data/` directory.  
Web scraping is optional and **not required** to reproduce results.

---

## 📂 Project Structure
R-frica-Predicts/
├── data/ # Prepared datasets (already provided)
├── models/ # Saved machine-learning models
├── results/ # Simulation output (RDS file)
├── plots/ # Final tournament prediction plot
├── scripts/ # All scripts used in the pipeline (01–07)
└── 00_main.R # Master script that runs the entire pipeline


---

## 🚀 Running the Full Pipeline

To execute the complete workflow, open RStudio and run:

```r
source("00_main.R")

This will automatically perform:
    - Data loading
    - Feature engineering
    - Model training
    - Tournament simulation
    - Final visualization creation


The predicted AFCON 2025 tournament bracket will be saved in:
    - plots/afcon_2025_prediction.png


