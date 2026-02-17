# Term Deposit Subscription Prediction (Logistic Regression) — ITAO 7100

This repository contains my coursework for **Statistics for Business (ITAO 7100)**.  
The project applies **logistic regression** to a bank term-deposit marketing dataset to predict whether a customer will subscribe (`yes/no`) based on customer characteristics and campaign interaction variables.

## Project Overview
Banks often run telemarketing campaigns for term deposits but cannot contact every customer.  
This work builds predictive models to support **targeted outreach** by estimating subscription likelihood.

### Key Predictors Explored
- Age
- Contact duration (call duration)
- Contact method
- Previous campaign outcome
- Month of contact
- Number of contacts

## Methods
- Data cleaning (missing values, trimming whitespace, datatype fixes)
- Exploratory analysis (bar charts, histograms, boxplots)
- Association testing (Chi-squared tests)
- Logistic regression modeling:
  - **Model 1 (baseline):** age + duration + contact method + previous outcome
  - **Model 2 (enhanced):** Model 1 + month + number_contacts
  - **Model 3 (deployable):** excludes contact_duration (unavailable pre-call)

## Model Evaluation
- Train/test split: 80/20 using stratified sampling (`caret::createDataPartition`)
- Confusion matrix + accuracy metrics (`caret::confusionMatrix`)
- Model fit comparison:
  - AIC
  - Pseudo R² (Hosmer–Lemeshow, Cox & Snell, Nagelkerke)
- Diagnostics:
  - VIF (multicollinearity)
  - Standardized residuals & Cook’s distance

## Practical Note (Why Model 3 Exists)
Contact duration improves predictive performance but is **not available before** calling customers.  
Model 3 is designed as a **realistic pre-call targeting model**.

## Repository Contents
- `scripts/` — R scripts for cleaning, EDA, tests, modeling, evaluation
- `reports/submitted/` — submitted report (Word/PDF)
- `reports/figures/` — exported charts used in the report
- `results/` — metrics and tables (optional)

## How to Run
1. Open RStudio
2. Place dataset in `data/raw/` (if permitted) or update the path in scripts
3. Run scripts in order:
   - `01_data_cleaning.R`
   - `02_eda_visuals.R`
   - `03_association_tests.R`
   - `04_models_eval.R`

## Author
**Fazal Ur Rehman Cheema**  
MSc Business Analytics

## Notes
This repository is shared for portfolio and learning purposes.
