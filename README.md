# Predicting Credit Card Default: State-Based vs Temporal Predictors

ICT515 Foundations of Data Science, Assignment 2 (Murdoch University).

This project uses the UCI [Default of Credit Card Clients](https://archive.ics.uci.edu/dataset/350/default+of+credit+card+clients) dataset (30,000 Taiwanese credit card clients, 2005) to answer one question:

> Are **state-based** predictors (static/demographic details) or **temporal** predictors (monthly repayment history) more accurate at predicting whether a client defaults on their next payment?

The work covers exploratory data analysis, visual feature selection, and a model comparison across three feature sets. The same three algorithms are used for every set.

## Results

Each feature set was modelled with logistic regression, a decision tree and a random forest. The best model for each set was chosen by 5-fold cross-validated ROC AUC; random forest came out on top for all three sets. Scores below are from the held-out 20% test set.

| Feature set | Variables | AUC | Balanced accuracy | Brier score |
|---|---|---|---|---|
| State-based | 5 | 0.621 | 0.592 | 0.167 |
| Temporal | 12 | 0.759 | 0.707 | 0.138 |
| Combined (baseline) | 17 | 0.773 | 0.709 | 0.137 |

- **Temporal predictors are far more accurate** than state-based ones (+0.14 AUC).
- **Adding state-based variables to the temporal ones adds very little** (+0.014 AUC).
- **State-based predictors alone are weak.** Their Brier score is only just below a no-skill model that gives every client the average default rate (about 0.17).

Lower Brier scores are better. The full results for all 9 models are in `test_results_all_feature_sets.csv`.

## Feature sets

The variables were renamed from `X1`–`X23` to readable names. Month suffixes run from `SEP` (September 2005, the most recent month) back to `APR` (April 2005).

| Set | Variables |
|---|---|
| State-based | `LIMIT_BAL`, `SEX`, `EDUCATION`, `MARRIAGE`, `AGE` |
| Temporal | `PAY_*` (repayment status, 6 months), `PAYAMT_*` (amount paid, 6 months) |
| Combined | State-based + temporal |

The bill amounts (`BILL_*`) were dropped during feature selection, because their default rate stays flat at about 20–26% with no trend. Section 8 of the script tests this decision. Re-adding them improved cross-validated ROC by only about 0.003, so the simpler model was kept.

## Method

1. **Data preparation.** The description row and ID column are removed and columns are converted to proper types. Undocumented codes (`EDUCATION` 0/5/6 and `MARRIAGE` 0) are merged into "Other".
2. **EDA.** An automated report is produced with SmartEDA (`smarteda_report.html`).
3. **Feature selection.** Every predictor is plotted against the default rate:
   - bar charts for categorical variables
   - boxplots plus decile default-rate plots for continuous variables

   A strength table (`feature_strength_table.csv`) backs up the plots.
4. **Modelling.**
   - **Split:** one stratified 80/20 train/test split and one set of 5 cross-validation folds (seed 515), shared by every model so the comparison is fair.
   - **Tuning:** hyper-parameters are tuned with `caret`.
   - **Random forest:** uses `ranger` as a probability forest.
5. **Evaluation.**
   - **Model selection:** by ROC AUC, because about 78% of clients don't default and accuracy alone is misleading.
   - **Test-set measures:** accuracy, sensitivity, specificity and balanced accuracy are reported at a cut-off equal to the training default rate (0.22). The Brier score measures how accurate the predicted probabilities are.
   - **Significance:** differences between algorithms are tested with paired comparisons of the fold-level ROC.

## Project files

| File | Description |
|---|---|
| `515.R` | The full analysis, from loading the data to the final evaluation |
| `default of credit card clients.xls` | Source dataset |
| `smarteda_report.html` | EDA report |
| `x6.png`–`x9.png` | Extra categorical plots added to the EDA report |
| `feature_plots/` | One plot per predictor against the default rate |
| `feature_strength_table.csv` | Default-rate spread and chi-square p-value for each predictor |
| `test_results_all_feature_sets.csv` | Test-set results for all 9 models |
| `final_comparison_selected_models.csv` | Test-set results for the selected model of each feature set |
| `roc_all_models.png`, `roc_selected_models.png` | Test-set ROC curves |
| `calibration_selected_models.png` | Predicted vs observed default rate for the selected models |

**Why `all_models.rds` is not in the repository:** the script saves the 9 trained models to `all_models.rds`, which is about 180 MB. That is over GitHub's 100 MB file limit, so `*.rds` files are listed in `.gitignore`. Running `515.R` recreates the file.

## How to run

1. Open `ict515.Rproj` in RStudio. This sets the working directory to the project folder, where the dataset must be.
2. Run `515.R` (**Source**, or `Rscript 515.R`). Any missing packages are installed automatically:
   `readxl`, `SmartEDA`, `dplyr`, `ggplot2`, `gridExtra`, `caret`, `e1071`, `rpart`, `ranger`, `pROC`.
3. Training the 9 models takes several minutes. All tables and plots are written to the project folder.

## Reference

Yeh, I.-C., & Lien, C. (2009). The comparisons of data mining techniques for the predictive accuracy of probability of default of credit card clients. *Expert Systems with Applications, 36*(2), 2473–2480.
