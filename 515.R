# =============================================================================
# ICT515 Assignment 2 - Default of Credit Card Clients
# Feature selection (visual) + model selection for three feature sets
#
#   1. Load & clean the data
#   2. Prepare the data for analysis (readable names, proper factor types)
#   3. Feature selection: plot every predictor against the outcome (DEFAULT)
#   4. Build the three feature sets:
#        - STATE-BASED : static/demographic variables
#        - TEMPORAL    : repayment-history variables
#        - COMBINED    : state + temporal (baseline)
#   5. One shared train/test split and one shared set of CV folds
#   6. Model selection: the SAME 3 algorithms on every feature set
#        - Logistic Regression, Decision Tree, Random Forest
#   7. Final evaluation on the held-out test set + comparison of feature sets
#   8. Check of the feature-selection decision (re-adding bill amounts)
# =============================================================================


# -----------------------------------------------------------------------------
# 0. PACKAGES
# -----------------------------------------------------------------------------
# Install only the packages that are missing, so re-running the script does
# not reinstall everything each time.
packages <- c("readxl", "SmartEDA", "dplyr", "ggplot2", "gridExtra",
              "caret", "e1071", "rpart", "ranger", "pROC")
missing_pkgs <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(readxl)
library(SmartEDA)
library(dplyr)
library(ggplot2)      # plotting
library(gridExtra)    # putting two ggplots side by side
library(caret)        # one consistent interface for training/tuning models
library(rpart)        # decision tree engine used by caret
library(ranger)       # random forest engine used by caret (probability forest)
library(pROC)         # ROC curves and AUC


# -----------------------------------------------------------------------------
# 1. Loaded and cleaned the data
# -----------------------------------------------------------------------------
# The .xls file must be in the working directory (Session > Set Working
# Directory > To Source File Location), so the script runs on any computer.
#loaded dataset into df
raw_data <- read_excel("default of credit card clients.xls")

#dropped the first data row(column description) and first column(id)
clean_data <- raw_data[-1, -1]

#checked the data types for the columns.
#all of them were categorical so conversion had to be done
table(sapply(clean_data, class))

#converted the numerical columns into their proper datatype
credit_data <- clean_data %>%
  mutate(across(
    c("X1", "X5", X12:X23),
    ~ as.numeric(as.character(.))
  ))

#created the report. Some categorical variables...
#...with over 10 distinct values did not show.
ExpReport(data = credit_data, Target = NULL, op_file = "smarteda_report.html")

#generated the visualizations for the extra categorical columns
#and added them manually to the html report
ExpCatViz(data = credit_data, target = "Y", clim = 15)


# -----------------------------------------------------------------------------
# 2. PREPARE THE DATA FOR FEATURE SELECTION AND MODELLING
# -----------------------------------------------------------------------------
# The X1..X23 names are hard to read in plots and in the report, so we rename
# them using the dataset documentation. Months run from SEP (Sept 2005, the
# most recent month) back to APR (April 2005).
months <- c("SEP", "AUG", "JUL", "JUN", "MAY", "APR")

model_data <- credit_data %>%
  rename(
    LIMIT_BAL = X1,   # credit limit (NT dollars)
    SEX       = X2,   # 1 = male, 2 = female
    EDUCATION = X3,   # 1 = grad school, 2 = university, 3 = high school, 4 = other
    MARRIAGE  = X4,   # 1 = married, 2 = single, 3 = other
    AGE       = X5    # age in years
  ) %>%
  # X6-X11  = repayment status per month (-1 = paid on time, 1..9 = months late)
  # X12-X17 = bill statement amount per month
  # X18-X23 = amount actually paid per month
  rename_with(~ paste0("PAY_",    months), X6:X11)  %>%
  rename_with(~ paste0("BILL_",   months), X12:X17) %>%
  rename_with(~ paste0("PAYAMT_", months), X18:X23) %>%
  mutate(
    # Repayment status is an ORDERED scale (more months late = worse), so we
    # store it as a number. This keeps the order information and avoids
    # creating ~10 dummy variables per month for the models.
    across(starts_with("PAY_"), ~ as.numeric(as.character(.))),
    
    # EDUCATION contains codes 0, 5 and 6 that are NOT in the documentation
    # (only 345 rows). We merge them into "Other" (code 4) instead of dropping.
    EDUCATION = as.numeric(as.character(EDUCATION)),
    EDUCATION = ifelse(EDUCATION %in% c(0, 5, 6), 4, EDUCATION),
    EDUCATION = factor(EDUCATION, levels = 1:4,
                       labels = c("GradSchool", "University", "HighSchool", "Other")),
    
    # MARRIAGE has an undocumented code 0 (54 rows) -> merge into "Other" (3).
    MARRIAGE = as.numeric(as.character(MARRIAGE)),
    MARRIAGE = ifelse(MARRIAGE == 0, 3, MARRIAGE),
    MARRIAGE = factor(MARRIAGE, levels = 1:3,
                      labels = c("Married", "Single", "Other")),
    
    SEX = factor(as.numeric(as.character(SEX)), levels = 1:2,
                 labels = c("Male", "Female")),
    
    # The outcome. caret needs factor levels that are valid R names, so we use
    # "No"/"Yes" rather than 0/1. "Yes" = the client defaulted next month.
    DEFAULT = factor(Y, levels = c("0", "1"), labels = c("No", "Yes"))
  ) %>%
  select(-Y) %>%
  as.data.frame()   # plain data.frame: caret and row-indexing behave predictably

str(model_data)

# No missing values in any column (confirmed by the EDA report as well)
sum(is.na(model_data))

# Overall default rate - this is the "reference line" for every plot below.
# About 22% of clients default, so the classes are imbalanced.
overall_rate <- mean(model_data$DEFAULT == "Yes")
overall_rate








# -----------------------------------------------------------------------------
# 3. FEATURE SELECTION: PLOT EACH PREDICTOR AGAINST THE OUTCOME
# -----------------------------------------------------------------------------
# Idea: a predictor is useful if the default rate CHANGES across its values.
# If the default rate stays flat at ~22% (the overall default rate of the
# dataset) no matter what the predictor's value is, the predictor tells the
# model nothing and can be dropped.
#
#  - Categorical / few-valued predictors -> bar chart of default rate per level
#  - Continuous predictors -> (a) boxplot by DEFAULT and
#                             (b) default rate across 10 equal-sized bins
#    The binned plot matters because a boxplot only compares the middle of the
#    two groups; it can miss a U-shaped relationship (which AGE turns out to
#    have).

dir.create("feature_plots", showWarnings = FALSE)  # all plots saved here

# --- 3a. Plot function for categorical predictors ----------------------------
plot_categorical <- function(df, var) {
  # default rate and group size for each level of the predictor
  rates <- df %>%
    group_by(level = factor(.data[[var]])) %>%
    summarise(n = n(), default_rate = mean(DEFAULT == "Yes"), .groups = "drop")
  
  p <- ggplot(rates, aes(x = level, y = default_rate)) +
    geom_col(fill = "steelblue") +
    # red dashed line = overall default rate; bars far from it = informative
    geom_hline(yintercept = overall_rate, linetype = "dashed", colour = "red") +
    geom_text(aes(label = paste0(round(default_rate * 100, 1), "%\nn=", n)),
              vjust = -0.2, size = 3) +
    scale_y_continuous(labels = scales::percent,
                       expand = expansion(mult = c(0, 0.25))) +
    labs(title = paste("Default rate by", var),
         subtitle = "Red dashed line = overall default rate",
         x = var, y = "Default rate") +
    theme_minimal()
  
  print(p)
  ggsave(file.path("feature_plots", paste0(var, ".png")), p, width = 7, height = 4)
}

# --- 3b. Plot function for continuous predictors -----------------------------
plot_numeric <- function(df, var) {
  # (a) Boxplot: does the distribution differ between defaulters and others?
  # Money variables are very skewed, so we zoom to the 1st-99th percentile
  # (coord_cartesian zooms WITHOUT deleting data from the box calculation).
  limits <- quantile(df[[var]], c(0.01, 0.99))
  p1 <- ggplot(df, aes(x = DEFAULT, y = .data[[var]], fill = DEFAULT)) +
    geom_boxplot(outlier.shape = NA) +
    coord_cartesian(ylim = limits) +
    scale_fill_manual(values = c(No = "grey70", Yes = "tomato")) +
    labs(title = paste(var, "by DEFAULT"), x = "Defaulted?", y = var) +
    theme_minimal() + theme(legend.position = "none")
  
  # (b) Default rate across 10 equal-sized bins (deciles) of the predictor.
  # A sloped or curved line = useful predictor; a flat line = not useful.
  binned <- df %>%
    mutate(bin = ntile(.data[[var]], 10)) %>%
    group_by(bin) %>%
    summarise(bin_median = median(.data[[var]]),
              default_rate = mean(DEFAULT == "Yes"), .groups = "drop")
  p2 <- ggplot(binned, aes(x = bin, y = default_rate)) +
    geom_line(colour = "steelblue") + geom_point(colour = "steelblue") +
    geom_hline(yintercept = overall_rate, linetype = "dashed", colour = "red") +
    scale_x_continuous(breaks = 1:10) +
    scale_y_continuous(labels = scales::percent) +
    labs(title = paste("Default rate by", var, "decile"),
         x = paste(var, "decile (1 = lowest, 10 = highest)"),
         y = "Default rate") +
    theme_minimal()
  
  # show both panels side by side and save them as one image
  g <- grid.arrange(p1, p2, ncol = 2)
  ggsave(file.path("feature_plots", paste0(var, ".png")), g, width = 11, height = 4)
}

# --- 3c. Simple number to back up what we see in each plot -------------------
# "spread" = highest group default rate minus lowest group default rate.
# Bigger spread = stronger relationship with DEFAULT. Groups with fewer than
# 100 clients are ignored so a handful of people cannot inflate the spread.
# The chi-square p-value is also reported, but with 30,000 rows almost
# everything is "significant", so we rely mainly on the plot + spread.
feature_strength <- function(df, var) {
  x <- df[[var]]
  groups <- if (is.numeric(x) && n_distinct(x) > 15) ntile(x, 10) else x
  counts <- table(groups)
  rates  <- tapply(df$DEFAULT == "Yes", groups, mean)
  rates  <- rates[names(counts)[counts >= 100]]
  p_val  <- suppressWarnings(chisq.test(table(groups, df$DEFAULT))$p.value)
  data.frame(variable = var,
             lowest_rate  = round(min(rates), 3),
             highest_rate = round(max(rates), 3),
             spread       = round(max(rates) - min(rates), 3),
             chisq_p      = signif(p_val, 3))
}

# --- 3d. Group the predictors --------------------------------------------------
state_vars       <- c("LIMIT_BAL", "SEX", "EDUCATION", "MARRIAGE", "AGE")
pay_status_vars  <- paste0("PAY_",    months)  # temporal: repayment status
bill_vars        <- paste0("BILL_",   months)  # temporal: bill amounts
pay_amount_vars  <- paste0("PAYAMT_", months)  # temporal: amounts paid
temporal_vars    <- c(pay_status_vars, bill_vars, pay_amount_vars)

categorical_vars <- c("SEX", "EDUCATION", "MARRIAGE", pay_status_vars)
numeric_vars     <- c("LIMIT_BAL", "AGE", bill_vars, pay_amount_vars)

# --- 3e. Draw every plot ---------------------------------------------------------
# State-based predictors
for (v in intersect(state_vars, categorical_vars)) plot_categorical(model_data, v)
for (v in intersect(state_vars, numeric_vars))     plot_numeric(model_data, v)

# Temporal predictors
for (v in pay_status_vars) plot_categorical(model_data, v)
for (v in c(bill_vars, pay_amount_vars)) plot_numeric(model_data, v)

# --- 3f. Summary table of relationship strength --------------------------------
strength_table <- bind_rows(lapply(c(state_vars, temporal_vars),
                                   function(v) feature_strength(model_data, v))) %>%
  arrange(desc(spread))
print(strength_table)
write.csv(strength_table, "feature_strength_table.csv", row.names = FALSE)











# -----------------------------------------------------------------------------
# 4. DECIDE THE FEATURE SETS (based on the plots above)
# -----------------------------------------------------------------------------
#  STATE-BASED
#   - LIMIT_BAL : clear downward trend - default ~32% for the lowest limits,
#                 ~14% for the highest. Strong -> KEEP
#   - EDUCATION : HighSchool/University default more than GradSchool;
#                 "Other" is much lower. -> KEEP
#   - MARRIAGE  : small but consistent differences between groups -> KEEP
#   - AGE       : boxplots look almost identical (same median), BUT the decile
#                 plot is U-shaped: young (<25) and older (>45) clients default
#                 more. -> KEEP (tree/forest models can use a U-shape)
#   - SEX       : males default slightly more (~24% vs ~21%). Weak but
#                 consistent over large groups -> KEEP (borderline)
#   State-based variables are the subject of the research question, so all 5
#   are kept.
#
#  TEMPORAL
#   - PAY_*     : default rate jumps from ~15% (paid on time) to 50-75% for
#                 clients 2+ months late. By far the strongest -> KEEP all 6
#   - PAYAMT_*  : clients who pay very little default more (~30-35%) than
#                 those who pay a lot (~13%). Clear trend -> KEEP all 6
#   - BILL_*    : default rate zig-zags between ~20% and ~26% across deciles
#                 with no trend. Not informative on their own -> DROP all 6
#                 (this decision is tested in section 8)
state_features    <- state_vars
temporal_features <- c(pay_status_vars, pay_amount_vars)
combined_features <- c(state_features, temporal_features)   # baseline set

# All three sets in one named list, so every later step loops over them in
# exactly the same way.
feature_sets <- list(
  "State-based"         = state_features,
  "Temporal"            = temporal_features,
  "Combined (baseline)" = combined_features
)









# -----------------------------------------------------------------------------
# 5. TRAIN/TEST SPLIT AND CROSS-VALIDATION SET-UP (shared by all feature sets)
# -----------------------------------------------------------------------------
# For the comparison "state vs temporal vs combined" to be fair, ALL models
# must be trained and tested on exactly the same rows. So we create the split
# and the CV folds once, with a fixed seed.
set.seed(515)
train_index <- createDataPartition(model_data$DEFAULT, p = 0.8, list = FALSE)[, 1]
train_data  <- model_data[train_index, ]
test_data   <- model_data[-train_index, ]   # never touched until section 7

# createDataPartition is stratified: both sets keep the ~22% default rate
prop.table(table(train_data$DEFAULT))
prop.table(table(test_data$DEFAULT))

# 5-fold cross-validation folds on the TRAINING data
set.seed(515)
cv_folds <- createFolds(train_data$DEFAULT, k = 5, returnTrain = TRUE)

# How caret should evaluate each candidate model during training:
#  - index = cv_folds      : use our fixed folds (same for every model)
#  - classProbs = TRUE     : keep predicted probabilities (needed for ROC)
#  - twoClassSummary       : report ROC AUC, sensitivity, specificity
# We pick models by ROC AUC instead of accuracy, because with 78% "No" a model
# that ALWAYS predicts "No" already gets 78% accuracy while being useless.
ctrl <- trainControl(index           = cv_folds,
                     classProbs      = TRUE,
                     summaryFunction = twoClassSummary,
                     savePredictions = "final")







# -----------------------------------------------------------------------------
# 6. MODEL SELECTION: SAME 3 ALGORITHMS ON EACH FEATURE SET
# -----------------------------------------------------------------------------
# Algorithms (logistic regression and classification trees were used in
# Yeh & Lien, 2009; random forest is a standard extension of trees):
#  - Logistic regression : simple, interpretable linear baseline
#  - Decision tree       : captures non-linear effects (e.g. AGE's U-shape)
#                          and thresholds; easy to explain
#  - Random forest       : many trees averaged; usually the most accurate,
#                          less interpretable
# caret tunes each model's settings (hyper-parameters) using the CV folds and
# keeps the setting with the best mean ROC.

# Random forest settings tried (6 combinations, chosen by CV ROC):
#  - mtry          : predictors tried at each split (2, 4 or 6)
#  - min.node.size : minimum clients per leaf (25 or 100); larger leaves
#                    give smoother, less overfitted probabilities
#  - splitrule     : gini (standard classification split)
rf_grid <- expand.grid(mtry = c(2, 4, 6),
                       splitrule = "gini",
                       min.node.size = c(25, 100))

fit_three_models <- function(features, data) {
  # builds e.g. DEFAULT ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE
  model_formula <- as.formula(paste("DEFAULT ~", paste(features, collapse = " + ")))
  
  set.seed(515)
  logreg <- train(model_formula, data = data, method = "glm", family = "binomial",
                  metric = "ROC", trControl = ctrl)
  
  # cp (complexity parameter) controls how far the tree grows; caret tries 10
  # values and keeps the best one
  set.seed(515)
  tree <- train(model_formula, data = data, method = "rpart",
                metric = "ROC", trControl = ctrl, tuneLength = 10)
  
  # Random forest using the ranger package as a PROBABILITY forest.
  # Why not randomForest: randomForest makes each tree cast ONE vote for the
  # majority class in its leaf. Many clients share identical state-based
  # values, so leaves stay mixed and their majority is almost always "No".
  # The forest then gave nearly everyone P(default) close to 0. A probability
  # forest instead averages the actual default RATE in each leaf, so a leaf
  # with 30% defaulters contributes 0.30 rather than a "No" vote.
  # caret switches ranger into probability mode because classProbs = TRUE.
  # importance = "impurity" is needed so varImp() works later.
  set.seed(515)
  forest <- train(model_formula, data = data, method = "ranger",
                  metric = "ROC", trControl = ctrl, tuneGrid = rf_grid,
                  num.trees = 300, importance = "impurity")
  
  list(LogisticRegression = logreg, DecisionTree = tree, RandomForest = forest)
}

# Train the 3 algorithms on each of the 3 feature sets (9 models in total).
# This takes several minutes.
all_models <- lapply(feature_sets, fit_three_models, data = train_data)

# --- 6a. Compare the 3 algorithms within each feature set (CV results) -------
# resamples() lines up the 5 fold-level scores of each model so they can be
# compared fairly (same folds for every model).
all_cv <- lapply(all_models, resamples)

# NOTE: in these CV summaries caret treats the FIRST factor level ("No") as
# the positive class and uses a 0.5 cut-off, so "Sens" = non-defaulters
# caught and "Spec" = defaulters caught. ROC is unaffected. Section 7
# reports them the right way round (positive = "Yes").
for (set_name in names(all_cv)) {
  cat("\n==================", set_name, "==================\n")
  print(summary(all_cv[[set_name]], metric = "ROC"))
  
  # Paired tests on the fold-level ROC (Bonferroni-adjusted).
  # Upper half = difference (row minus column), lower half = p-value.
  # Caveat: only 5 folds that share most of their training data, so the
  # p-values are somewhat optimistic.
  print(summary(diff(all_cv[[set_name]], metric = "ROC")))
  
  # Box plots of fold-level ROC: the model whose box sits highest (and is
  # narrow) is the best and most stable choice. (print() is needed inside a loop)
  print(bwplot(all_cv[[set_name]], metric = "ROC",
               main = paste(set_name, "models: CV ROC AUC")))
}

# Pick the best algorithm for each feature set by mean CV ROC
best_name <- function(cv) {
  mean_roc <- colMeans(cv$values[, grep("~ROC$", names(cv$values))])
  sub("~ROC$", "", names(which.max(mean_roc)))
}
best_models <- sapply(all_cv, best_name)
best_models

# Which predictors did the best model of each set rely on most?
for (set_name in names(best_models)) {
  imp <- varImp(all_models[[set_name]][[best_models[[set_name]]]])
  # top = number of predictors to show (capped at how many the model has)
  print(plot(imp, top = min(15, nrow(imp$importance)),
             main = paste("Variable importance -", best_models[[set_name]],
                          "(", set_name, ")")))
}










# -----------------------------------------------------------------------------
# 7. FINAL EVALUATION ON THE UNSEEN TEST SET
# -----------------------------------------------------------------------------
# Threshold: by default a model says "Yes" when P(default) >= 0.5. Because
# only 22% of clients default, the state-based models rarely reach 0.5 and
# would predict "No" for almost everyone. We therefore classify as "Yes" when
# the probability is above the training default rate (~0.22) - i.e. "riskier
# than the average client". AUC does not depend on the threshold at all.
threshold <- mean(train_data$DEFAULT == "Yes")

evaluate_on_test <- function(model, data, threshold) {
  prob <- predict(model, newdata = data, type = "prob")[, "Yes"]
  pred <- factor(ifelse(prob >= threshold, "Yes", "No"), levels = c("No", "Yes"))
  cm   <- confusionMatrix(pred, data$DEFAULT, positive = "Yes")
  roc_obj <- roc(data$DEFAULT, prob, levels = c("No", "Yes"),
                 direction = "<", quiet = TRUE)
  # Brier score = mean squared error between the predicted probability and
  # the actual outcome (1 = defaulted, 0 = not). It measures how accurate the
  # probabilities themselves are (calibration). Lower = better.
  # This is the classification counterpart of MSE/RMSE.
  brier <- mean((prob - (data$DEFAULT == "Yes"))^2)
  data.frame(AUC              = round(as.numeric(auc(roc_obj)), 3),
             Accuracy         = round(cm$overall[["Accuracy"]], 3),
             Sensitivity      = round(cm$byClass[["Sensitivity"]], 3),  # defaulters caught
             Specificity      = round(cm$byClass[["Specificity"]], 3),  # non-defaulters correctly cleared
             BalancedAccuracy = round(cm$byClass[["Balanced Accuracy"]], 3),
             Brier            = round(brier, 4))
}

# Evaluate ALL nine models (3 algorithms x 3 feature sets) on the test set
results <- bind_rows(lapply(names(all_models), function(set_name) {
  bind_rows(lapply(names(all_models[[set_name]]), function(m)
    cbind(FeatureSet = set_name, Model = m,
          evaluate_on_test(all_models[[set_name]][[m]], test_data, threshold))))
}))
print(results)
write.csv(results, "test_results_all_feature_sets.csv", row.names = FALSE)

# Reference Brier score of a "no-skill" model that gives every client the
# average default rate. A useful model must score BELOW this (~0.17).
no_skill_brier <- mean((threshold - (test_data$DEFAULT == "Yes"))^2)
no_skill_brier

# Headline comparison: the CV-selected model of each feature set
final_comparison <- results %>%
  filter(paste(FeatureSet, Model) %in% paste(names(best_models), best_models))
print(final_comparison)
write.csv(final_comparison, "final_comparison_selected_models.csv", row.names = FALSE)

# --- 7a. ROC curves on the test set -------------------------------------------
# The further a curve bulges to the top-left, the better.
# The diagonal = random guessing (AUC 0.5).
get_test_prob <- function(model) predict(model, test_data, type = "prob")[, "Yes"]

roc_list <- list()
for (set_name in names(all_models)) {
  for (m in names(all_models[[set_name]])) {
    roc_list[[paste(set_name, "-", m)]] <-
      roc(test_data$DEFAULT, get_test_prob(all_models[[set_name]][[m]]),
          levels = c("No", "Yes"), direction = "<", quiet = TRUE)
  }
}

# All 9 models
roc_plot_all <- ggroc(roc_list, legacy.axes = TRUE) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  labs(title = "Test-set ROC curves: all models and feature sets",
       x = "False positive rate (1 - specificity)",
       y = "True positive rate (sensitivity)", colour = "Model") +
  theme_minimal()
print(roc_plot_all)
ggsave("roc_all_models.png", roc_plot_all, width = 9, height = 6)

# Only the selected model of each feature set (cleaner for the report)
selected_keys <- paste(names(best_models), "-", best_models)
roc_plot_best <- ggroc(roc_list[selected_keys], legacy.axes = TRUE, linewidth = 1) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  labs(title = "Test-set ROC: selected model per feature set",
       x = "False positive rate (1 - specificity)",
       y = "True positive rate (sensitivity)", colour = "Feature set - model") +
  theme_minimal()
print(roc_plot_best)
ggsave("roc_selected_models.png", roc_plot_best, width = 8, height = 5)

# --- 7b. Calibration plot of the selected models ------------------------------
# Are the predicted probabilities realistic? Test clients are sorted into 10
# groups by predicted probability; for each group we plot the average
# predicted probability against the ACTUAL default rate. Points on the dashed
# diagonal = well-calibrated probabilities (similar in spirit to Yeh & Lien's
# predicted-vs-real probability comparison).
calibration_data <- bind_rows(lapply(names(best_models), function(set_name) {
  prob <- get_test_prob(all_models[[set_name]][[best_models[[set_name]]]])
  data.frame(prob = prob, actual = test_data$DEFAULT == "Yes") %>%
    mutate(bin = ntile(prob, 10)) %>%
    group_by(bin) %>%
    summarise(mean_predicted = mean(prob),
              observed_rate  = mean(actual), .groups = "drop") %>%
    mutate(FeatureSet = paste(set_name, "-", best_models[[set_name]]))
}))
calibration_plot <- ggplot(calibration_data,
                           aes(x = mean_predicted, y = observed_rate, colour = FeatureSet)) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  geom_line() + geom_point() +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  labs(title = "Calibration of the selected models (test set)",
       x = "Mean predicted probability of default",
       y = "Observed default rate", colour = "Feature set - model") +
  theme_minimal()
print(calibration_plot)
ggsave("calibration_selected_models.png", calibration_plot, width = 7, height = 6)

# Save the trained models so they don't have to be retrained for the report
saveRDS(all_models, "all_models.rds")








# -----------------------------------------------------------------------------
# 8. CHECK OF THE FEATURE-SELECTION DECISION: RE-ADD THE BILL AMOUNTS
# -----------------------------------------------------------------------------
# The plots suggested BILL_* carry no information on their own. Here we test
# that decision: the combined random forest is retrained WITH the bill
# amounts (same settings, folds and seed) and compared with the model without.
bill_formula <- as.formula(paste("DEFAULT ~",
                                 paste(c(combined_features, bill_vars), collapse = " + ")))
set.seed(515)
rf_with_bills <- train(bill_formula, data = train_data, method = "ranger",
                       metric = "ROC", trControl = ctrl, tuneGrid = rf_grid,
                       num.trees = 300, importance = "impurity")

bill_check <- resamples(list(Without_BILL = all_models[["Combined (baseline)"]]$RandomForest,
                             With_BILL    = rf_with_bills))
summary(bill_check, metric = "ROC")
summary(diff(bill_check, metric = "ROC"))
# Previous run: mean ROC 0.780 without vs 0.783 with (p = 0.008).
# A statistically detectable but practically negligible gain (~0.003 AUC),
# so the simpler model without the 6 bill variables is kept. The gain
# suggests bill amounts are informative only IN COMBINATION with other
# variables (e.g. bill relative to LIMIT_BAL), which univariate plots cannot
# show - a limitation of the visual feature-selection method.