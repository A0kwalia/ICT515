# =============================================================================
# ICT515 Assignment 2 - Default of Credit Card Clients
# Part 2: Feature selection (visual) + model selection
#
#   1. Load & clean the data (your existing code)
#   2. Prepare the data for analysis (readable names, proper factor types)
#   3. Feature selection: plot every predictor against the outcome (DEFAULT)
#   4. Build the two feature sets:
#        - STATE-BASED   : static/demographic variables (my part)
#        - COMBINED      : state + selected temporal variables (baseline)
#      (the TEMPORAL-only set is modelled by the other team, using the SAME
#       train/test split and CV folds saved in this script)
#   5. Model selection: train the same 3 algorithms on each feature set
#        - Logistic Regression, Decision Tree, Random Forest
#   6. Evaluate the chosen models on the held-out test set and compare
# =============================================================================


# -----------------------------------------------------------------------------
# 0. PACKAGES
# -----------------------------------------------------------------------------
# Install only the packages that are missing, so re-running the script does
# not reinstall everything each time.
packages <- c("readxl", "SmartEDA", "dplyr", "ggplot2", "gridExtra",
              "caret", "e1071", "rpart", "randomForest", "pROC")
missing_pkgs <- packages[!packages %in% installed.packages()[, "Package"]]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)

library(readxl)
library(SmartEDA)
library(dplyr)
library(ggplot2)      # plotting
library(gridExtra)    # putting two ggplots side by side
library(caret)        # one consistent interface for training/tuning models
library(rpart)        # decision tree engine used by caret
library(randomForest) # random forest engine used by caret
library(pROC)         # ROC curves and AUC



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
ExpCatViz(
  data = credit_data, 
  target = "Y", 
  clim = 15
)



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

# Overall default rate - this is the "reference line" for every plot below.
# About 22% of clients default, so the classes are imbalanced.
overall_rate <- mean(model_data$DEFAULT == "Yes")
overall_rate


# -----------------------------------------------------------------------------
# 3. FEATURE SELECTION: PLOT EACH PREDICTOR AGAINST THE OUTCOME
# -----------------------------------------------------------------------------
# Idea: a predictor is useful if the default rate CHANGES across its values.
# If the default rate stays flat at ~22% no matter what the predictor's value
# is, the predictor tells the model nothing and can be dropped.
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
# State-based predictors (needed for the state model)
for (v in intersect(state_vars, categorical_vars)) plot_categorical(model_data, v)
for (v in intersect(state_vars, numeric_vars))     plot_numeric(model_data, v)

# Temporal predictors (needed to choose the temporal part of the COMBINED
# baseline; the other team can reuse these plots too)
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
# What the plots show (check these against your own plots before submitting):
#  STATE-BASED
#   - LIMIT_BAL : clear downward trend - default ~32% for the lowest limits,
#                 ~14% for the highest. Strong -> KEEP
#   - EDUCATION : HighSchool/University default more than GradSchool;
#                 "Other" is much lower. -> KEEP
#   - MARRIAGE  : Married and Other default more than Single. Moderate -> KEEP
#   - AGE       : boxplots look almost identical (same median), BUT the decile
#                 plot is U-shaped: young (<25) and older (>45) clients default
#                 more. -> KEEP (tree/forest models can use a U-shape)
#   - SEX       : males default slightly more (~24% vs ~21%). Weakest of the
#                 five but consistent over large groups -> KEEP (borderline)
#  -> all 5 state-based variables are kept.
#
#  TEMPORAL (for the combined baseline)
#   - PAY_*     : default rate jumps from ~15% (paid on time) to 50-70% for
#                 clients 2+ months late. By far the strongest -> KEEP all 6
#   - PAYAMT_*  : clients who pay very little default more (~30-35%) than
#                 those who pay a lot (~13%). Clear trend -> KEEP all 6
#   - BILL_*    : default rate stays flat around 20-26% across all deciles
#                 with no trend. Not informative -> DROP all 6
state_features    <- state_vars
temporal_selected <- c(pay_status_vars, pay_amount_vars)
combined_features <- c(state_features, temporal_selected)   # baseline set


# -----------------------------------------------------------------------------
# 5. TRAIN/TEST SPLIT AND CROSS-VALIDATION SET-UP (shared with the other team)
# -----------------------------------------------------------------------------
# For the comparison "state vs temporal vs combined" to be fair, ALL models
# must be trained and tested on exactly the same rows. So we create the split
# and the CV folds once, with a fixed seed, and save them to files that the
# temporal team loads with readRDS().
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

saveRDS(train_index, "train_index.rds")   # send these two files to the
saveRDS(cv_folds,    "cv_folds.rds")      # temporal team

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
# Algorithms (all used in Yeh & Lien, 2009, or a standard extension):
#  - Logistic regression : simple, interpretable linear baseline
#  - Decision tree       : captures non-linear effects (e.g. AGE's U-shape)
#                          and thresholds; easy to explain
#  - Random forest       : many trees averaged; usually the most accurate,
#                          less interpretable
# caret tunes each model's settings (hyper-parameters) using the CV folds and
# keeps the setting with the best mean ROC.
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
  
  # mtry = number of predictors tried at each split; caret tries 3 values.
  # ntree = 200 keeps training time reasonable (this is the slowest model).
  set.seed(515)
  forest <- train(model_formula, data = data, method = "rf",
                  metric = "ROC", trControl = ctrl, tuneLength = 3, ntree = 200)
  
  list(LogisticRegression = logreg, DecisionTree = tree, RandomForest = forest)
}

# Train on the STATE-BASED features (may take a few minutes)
state_models <- fit_three_models(state_features, train_data)

# Train on the COMBINED features = baseline (slower: more predictors)
combined_models <- fit_three_models(combined_features, train_data)

# --- Compare the 3 algorithms using their cross-validation results ----------
# resamples() lines up the 5 fold-level scores of each model so they can be
# compared fairly (same folds for every model).
state_cv    <- resamples(state_models)
combined_cv <- resamples(combined_models)

# NOTE: in these CV summaries caret treats the FIRST factor level ("No") as
# the positive class, so "Sens" here = non-defaulters caught and "Spec" =
# defaulters caught. ROC is unaffected. Section 7 reports them the right way
# round (positive = "Yes").
summary(state_cv)      # mean ROC / Sens / Spec across folds - state models
summary(combined_cv)   # same for combined models

# Box plots of fold-level ROC: the model whose box sits highest (and is narrow)
# is the best and most stable choice.
bwplot(state_cv,    metric = "ROC", main = "State-based models: CV ROC AUC")
bwplot(combined_cv, metric = "ROC", main = "Combined (baseline) models: CV ROC AUC")

# Pick the best algorithm for each feature set by mean CV ROC
best_name <- function(cv) {
  mean_roc <- colMeans(cv$values[, grep("~ROC$", names(cv$values))])
  sub("~ROC$", "", names(which.max(mean_roc)))
}
best_state    <- best_name(state_cv)
best_combined <- best_name(combined_cv)
best_state
best_combined

# Which predictors did the best models rely on most?
plot(varImp(state_models[[best_state]]),
     main = paste("Variable importance -", best_state, "(state)"))
plot(varImp(combined_models[[best_combined]]), top = 15,
     main = paste("Variable importance -", best_combined, "(combined)"))


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
  data.frame(AUC              = round(as.numeric(auc(roc_obj)), 3),
             Accuracy         = round(cm$overall[["Accuracy"]], 3),
             Sensitivity      = round(cm$byClass[["Sensitivity"]], 3),  # defaulters caught
             Specificity      = round(cm$byClass[["Specificity"]], 3),  # non-defaulters correctly cleared
             BalancedAccuracy = round(cm$byClass[["Balanced Accuracy"]], 3))
}

# Evaluate ALL six models (3 algorithms x 2 feature sets) on the test set
results <- bind_rows(
  lapply(names(state_models), function(m)
    cbind(FeatureSet = "State-based", Model = m,
          evaluate_on_test(state_models[[m]], test_data, threshold))),
  lapply(names(combined_models), function(m)
    cbind(FeatureSet = "Combined (baseline)", Model = m,
          evaluate_on_test(combined_models[[m]], test_data, threshold)))
)
print(results)
write.csv(results, "test_results_state_vs_combined.csv", row.names = FALSE)

# ROC curves on the test set: the further a curve bulges to the top-left,
# the better. The diagonal = random guessing (AUC 0.5).
roc_list <- c(
  setNames(lapply(state_models, function(m)
    roc(test_data$DEFAULT, predict(m, test_data, type = "prob")[, "Yes"],
        levels = c("No", "Yes"), direction = "<", quiet = TRUE)),
    paste("State -", names(state_models))),
  setNames(lapply(combined_models, function(m)
    roc(test_data$DEFAULT, predict(m, test_data, type = "prob")[, "Yes"],
        levels = c("No", "Yes"), direction = "<", quiet = TRUE)),
    paste("Combined -", names(combined_models)))
)
roc_plot <- ggroc(roc_list, legacy.axes = TRUE) +
  geom_abline(linetype = "dashed", colour = "grey50") +
  labs(title = "Test-set ROC curves: state-based vs combined",
       x = "False positive rate (1 - specificity)",
       y = "True positive rate (sensitivity)", colour = "Model") +
  theme_minimal()
print(roc_plot)
ggsave("roc_state_vs_combined.png", roc_plot, width = 8, height = 5)

# Save the trained models so they don't have to be retrained for the report
saveRDS(state_models,    "state_models.rds")
saveRDS(combined_models, "combined_models.rds")



