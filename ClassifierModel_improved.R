# ==============================================================================
# STAGE 1: DATA PREPARATION & MIXED VARIABLE HANDLING
# ==============================================================================
library(dplyr)
library(caret)
library(randomForest)
library(pROC)
library(ggplot2)

set.seed(123) # For strict reproducibility

# 1. Prepare the dataset with BOTH high-quality and "lower-quality" variables
df_pipeline <- df %>%
  filter(year != 2020) %>%                          # Drop COVID anomaly year
  filter(!is.na(period)) %>%                         # Ensure target variable is clean
  mutate(period = factor(period, levels = c("Before", "After"))) %>%
  
  # Select your complete feature pool
  select(
    period,
    # Core variables
    C_VELOCITAT_VIA, D_TIPUS_VIA, F_VEH_PESANTS_IMPLICADES, F_UNITATS_IMPLICADES, D_LLUMINOSITAT,
    # Group project engineered / lower-quality variables
    affected_highway, tipAcc, is_night, is_peak_hour, zona, weekend
  ) %>%
  
  # Safe NA Imputation: Convert categorical NAs to an explicit "Unknown" group
  # This stops na.omit() from wiping out half your rows!
  mutate(across(where(is.factor), ~addNA(.x) %>% fct_na_value_to_level("Unknown"))) %>%
  mutate(across(where(is.character), ~ifelse(is.na(.x), "Unknown", .x) %>% as.factor())) %>%
  
  # Drop rows ONLY if vital numeric values are completely missing
  filter(!is.na(C_VELOCITAT_VIA) & !is.na(F_VEH_PESANTS_IMPLICADES)) %>%
  droplevels()

# 2. Train-Test Split (80/20)
train_idx <- createDataPartition(df_pipeline$period, p = 0.8, list = FALSE)
train_set <- df_pipeline[train_idx, ]
test_set  <- df_pipeline[-train_idx, ]


# ==============================================================================
# STAGE 2: THE KITCHEN-SINK RANDOM FOREST (AUTOMATED SUBSETTING)
# ==============================================================================
message("\n--- Running Stage 2: Random Forest Feature Selection Loop ---")

# Downsample the training set to handle class imbalance natively inside the Forest
rf_pipeline_model <- randomForest(
  period ~ ., 
  data = train_set, 
  ntree = 500, 
  importance = TRUE,
  sampsize = c(Before = min(table(train_set$period)), After = min(table(train_set$period)))
)

# Extract variable importance in percent
importance_matrix <- as.data.frame(importance(rf_pipeline_model))
importance_matrix$Variable <- rownames(importance_matrix)

# Sort them to find your top performers automatically
top_variables <- importance_matrix %>%
  arrange(desc(MeanDecreaseGini)) %>%
  slice_head(n = 5) %>% # Automatically extract the top 5 high-impact features
  pull(Variable)

message("The Random Forest has selected your top 5 robust predictors across all tree combinations:")
print(top_variables)


# ==============================================================================
# STAGE 3: THE FINAL INTERPRETABLE LOGISTIC REGRESSION
# ==============================================================================
message("\n--- Running Stage 3: Fitting Final Core Logistic Regression ---")

# Dynamically build the model formula using only the top 5 variables extracted above
final_formula <- as.formula(paste("period ~", paste(top_variables, collapse = " + ")))

# Downsample the training data specifically for the Logistic Regression
balanced_train_lr <- downSample(
  x = train_set %>% select(all_of(top_variables)), 
  y = train_set$period, 
  yname = "period"
)

# Train the final policy model
final_glm_model <- glm(final_formula, data = balanced_train_lr, family = binomial)


# ==============================================================================
# STAGE 4: EVALUATION & VISUALIZATION FOR YOUR REPORT
# ==============================================================================
message("\n--- Final Model Metrics ---")

# 1. Predict and evaluate the final model against the original un-balanced test set
final_probs <- predict(final_glm_model, newdata = test_set, type = "response")
final_preds <- factor(ifelse(final_probs > 0.5, "After", "Before"), levels = c("Before", "After"))

final_cm <- confusionMatrix(final_preds, test_set$period, positive = "After")
print(final_cm)

# 2. Plot the Variable Importance Chart showing the feature selection process
rf_imp_plot <- ggplot(importance_matrix, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "darkred", color = "black", width = 0.6) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Stage 2: Feature Selection Profile",
    subtitle = "Consistent impact across 500 random variable subsets",
    x = "All Tested Features", y = "Mean Decrease Gini"
  )
print(rf_imp_plot)

# Save the plot safely to your processed graphics directory
ggsave("../plots/pipeline_feature_selection.png", plot = rf_imp_plot, width = 7, height = 4.5, dpi = 300)

