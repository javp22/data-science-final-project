# ==============================================================================
# MULTI-MODEL PIPELINE: LOGISTIC REGRESSION VS. RANDOM FOREST
# ==============================================================================
library(dplyr)
library(caret)
library(randomForest)
library(pROC)
library(ggplot2)

# 1. INITIAL SETUP & TRAIN-TEST SPLIT
# Ensure you run the data cleaning script first so df_modeling is in memory
set.seed(123) # For reproducibility

# Create an 80/20 train/test split
train_idx  <- createDataPartition(df_modeling$period, p = 0.8, list = FALSE)
train_set  <- df_modeling[train_idx, ]
test_set   <- df_modeling[-train_idx, ]


# ==============================================================================
# PHASE 1: EVALUATE LOGISTIC REGRESSION BASELINE
# ==============================================================================
message("\n--- PHASE 1: Evaluating Logistic Regression Baseline ---")

# Re-fit the model on the clean training set
glm_model <- glm(period ~ ., data = train_set, family = binomial)

# Predict probabilities on the test set
glm_probs <- predict(glm_model, newdata = test_set, type = "response")

# Convert probabilities to class decisions (Threshold = 0.5)
# Note: R maps the second factor level ('After') to success if using standard factors
glm_preds <- factor(ifelse(glm_probs > 0.5, "After", "Before"), levels = c("Before", "After"))

# 1A. Generate Confusion Matrix
glm_cm <- confusionMatrix(glm_preds, test_set$period, positive = "After")
print(glm_cm)

# 1B. Calculate AUC-ROC
glm_roc <- roc(test_set$period, glm_probs, levels = c("Before", "After"), direction = "<")
message("Logistic Regression AUC Score: ", round(auc(glm_roc), 4))


# ==============================================================================
# PHASE 2: RUN RANDOM FOREST CLASSIFIER
# ==============================================================================
message("\n--- PHASE 2: Training Random Forest ---")

# Downsample or adjust class weights because 'Before' heavily outweighs 'After'
# This stops the Random Forest from blindly guessing 'Before' to get high accuracy
rf_model <- randomForest(
  period ~ ., 
  data = train_set, 
  ntree = 500, 
  importance = TRUE,
  sampsize = c(Before = min(table(train_set$period)), After = min(table(train_set$period))) # Balanced sampling
)

# Predict classes and probabilities on the test set
rf_preds <- predict(rf_model, newdata = test_set, type = "response")
rf_probs <- predict(rf_model, newdata = test_set, type = "prob")[, "After"]

# 2A. Generate Confusion Matrix for Random Forest
rf_cm <- confusionMatrix(rf_preds, test_set$period, positive = "After")
print(rf_cm)

# 2B. Calculate AUC-ROC for Random Forest
rf_roc <- roc(test_set$period, rf_probs, levels = c("Before", "After"), direction = "<")
message("Random Forest AUC Score: ", round(auc(rf_roc), 4))


# ==============================================================================
# PHASE 3: COMPARISON DIAGRAMS
# ==============================================================================

# 3A. Plot ROC Curves Side-by-Side
plot(glm_roc, col = "#377EB8", main = "ROC Curve Comparison: Eras Identification", lwd = 2)
plot(rf_roc, col = "#E41A1C", add = TRUE, lwd = 2)
legend("bottomright", legend = c(paste("Logistic Regression (AUC:", round(auc(glm_roc), 2), ")"),
                                 paste("Random Forest (AUC:", round(auc(rf_roc), 2), ")")),
       col = c("#377EB8", "#E41A1C"), lmd = 2, lwd = 2)

# 3B. Plot Variable Importance from Random Forest
# This fulfills the scientific requirement to see which factors matter most
importance_df <- as.data.frame(importance(rf_model))
importance_df$Variable <- rownames(importance_df)

p_imp <- ggplot(importance_df, aes(x = reorder(Variable, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col(fill = "steelblue", color = "black") +
  coord_flip() +
  theme_minimal() +
  labs(title = "Random Forest: Variable Importance",
       subtitle = "Which factors most uniquely define the post-toll era?",
       x = "Variables", y = "Importance (Mean Decrease Gini)")

print(p_imp)