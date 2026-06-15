library(dplyr)
library(caret)
library(gbm)
library(pROC)

set.seed(123)

#configuration
X_packages       <- 5
TARGET_DEPTH     <- 5 
TARGET_SHRINKAGE <- 0.02
MAX_TREES        <- 2500

#final_formula <- as.formula("period_numeric ~ C_VELOCITAT_VIA + D_LLUMINOSITAT + tipAcc + D_TIPUS_VIA + F_UNITATS_IMPLICADES + nomDem")
final_formula <- as.formula("period_numeric ~ D_SUBTIPUS_ACCIDENT + F_ALTRES_UNIT_IMPLICADES + D_TIPUS_VIA + 
                            D_INFLUIT_CARACT_ENTORN + D_REGULACIO_PRIORITAT + D_LLUMINOSITAT + D_TITULARITAT_VIA + D_SUBZONA + D_CARACT_ENTORN + D_SENTITS_VIA + 
                            C_VELOCITAT_VIA + D_INFLUIT_VISIBILITAT + D_CARRIL_ESPECIAL + F_VIANANTS_IMPLICADES + F_CICLOMOTORS_IMPLICADES + D_TRACAT_ALTIMETRIC + grupHor + weekday")
#pre-process
df_cleanData <- df

df_cleanData$period <- as.factor(df_cleanData$period)
cols_to_factor <- c("nomDem", "D_LLUMINOSITAT", "tipAcc", "D_TIPUS_VIA")
for(col in cols_to_factor) if(col %in% names(df_cleanData)) df_cleanData[[col]] <- as.factor(df_cleanData[[col]])

df_pipeline <- df_cleanData %>% mutate(period_numeric = ifelse(period == "After", 1, 0))

df_before <- df_pipeline %>% filter(period_numeric == 0)
df_after  <- df_pipeline %>% filter(period_numeric == 1)

df_before <- df_before[sample(nrow(df_before)), ]
df_before$package_id <- cut(seq_len(nrow(df_before)), breaks = X_packages, labels = FALSE)

#test set
test_size <- floor(nrow(df_after) * 0.20)
test_after  <- df_after[1:test_size, ]
train_after <- df_after[(test_size + 1):nrow(df_after), ]

package_1_indices   <- which(df_before$package_id == 1)
test_before_indices <- package_1_indices[1:test_size]
test_before     <- df_before[test_before_indices, ]
df_before_train <- df_before[-test_before_indices, ]

final_test_set <- rbind(test_after, test_before %>% select(-package_id)) %>% droplevels()

#training loop
model_ensemble <- list()
optimal_trees  <- numeric(X_packages)

print("--- STARTING TRAINING FOR OPTIMIZED GBM COMMITTEE ---")

for(i in 1:X_packages) {
  #balanced subset
  current_before_chunk <- df_before_train %>% filter(package_id == i) %>% select(-package_id) %>% droplevels()
  current_after_chunk  <- train_after %>% sample_n(nrow(current_before_chunk), replace = TRUE)
  balanced_subset <- rbind(current_before_chunk, current_after_chunk) %>% droplevels()
  
  model_name <- paste0("model_", i)
  
  #boosted tree
  boost_fit <- gbm(
    final_formula, 
    data = balanced_subset, 
    distribution = "bernoulli", 
    n.trees = MAX_TREES, 
    interaction.depth = TARGET_DEPTH, 
    shrinkage = TARGET_SHRINKAGE,
    bag.fraction = 0.8,
    cv.folds = 5, 
    n.cores = 1 
  )
  
  optimal_trees[i] <- gbm.perf(boost_fit, method = "cv", plot.it = FALSE)
  model_ensemble[[model_name]] <- boost_fit
  print(paste("Sub-Model", i, "optimized at:", optimal_trees[i], "trees."))
}

#prediction matrix
prediction_matrix <- matrix(NA, nrow = nrow(final_test_set), ncol = X_packages)

for(i in 1:X_packages) {
  model_name <- paste0("model_", i)
  prediction_matrix[, i] <- predict(
    model_ensemble[[model_name]], 
    newdata = final_test_set, 
    n.trees = optimal_trees[i], 
    type = "response"
  )
}

#average probabilities
ensemble_average_probs <- rowMeans(prediction_matrix, na.rm = TRUE)

#calibration
roc_obj <- roc(final_test_set$period_numeric, ensemble_average_probs, quiet = TRUE)

optimal_coords <- coords(roc_obj, "best", ret = "threshold", best.method = "closest.topleft")
balanced_threshold <- as.numeric(optimal_coords$threshold)

print(paste("   OPTIMIZED BALANCED THRESHOLD FOUND:", round(balanced_threshold, 4)))

final_ensemble_preds <- factor(ifelse(ensemble_average_probs > balanced_threshold, "After", "Before"), levels = c("Before", "After"))
test_actuals <- factor(ifelse(final_test_set$period_numeric == 1, "After", "Before"), levels = c("Before", "After"))

#
#final metrics
#

print(confusionMatrix(final_ensemble_preds, test_actuals, positive = "After"))


roc_obj <- roc(response = final_test_set$period_numeric, 
               predictor = ensemble_average_probs)

plot(roc_obj, 
     main = "ROC Curve", 
     col = "blue", 
     lwd = 2, 
     print.auc = TRUE)

