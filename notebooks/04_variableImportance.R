library(dplyr)
library(ggplot2)

importance_list <- list()

for(i in 1:X_packages) {
  model_name <- paste0("model_", i)
  current_model <- model_ensemble[[model_name]]
  current_trees <- optimal_trees[i]
 
  raw_inf <- summary(current_model, n.trees = current_trees, plotit = FALSE)

  var_names <- if("var" %in% names(raw_inf)) as.character(raw_inf$var) else rownames(raw_inf)
  
  importance_list[[model_name]] <- data.frame(
    Variable = var_names,
    Influence = raw_inf$rel.inf,
    Model = model_name,
    stringsAsFactors = FALSE
  )
}

#all 5 models
ensemble_importance_raw <- bind_rows(importance_list)

ensemble_importance <- ensemble_importance_raw %>%
  group_by(Variable) %>%
  summarise(
    Mean_Relative_Influence = mean(Influence, na.rm = TRUE),
    SD_Relative_Influence = sd(Influence, na.rm = TRUE)
  ) %>%
  arrange(desc(Mean_Relative_Influence))

print(as.data.frame(ensemble_importance))

#plot
ggplot(ensemble_importance, aes(x = reorder(Variable, Mean_Relative_Influence), y = Mean_Relative_Influence)) +
  geom_col(fill = "darkred", color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = pmax(0, Mean_Relative_Influence - SD_Relative_Influence), 
                    ymax = Mean_Relative_Influence + SD_Relative_Influence), 
                width = 0.2, color = "gray30", alpha = 0.8) +
  coord_flip() +
  labs(
    title = "Variable Importance",
    x = "predictors",
    y = "mean relative importance (%)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )
