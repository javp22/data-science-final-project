# data-science-final-project
# Traffic Accident Analysis Before and After Toll Removal

## Project Overview

This project analyzes traffic accidents occurring before and after the removal of highway tolls. The objective is to determine whether the characteristics of accidents changed following the policy intervention and to identify the variables that best explain the differences between both periods.

The workflow includes:

1. Data loading and preprocessing.
2. Exploratory Data Analysis (EDA).
3. Classification using Gradient Boosting Machines (GBM).
4. Decision tree modeling for interpretability.
5. Variable importance analysis.

---

## Project Structure

```text
.
├── data/
│   └── (input datasets)
│
├── notebooks/
│   ├── 00_loadingData.R
│   ├── 01_eda.R
│   ├── 02_classifier_bgm.R
│   ├── 03_variableDecisionModel.R
│   └── 04_variableImportance.R
│
├── plots/
│   └── eda/
│       ├── accident_outcomes.png
│       ├── accident_type_distribution.png
│       ├── accidents_by_year.png
│       ├── ...
│
└── README.md
```

---

## Requirements

Install the required R packages before running the analysis:

```r
install.packages(c(
  "tidyverse",
  "caret",
  "gbm",
  "rpart",
  "rpart.plot",
  "ggplot2",
  "dplyr"
))
```

Depending on the implementation, additional packages may be required.

---

## How to Run the Project

### Step 1: Load and Prepare the Data

Run:

```r
source("notebooks/00_loadingData.R")
```

This script loads the raw data and performs the necessary preprocessing steps.

---

### Step 2: Exploratory Data Analysis

Run:

```r
source("notebooks/01_eda.R")
```

This script generates descriptive statistics and visualizations that are saved under:

```text
plots/eda/
```

---

### Step 3: Train the GBM Classifier

Run:

```r
source("notebooks/02_classifier_bgm.R")
```

The model attempts to classify accidents as occurring either before or after toll removal based on infrastructure, environmental, and accident-related characteristics.

---

### Step 4: Decision Tree Analysis

Run:

```r
source("notebooks/03_variableDecisionModel.R")
```

This script trains an interpretable decision tree model to identify the most relevant decision rules separating the two periods.

---

### Step 5: Variable Importance

Run:

```r
source("notebooks/04_variableImportance.R")
```

This script computes and visualizes variable importance rankings, helping identify the factors that contributed most to the classification task.

---

## Outputs

The project produces:

* Exploratory visualizations.
* Classification performance metrics.
* Decision tree visualizations.
* Variable importance rankings.

These outputs can be used to evaluate whether toll removal was associated with a structural change in accident characteristics.

