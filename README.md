## Dataset

- **Source:** Kaggle — AI Developer Productivity Dataset
- **Observations:** 500
- **Target:** `task_success` (0 = Fail, 1 = Success)
- **Features:** Coding Hours, Coffee Intake (mg), Distractions, Sleep Hours, Commits, Bugs Reported, AI Usage Hours, Cognitive Load

## Model

- **Algorithm:** k-Nearest Neighbors (kNN) via `class` package in R
- **Split:** 80/20 (400 train / 100 test) with `set.seed(31)`
- **Scaling:** Z-score normalization applied to all features
- **Tuning:** Accuracy evaluated across all odd k values from 1 to 31

## Evaluation Metrics

- Accuracy
- Sensitivity (True Positive Rate)
- Specificity (True Negative Rate)
- F1 Score (Harmonic Mean)

## Requirements

```r
install.packages(c("class", "caret", "psych", "corrplot", "shiny"))
```

## Usage

To knit the final report to PDF, open `Culminating-Activity.Rmd` in RStudio and click **Knit**, or run:

```r
rmarkdown::render("Culminating-Activity.Rmd")
```

To run the Shiny dashboard, open the `kNN-Classification-Culminating-Activity` folder in RStudio and click **Run App**.
