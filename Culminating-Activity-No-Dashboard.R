library(class)
library(caret)
library(psych)
library(corrplot)
 
 # ── Data Loading ───────────────────────────────────────────────────────────────
 setwd("~/Desktop/Coding/RStudio/Culminating-Activity")
 dev_data <- read.csv("ai_dev_productivity.csv")
 dev_data$task_success <- factor(dev_data$task_success, levels = c(0, 1),
                                 labels = c("Fail", "Success"))
 
 features <- c("hours_coding", "coffee_intake_mg", "distractions",
               "sleep_hours", "commits", "bugs_reported",
               "ai_usage_hours", "cognitive_load")
 
 # ── Descriptive Statistics ─────────────────────────────────────────────────────
 describe(dev_data[, features])[, c("mean", "sd", "min", "max", "n")]
 
 table(dev_data$task_success)
 prop.table(table(dev_data$task_success)) * 100
 
 # ── Histograms ─────────────────────────────────────────────────────────────────
 par(mfrow = c(4, 2), mar = c(4, 4, 3, 1))
 colors <- c("#0969da", "#9a6700", "#cf222e", "#08306b",
             "#1a7f37", "#cf222e", "#0969da", "#57606a")
 
 for (i in seq_along(features)) {
   hist(dev_data[[features[i]]],
        main   = features[i],
        xlab   = "",
        col    = colors[i],
        border = NA,
        breaks = 20)
 }
 par(mfrow = c(1, 1))
 
 # ── Bar Plot ───────────────────────────────────────────────────────────────────
 tb <- table(dev_data$task_success)
 barplot(tb,
         col       = c("#cf222e", "#1a7f37"),
         border    = NA,
         main      = "Task Success Distribution",
         ylab      = "Count",
         names.arg = c("Fail", "Success"))
 
 # ── Boxplots ───────────────────────────────────────────────────────────────────
 par(mfrow = c(4, 2), mar = c(4, 4, 3, 1))
 
 for (i in seq_along(features)) {
   boxplot(dev_data[[features[i]]] ~ dev_data$task_success,
           main   = features[i],
           xlab   = "Task Success",
           ylab   = "",
           col    = c("#cf222e", "#1a7f37"),
           border = "#24292f")
 }
 par(mfrow = c(1, 1))
 
 # ── Correlation Matrix ─────────────────────────────────────────────────────────
 corr_matrix <- cor(dev_data[, features])
 corrplot(corr_matrix,
          method      = "color",
          type        = "upper",
          tl.cex      = 0.8,
          tl.col      = "#24292f",
          addCoef.col = "#24292f",
          number.cex  = 0.7,
          col         = colorRampPalette(c("#cf222e", "#ffffff", "#0969da"))(200))
 
 # ── Split ──────────────────────────────────────────────────────────────────────
 set.seed(31)
 n         <- nrow(dev_data)
 train_idx <- sample(seq_len(n), size = floor(0.8 * n))
 train_df  <- dev_data[train_idx, ]
 test_df   <- dev_data[-train_idx, ]
 
 # ── Scale ──────────────────────────────────────────────────────────────────────
 tr_raw <- train_df[, features]
 te_raw <- test_df[, features]
 
 tr_sc  <- scale(tr_raw)
 te_sc  <- scale(te_raw,
                 center = attr(tr_sc, "scaled:center"),
                 scale  = attr(tr_sc, "scaled:scale"))
 
 # ── Accuracy Across k Values ───────────────────────────────────────────────────
 k_vals  <- seq(1, 31, by = 2)
 acc_vec <- sapply(k_vals, function(k) {
   pred <- class::knn(tr_sc, te_sc, train_df$task_success, k = k)
   mean(pred == test_df$task_success)
 })
 
 best_k <- k_vals[which.max(acc_vec)]
 cat("Best k:", best_k, "\n")
 
 barplot(acc_vec * 100,
         names.arg = k_vals,
         col       = ifelse(k_vals == best_k, "#0969da", "#d0d7de"),
         border    = NA,
         main      = "Accuracy Across k Values",
         xlab      = "k",
         ylab      = "Accuracy (%)",
         ylim      = c(0, 100))
 
 legend("bottomright",
        legend = c("Best k", "Other k"),
        fill   = c("#0969da", "#d0d7de"),
        border = NA, bty = "n")
 
 # ── Final Model ────────────────────────────────────────────────────────────────
 pred_best            <- class::knn(tr_sc, te_sc, train_df$task_success, k = best_k)
 pred_best            <- factor(as.character(pred_best), levels = c("Success", "Fail"))
 test_df$task_success <- factor(as.character(test_df$task_success), levels = c("Success", "Fail"))
 
 cm <- caret::confusionMatrix(pred_best, test_df$task_success, positive = "Success")
 print(cm)
 
 # ── Performance Metrics ────────────────────────────────────────────────────────
 tbl  <- cm$table
 tp   <- tbl[1, 1]; fn <- tbl[1, 2]
 fp   <- tbl[2, 1]; tn <- tbl[2, 2]
 
 acc  <- round(((tp + tn) / (tp + tn + fp + fn)) * 100, 1)
 sens <- round((tp / (tp + fn)) * 100, 1)
 spec <- round((tn / (tn + fp)) * 100, 1)
 f1   <- round((2 * tp / (2 * tp + fp + fn)) * 100, 1)
 
 cat("Accuracy:   ", acc,  "%\n")
 cat("Sensitivity:", sens, "%\n")
 cat("Specificity:", spec, "%\n")
 cat("F1 Score:   ", f1,   "%\n")
 
 # ── Scatter Plot ───────────────────────────────────────────────────────────────
 plot_df         <- test_df
 plot_df$pred    <- pred_best
 plot_df$correct <- pred_best == test_df$task_success
 
 col_map <- ifelse(plot_df$pred == "Success", "#1a7f37", "#cf222e")
 pch_map <- ifelse(plot_df$correct, 16, 4)
 
 plot(plot_df$hours_coding, plot_df$commits,
      col  = col_map,
      pch  = pch_map,
      cex  = 1.2,
      main = "kNN Prediction: Coding Hours vs Commits",
      xlab = "Coding Hours",
      ylab = "Commits")
 
 legend("topright",
        legend = c("Predicted Success", "Predicted Fail", "Correct", "Misclassified"),
        col    = c("#1a7f37", "#cf222e", "#57606a", "#57606a"),
        pch    = c(16, 16, 16, 4),
        bty    = "n")