dir.create("reports/cart", recursive=TRUE, showWarnings=FALSE)
# =====================================================
# 0. Load packages ------------------------------------------------
library(rpart)        # CART decision tree
library(rpart.plot)   # Plot decision tree
# Empirical ROC AUC (Mann-Whitney rank statistic; ties get average ranks).
# Equivalent orientation to pROC with NotPopular as control, Popular as case.
auc_rank <- function(actual, probability) {
 positive <- actual == "Popular"
 n1 <- sum(positive); n0 <- sum(!positive)
 if (!n1 || !n0) return(NA_real_)
 (sum(rank(probability, ties.method="average")[positive])-n1*(n1+1)/2)/(n1*n0)
}
library(ggplot2)      # Visualization
# 1. Import dataset (Change to your own file path!!!)------------------------------------------------
df <- read.csv("data/processed/m2_cleaned_data.csv", stringsAsFactors = FALSE)
# 2. Convert target variable to factor ------------------------------------------------
df$popular <- factor(df$popular, levels = c(0, 1),
                     labels = c("NotPopular", "Popular"))
# 3. Split data by temporal indicator ------------------------------------------------
train <- df[df$temporal_split == "Training", ]
valid <- df[df$temporal_split == "Validation", ]
test  <- df[df$temporal_split == "Test", ]
cat("Sample size: train =", nrow(train), "| valid =", nrow(valid),
    "| test =", nrow(test), "\n")
# 4. Feature list ------------------------------------------------
exclude <- c("url", "timedelta", "shares", "popular", "temporal_split",
             "period",
             "data_channel_is_lifestyle", "data_channel_is_entertainment",
             "data_channel_is_bus", "data_channel_is_socmed",
             "data_channel_is_tech", "data_channel_is_world",
             "weekday_is_monday", "weekday_is_tuesday", "weekday_is_wednesday",
             "weekday_is_thursday", "weekday_is_friday", "weekday_is_saturday",
             "weekday_is_sunday")
feats <- setdiff(names(df), exclude)
cat("Total candidate features for model:", length(feats), "\n")
form  <- as.formula(paste("popular ~", paste(feats, collapse = " + ")))
# 5. Train full CART tree on training set ----------------------------------------
set.seed(42)
fit <- rpart(form, data = train, method = "class",
             control = rpart.control(minsplit = 20, minbucket = 7,
                                     cp = 0, maxdepth = 10))
printcp(fit)
# 6. Select optimal cp using validation set ----------------------------------------
cps    <- fit$cptable[, "CP"]
valErr <- sapply(cps, function(cp) {
  m    <- prune(fit, cp = cp)
  pred <- predict(m, valid, type = "class")
  mean(pred != valid$popular)
})
bestCP   <- cps[which.min(valErr)]
fit_final <- prune(fit, cp = bestCP)
cat("\nOptimal cp =", bestCP, "| Validation misclassification rate =", min(valErr), "\n")
# ====================== ✨New: Extract features used after bestCP pruning ======================
node_info <- fit_final$frame
# Filter leaf nodes, retain split (non-leaf) nodes only
split_vars <- node_info[node_info$var != "<leaf>", "var"]
used_features <- unique(as.character(split_vars))
cat("\n========== Feature list actually used by pruned fit_final model ==========\n")
print(used_features)
cat("Number of features used in fit_final:", length(used_features), "\n")
# =======================================================================================
# ============ 🖼 Figure1: CP - Validation Error Curve ============
cp_df <- data.frame(cp = cps, val_error = valErr)
best_pt <- cp_df[which.min(cp_df$val_error), ]
p1 <- ggplot(cp_df[cp_df$cp > 0, ], aes(x = cp, y = val_error)) +
  geom_line(linewidth = 1, colour = "#2E5FA8") +
  geom_point(size = 1.2, colour = "#2E5FA8") +
  geom_vline(xintercept = best_pt$cp, linetype = "dashed", colour = "red") +
  geom_point(data = best_pt, colour = "red", size = 3) +
  labs(x = "Complexity Parameter (log scale)", y = "Validation Error Rate",
       title = "Validation Error vs. CP, red dashed line = optimal CP") +
  scale_x_log10() +
  theme_bw()
ggsave("reports/cart/fig1_cp_error_curve.pdf", p1, width = 8, height = 5)
# 7. Plot final decision tree ------------------------------------------------
# ============ 🖼 Figure2: CART Decision Tree ============
pdf("reports/cart/fig2_decision_tree.pdf", width = 13, height = 7)
rpart.plot(fit_final, type = 2, extra = 104, fallen.leaves = TRUE,
           tweak = 1.0, box.palette = "RdYlGn",
           main = "CART Decision Tree: Predicting Article Popularity(popular)")
dev.off()
# 8. Predict probability ------------------------------------------------
pValid <- predict(fit_final, valid, type = "prob")[, "Popular"]
pTest  <- predict(fit_final, test,  type = "prob")[, "Popular"]
# 9. AUC ------------------------------------------------
rocV <- auc_rank(valid$popular, pValid); cat("\nValidation AUC =", round(rocV, 4), "\n")
rocT <- auc_rank(test$popular, pTest);  cat("Test AUC =", round(rocT, 4), "\n")
# 10. Threshold selection ------------------------------------------------
thr_grid <- seq(0.3, 0.7, by = 0.01)
j <- sapply(thr_grid, function(t) {
  cm   <- table(Pred = factor(pValid > t, levels = c(FALSE, TRUE)), Truth = valid$popular)
  sens <- cm[2, 2] / sum(cm[, 2])
  spec <- cm[1, 1] / sum(cm[, 1])
  sens + spec - 1
})
thr <- thr_grid[which.max(j)]
cat("\nOptimal threshold from validation set =", thr, "\n")
predClass <- ifelse(pTest > thr, "Popular", "NotPopular")
# ====================== Test‑Set Confusion Matrix (New added) ======================
conf_matrix_test <- table(Actual = test$popular, Predicted = factor(predClass, levels = levels(test$popular)))
cat("\n========== Confusion Matrix (Test Set) ==========\n")
print(conf_matrix_test)
correct_num <- sum(diag(conf_matrix_test))
wrong_num   <- sum(conf_matrix_test) - correct_num
cat("Total correct predictions:", correct_num, "\n")
cat("Total wrong predictions:  ", wrong_num, "\n")
# ==================================================================================
# ====================== New: Random Baseline Simulation (Probability to draw Popular) ======================
set.seed(123)
n_runs <- 1000
# Use Training prevalence for the random baseline; never estimate the baseline from Test labels.
pop_rate <- mean(train$popular == "Popular")
random_acc_vec <- numeric(n_runs)
random_hit_pop_vec <- numeric(n_runs)
for (i in 1:n_runs) {
  # Random prediction based on real popularity proportion
  random_pred <- sample(c("Popular", "NotPopular"),
                        size = nrow(test),
                        prob = c(pop_rate, 1 - pop_rate),
                        replace = TRUE)
  random_acc_vec[i] <- mean(random_pred == test$popular)
  # Probability that random guess picks Popular in this trial
  random_hit_pop_vec[i] <- mean(random_pred == "Popular")
}
random_mean_hit_pop <- mean(random_hit_pop_vec)
random_sd_hit_pop   <- sd(random_hit_pop_vec)
random_mean_acc <- mean(random_acc_vec)
random_sd_acc   <- sd(random_acc_vec)
cart_test_acc   <- mean(predClass == test$popular)
cat("\n========== Random Baseline Simulation (1000 trials) ==========\n")
cat("Theoretical probability of selecting Popular in random draw: ", round(pop_rate,4), "\n")
cat("Average observed probability of selecting Popular over 1000 runs: ", round(random_mean_hit_pop,4), "\n")
cat("SD of observed Popular selection probability: ", round(random_sd_hit_pop,4), "\n")
cat("Random guess average accuracy: ", round(random_mean_acc,4), "\n")
cat("Random guess accuracy standard deviation: ", round(random_sd_acc,4), "\n")
cat("CART model test accuracy: ", round(cart_test_acc,4), "\n")
cat("Accuracy gain over random baseline: ", round(cart_test_acc - random_mean_acc,4), "\n")
# ============================================================================
# ============ 🖼 Figure4: Feature Importance Bar Plot (Top10) ============
imp <- as.data.frame(fit_final$variable.importance)
imp$feature <- rownames(imp)
colnames(imp)[1] <- "importance"
imp <- imp[order(imp$importance, decreasing = TRUE), ]
imp_top10 <- imp[1:min(10, nrow(imp)), ]
p4 <- ggplot(imp_top10, aes(x = importance, y = reorder(feature, importance))) +
  geom_bar(stat = "identity", fill = "#3474b3") +
  labs(x = "Feature Importance (Total Gini Impurity Reduction)", y = "Feature",
       title = "Top 10 Important Features of CART Model") +
  theme_bw()
ggsave("reports/cart/fig4_var_importance.pdf", p4, width = 8, height = 5)
#################################################################
var_imp <- data.frame(
  Feature = names(fit_final$variable.importance),
  Importance = as.numeric(fit_final$variable.importance)
)
# Keep top10 only
var_imp_top10 <- head(var_imp,25)
# Reorder for horizontal bar chart
var_imp_top10$Feature <- reorder(var_imp_top10$Feature, var_imp_top10$Importance)

# Plot
ggplot(var_imp_top10, aes(x = Importance, y = Feature)) +
  geom_col(fill="#4472C4") +
  labs(
    title = "Feature Importance of Pruned CART Model",
    x = "Variable Importance (Total Gini Impurity Reduction)",
    y = "Feature Name"
  ) +
  theme_bw()

# Print full variable importance ranking in console
cat("\n========== Full variable importance ranking for fit_final (for reference only) ==========\n")
print(sort(fit_final$variable.importance, decreasing = TRUE))

# Predict Popular probability for all training samples
pTrain <- predict(fit_final, train, type = "prob")[, "Popular"]
# Append probability column into train dataframe
train_sorted <- train
train_sorted$prob_Popular <- pTrain
# Sort training data by predicted Popular probability in descending order
train_sorted <- train_sorted[order(train_sorted$prob_Popular, decreasing = TRUE), ]


write.csv(data.frame(best_cp=bestCP,threshold=thr,validation_auc=as.numeric(rocV),test_auc=as.numeric(rocT),test_accuracy=mean(predClass==test$popular)),"reports/cart/metrics.csv",row.names=FALSE)
write.csv(data.frame(url=test$url,actual=test$popular,probability=pTest,predicted=predClass),"reports/cart/test_predictions.csv",row.names=FALSE)
saveRDS(fit_final,"reports/cart/model.rds")
capture.output(sessionInfo(),file="reports/cart/sessionInfo.txt")
