dir.create("reports/random_forest",recursive=TRUE,showWarnings=FALSE)
pdf("reports/random_forest/diagnostics.pdf")
# Import libraries
library(data.table)
library(ggplot2)
library(randomForest)


# Set WD and import data
rf_dt <- read.csv("data/processed/m2_cleaned_data.csv", stringsAsFactors = T)
backup_dt <- rf_dt

# Check Missing Values
sum(is.na(rf_dt))

# Trim column names
setnames(rf_dt, trimws(names(rf_dt)))

# Check data type of columns
str(rf_dt)

# Converting column data types
rf_dt$url <- as.character(rf_dt$url)
rf_dt$popular <- factor(rf_dt$popular, levels = c(0, 1), labels = c("Not Popular", "Popular"))
rf_dt$empty_content <- factor(rf_dt$empty_content, levels = c(0, 1), labels = c("No", "Yes"))

# Removing unnecessary columns
exclude_cols <- c(
  "url",
  "temporal_split",
  "shares",
  "timedelta",
  "data_channel_is_lifestyle",
  "data_channel_is_entertainment",
  "data_channel_is_bus",
  "data_channel_is_socmed",
  "data_channel_is_tech",
  "data_channel_is_world",
  "weekday_is_monday",
  "weekday_is_tuesday",
  "weekday_is_wednesday",
  "weekday_is_thursday",
  "weekday_is_friday",
  "weekday_is_saturday",
  "weekday_is_sunday",
  "is_weekend",
  "period"
)

predictors <- setdiff(names(rf_dt), c(exclude_cols, "popular"))

# Separating Dataset into test, validate and train sets

train_dt <- rf_dt[rf_dt$temporal_split == "Training", c("popular", predictors)]
validation_dt <- rf_dt[rf_dt$temporal_split == "Validation",c("popular", predictors)]
test_dt <- rf_dt[rf_dt$temporal_split == "Test",c("popular", predictors)]
test_dt_labelled <- rf_dt[rf_dt$temporal_split == "Test",c("url","popular", predictors)]

# Check sizes
nrow(train_dt) # 23793 entires
nrow(validation_dt) # 7966 entries
nrow(test_dt) # 7884 entries

# Testing Different RSF and B values
# Default B = 500, RSF = 6
mtry_values <- c(3, 6, 12)
ntree_values <- c(100, 300, 500)
validation_results <- data.frame()


for (m in mtry_values) {
  for (n in ntree_values) {
    cat("Training grid: mtry=",m,", ntree=",n,"\n"); flush.console()
    
    set.seed(123)
    
    # Train model
    rf_model <- randomForest(
      popular ~ .,
      data = train_dt,
      ntree = n,
      mtry = m,
      importance = FALSE
    )
    
    # Get OOB error
    oob_error <- tail(rf_model$err.rate[, "OOB"],1)
    
    # Predict validation data
    validation_pred <- predict(rf_model, validation_dt)
    
    # Create confusion matrix
    cm <- table(Actual = validation_dt$popular, Predicted = validation_pred)
    
    # Calcualte Evaluation Metrics
    accuracy <- sum(diag(cm)) / sum(cm)
    false_positive_rate <- cm["Not Popular", "Popular"] / sum(cm["Not Popular", ])
    false_negative_rate <- cm["Popular", "Not Popular"] / sum(cm["Popular", ])
    
    validation_results <- rbind(
      validation_results,
      data.frame(
        mtry = m,
        ntree = n,
        OOB_Error = oob_error,
        Accuracy = accuracy,
        False_Positive_Rate = false_positive_rate,
        False_Negative_Rate = false_negative_rate
      )
    )
  }
}

# Get Results Table
validation_results
setorder(validation_results, -Accuracy)
validation_results

write.csv(validation_results,"reports/random_forest/validation_grid.csv",row.names=FALSE)

# Obtaining best RSF and B values
best_row <- which.max(validation_results$Accuracy)
best_mtry <- validation_results$mtry[best_row]
best_ntree <- validation_results$ntree[best_row]

best_mtry
best_ntree

# Combine train and validation data
train_validation_dt <- rbind(train_dt,validation_dt)

# Train Final Random Forest Model
set.seed(123)

rf_final <- randomForest(
  popular ~ .,
  data = train_validation_dt,
  mtry = best_mtry,
  ntree = best_ntree,
  importance = TRUE
)

rf_final
plot(rf_final)

# Get final OOB error
oob_error_final <- tail(rf_final$err.rate[, "OOB"],1)

# Predict test data
test_pred <- predict(rf_final,test_dt)

# Create confusion matrix
test_cm <- table(Actual = test_dt$popular, Predicted = test_pred)
test_cm

# Calculate Evaluation Metrics
test_accuracy <- sum(diag(test_cm))/sum(test_cm)
test_false_positive_rate <- test_cm["Not Popular", "Popular"] / sum(test_cm["Not Popular", ])
test_false_negative_rate <- test_cm["Popular", "Not Popular"] / sum(test_cm["Popular", ])

final_results <- data.frame(
  mtry = best_mtry,
  ntree = best_ntree,
  OOB_Error = oob_error_final,
  Accuracy = test_accuracy,
  False_Positive_Rate = test_false_positive_rate,
  False_Negative_Rate = test_false_negative_rate
)

final_results
var.impt <- importance(rf_final)
varImpPlot(rf_final, type = 1)


# Obtaining probability values and ranking
prob_matrix <- predict(rf_final, newdata = test_dt, type = "prob")
combined_data <- cbind(test_dt_labelled, prob_matrix)
setorder(combined_data, -Popular)
combined_data
write.csv(validation_results,"reports/random_forest/validation_grid.csv",row.names=FALSE)
write.csv(final_results,"reports/random_forest/metrics.csv",row.names=FALSE)
write.csv(data.frame(url=combined_data$url,actual=combined_data$popular,probability=combined_data$Popular),"reports/random_forest/test_rankings.csv",row.names=FALSE)
write.csv(data.frame(feature=rownames(var.impt),var.impt),"reports/random_forest/importance.csv",row.names=FALSE)
saveRDS(rf_final,"reports/random_forest/model.rds")
capture.output(sessionInfo(),file="reports/random_forest/sessionInfo.txt")
dev.off()
