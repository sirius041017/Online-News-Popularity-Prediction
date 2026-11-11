# M3 Logistic Regression
# Run the complete script in RStudio. Only base R is required.

# 1. Input and output
input_path <- "data/processed/m2_cleaned_data.csv"
output_dir <- file.path(getwd(), "reports/logistic")
classification_threshold <- 0.5

if (!file.exists(input_path)) stop("Data file not found: ", input_path)
if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)
if (!dir.exists(output_dir)) stop("Cannot create output directory: ", output_dir)
out_dir <- output_dir
save_csv <- function(x, filename) {
  write.csv(x, file.path(out_dir, filename), row.names = FALSE)
}

d <- read.csv(input_path,stringsAsFactors=FALSE,check.names=FALSE)
stopifnot(all(c("shares","popular","url","timedelta","channel","weekday","period",
  "temporal_split","n_tokens_content","num_imgs","num_videos","LDA_04") %in% names(d)),
  !anyNA(d),!anyDuplicated(d$url),all(nzchar(trimws(d$url))),
  all(d$temporal_split %in% c("Training","Validation","Test")))
original_names <- names(d)
old_label <- d$popular
d$popular <- as.integer(d$shares>1400)
label_changes <- sum(d$popular!=old_label)

# 2. Predictor coding
# Duplicate encodings are represented once. Shares is used only for the outcome.
channel_dummy <- grep("^data_channel_is_",names(d),value=TRUE)
weekday_dummy <- grep("^weekday_is_",names(d),value=TRUE)
excluded <- c("url","timedelta","shares","popular","temporal_split",
              channel_dummy,weekday_dummy,"is_weekend","period","LDA_04")
features <- setdiff(names(d),excluded)
audit <- data.frame(field=names(d),role="predictor",reason="Included in full ordinary logistic regression")
set_role <- function(fields,role,reason) {
  ix <- match(fields,audit$field);ix<-ix[!is.na(ix)]
  audit$role[ix] <<- role; audit$reason[ix] <<- reason
}
set_role("shares","label_source","popular = as.integer(shares > 1400); excluded from predictors")
set_role("popular","outcome","Recomputed from shares > 1400")
set_role("url","identifier","Article identifier only")
set_role("timedelta","time_metadata","Age at data collection; retained for time-split audit only")
set_role("temporal_split","split","Keep original Training/Validation/Test partitions")
set_role(channel_dummy,"represented_by_channel","Duplicate channel encoding")
set_role(c(weekday_dummy,"is_weekend","period"),"represented_by_weekday","Duplicate weekday/weekend encoding")
set_role("LDA_04","reference_topic","Five topic proportions sum to one; reference topic with intercept")
# Historical share aggregates must be available before publication.
share_features <- grep("^(kw_|self_reference_)",features,value=TRUE)
set_role(share_features,"predictor_availability_assumed",
         "Included; pre-publication availability of historical aggregates must be verified")

channel_map <- c(data_channel_is_lifestyle="Lifestyle",data_channel_is_entertainment="Entertainment",
 data_channel_is_bus="Business",data_channel_is_socmed="Social Media",
 data_channel_is_tech="Technology",data_channel_is_world="World")
for(v in names(channel_map)) stopifnot(all(d[[v]]==as.integer(d$channel==channel_map[[v]])))
for(v in weekday_dummy) stopifnot(all(d[[v]]==as.integer(tolower(d$weekday)==sub("weekday_is_","",v))))
stopifnot(all(d$is_weekend==as.integer(d$weekday %in% c("Saturday","Sunday"))),
 all(d$period==ifelse(d$is_weekend==1,"Weekend","Weekday")),
 max(abs(rowSums(d[paste0("LDA_0",0:4)])-1))<1e-8)

# 3. Preserve the supplied chronological split
train_raw <- d[d$temporal_split=="Training",]
valid_raw <- d[d$temporal_split=="Validation",]
test_raw <- d[d$temporal_split=="Test",]
for(x in list(train_raw,valid_raw,test_raw)) stopifnot(nrow(x)>0,length(unique(x$popular))==2)
# This field is nearly identical to 1 - empty_content in the training data.
if(all(c("n_non_stop_words","empty_content") %in% features) &&
   max(abs(train_raw$n_non_stop_words-(1-train_raw$empty_content)))<1e-6) {
  set_role("n_non_stop_words","represented_by_empty_content",
           "Numerical duplicate of 1-empty_content within 1e-6 in Training")
  features<-setdiff(features,"n_non_stop_words")
}
constant <- features[vapply(train_raw[features],function(x)length(unique(x))<2,TRUE)]
if(length(constant)) {
 set_role(constant,"constant_in_training","No variation to estimate a coefficient")
 features <- setdiff(features,constant)
}
num <- features[vapply(train_raw[features],is.numeric,TRUE)]
factors <- setdiff(features,num)
levs <- lapply(train_raw[factors],function(x)sort(unique(x)))
for(v in factors) {
 ref<-if(v=="channel") "Other / Unclassified" else if(v=="weekday") "Monday" else levs[[v]][1]
 levs[[v]]<-c(ref,setdiff(levs[[v]],ref))
}
# Estimate scaling parameters on training data only.
centers<-vapply(train_raw[num],mean,0.0)
scales<-vapply(train_raw[num],sd,0.0)
stopifnot(all(is.finite(scales)),all(scales>0))
prepare <- function(raw) {
 stopifnot(all(features %in% names(raw)),!anyNA(raw[features]))
 ans<-raw
 for(v in num) {
   stopifnot(is.numeric(raw[[v]]),all(is.finite(raw[[v]])))
   ans[[v]]<-(raw[[v]]-centers[[v]])/scales[[v]]
 }
 for(v in factors) {
   stopifnot(all(raw[[v]] %in% levs[[v]]))
   ans[[v]]<-factor(raw[[v]],levels=levs[[v]])
 }
 ans
}
train<-prepare(train_raw);valid<-prepare(valid_raw);test<-prepare(test_raw)
# 4. Fit the full ordinary logistic regression
f<-reformulate(features,response="popular")
X<-model.matrix(f,train)
q<-qr(X,tol=1e-9)
if(q$rank<ncol(X)) stop("Linearly dependent model columns: ",paste(colnames(X)[q$pivot[(q$rank+1):ncol(X)]],collapse=", "))
fit<-glm(f,data=train,family=binomial(),control=glm.control(maxit=100))
stopifnot(fit$converged,!anyNA(coef(fit)),all(is.finite(coef(fit))))

# 5. Adjusted odds ratios and Wald confidence intervals
# Numeric effects are reported per training SD and per original-unit increment.
s<-coef(summary(fit));terms<-rownames(s)
unit_scale<-rep(1,length(terms)); names(unit_scale)<-terms
unit_scale[intersect(terms,num)]<-scales[intersect(terms,num)]
unit_increment<-rep(1,length(terms));names(unit_increment)<-terms
unit_increment["n_tokens_content"]<-100
b<-s[,1]/unit_scale*unit_increment;se<-s[,2]/unit_scale*unit_increment
or_tab<-data.frame(term=terms,beta_standardized=s[,1],OR_per_SD_or_level=exp(s[,1]),
  SD_OR_CI_low=exp(s[,1]-1.96*s[,2]),SD_OR_CI_high=exp(s[,1]+1.96*s[,2]),
  increment=ifelse(terms=="n_tokens_content","100 content tokens",
    ifelse(terms %in% num,"1 original unit","factor level vs reference / intercept")),
  OR=exp(b),CI_low=exp(b-1.96*se),CI_high=exp(b+1.96*se),p_value=s[,4],row.names=NULL)
save_csv(or_tab,"adjusted_OR_95CI_p.csv")
save_csv(data.frame(field=num,training_mean=centers,training_sd=scales),"numeric_scaling.csv")
save_csv(audit,"all_fields_audit.csv")
save_csv(data.frame(field=features),"included_features.csv")
save_csv(as.data.frame(table(d$temporal_split,d$popular)),"split_counts.csv")
# 6. Collinearity and influence diagnostics
# VIF values apply to individual design-matrix columns.
Z<-X[,-1,drop=FALSE]
vif<-vapply(seq_len(ncol(Z)),function(j) {
 res<-lm.fit(cbind(1,Z[,-j,drop=FALSE]),Z[,j])$residuals
 sum((Z[,j]-mean(Z[,j]))^2)/sum(res^2)
},0.0)
save_csv(data.frame(column=colnames(Z),VIF=vif),"column_VIF.csv")
cd<-cooks.distance(fit)
save_csv(data.frame(url=train_raw$url,cooks_distance=cd,flag=cd>4/nrow(train)),"influence.csv")
capture.output(list(model=summary(fit),original_fields=length(original_names),
  included_fields=length(features),design_predictors=ncol(X)-1,label_changes=label_changes,
  max_column_VIF=max(vif),columns_VIF_above_10=sum(vif>10),influence_flags=sum(cd>4/nrow(train)),
  extreme_fitted=sum(fitted(fit)<1e-6|fitted(fit)>1-1e-6)),
  file=file.path(out_dir,"model_diagnostics.txt"))
# 7. Whole-factor likelihood-ratio tests
# The six leading factors are shown for interpretation; the full model is retained.
term_drop <- drop1(fit,test="Chisq")
term_tests <- data.frame(term=rownames(term_drop),df=term_drop$Df,
  likelihood_ratio=term_drop$LRT,p_value=term_drop[["Pr(>Chi)"]])
term_tests<-term_tests[term_tests$term!="<none>",]
term_tests$BH_adjusted_p<-p.adjust(term_tests$p_value,method="BH")
term_tests<-term_tests[order(term_tests$p_value,term_tests$term),]
term_tests$rank<-seq_len(nrow(term_tests))
save_csv(term_tests,"all_factor_significance.csv")
save_csv(head(term_tests,6),"top6_significant_factors.csv")

# 8. Prediction at a fixed 0.5 cutoff
safe_divide <- function(numerator, denominator) {
  if (denominator == 0) NA_real_ else numerator / denominator
}

evaluate <- function(raw, prepared, split_name) {
  probability <- as.numeric(predict(fit, newdata = prepared, type = "response"))
  stopifnot(length(probability) == nrow(raw), all(is.finite(probability)),
            all(probability >= 0 & probability <= 1))
  predicted <- as.integer(probability >= classification_threshold)
  actual <- raw$popular
  cm <- table(Actual = factor(actual, levels = 0:1,
                             labels = c("Not popular", "Popular")),
              Predicted = factor(predicted, levels = 0:1,
                                 labels = c("Not popular", "Popular")))
  tn <- unname(cm[1, 1]); fp <- unname(cm[1, 2])
  fn <- unname(cm[2, 1]); tp <- unname(cm[2, 2])
  metrics <- data.frame(
    split = split_name, n = length(actual), threshold = classification_threshold,
    accuracy = (tp + tn) / length(actual),
    precision_popular = safe_divide(tp, tp + fp),
    recall_popular = safe_divide(tp, tp + fn),
    specificity = safe_divide(tn, tn + fp),
    F1_popular = safe_divide(2 * tp, 2 * tp + fp + fn)
  )
  prefix <- tolower(split_name)
  counts <- addmargins(cm)
  rownames(counts)[3] <- colnames(counts)[3] <- "Total"
  export_matrix <- function(x, filename) {
    save_csv(data.frame(actual = rownames(x), as.data.frame.matrix(x), check.names = FALSE,
                        row.names = NULL), filename)
  }
  export_matrix(counts, paste0(prefix, "_confusion_counts.csv"))
  # Every percentage uses all articles in this split as its denominator.
  export_matrix(round(100 * counts / sum(cm), 4),
                paste0(prefix, "_confusion_percent.csv"))
  save_csv(data.frame(url = raw$url, actual_popular = actual,
                      probability = probability, predicted_popular = predicted),
           paste0(prefix, "_predictions.csv"))
  list(metrics = metrics, confusion = cm, probability = probability)
}

validation_results <- evaluate(valid_raw, valid, "Validation")
test_results <- evaluate(test_raw, test, "Test")
metrics <- rbind(validation_results$metrics, test_results$metrics)
save_csv(metrics, "classification_metrics.csv")

# 9. Figures
labels <- c(kw_avg_avg = "Keyword mean-share statistic",
            kw_max_avg = "Keyword max-share statistic",
            weekday = "Publication weekday", channel = "Article channel",
            abs_title_subjectivity = "Absolute title subjectivity",
            n_tokens_content = "Content length")
term_label <- function(x) {
  ans <- unname(labels[x])
  ans[is.na(ans)] <- x[is.na(ans)]
  ans
}
leading <- head(term_tests, 6)
png(file.path(out_dir, "factor_significance.png"), width = 1600, height = 850, res = 180)
par(mar = c(5, 16, 3, 2))
barplot(rev(-log10(leading$p_value)), horiz = TRUE,
        names.arg = rev(term_label(leading$term)), las = 1,
        col = "#6C87A6", border = NA, xlab = "Statistical evidence: -log10(p)",
        main = "Six leading factors in the full model")
dev.off()

numeric_leading <- leading$term[leading$term %in% num]
if (length(numeric_leading)) {
  effects <- or_tab[match(numeric_leading, or_tab$term), ]
  y_position <- rev(seq_len(nrow(effects)))
  png(file.path(out_dir, "adjusted_odds_ratios.png"),
      width = 1600, height = 850, res = 180)
  par(mar = c(5, 16, 3, 2))
  plot(effects$OR_per_SD_or_level, y_position,
       xlim = range(c(1, effects$SD_OR_CI_low, effects$SD_OR_CI_high)) * c(0.8, 1.1),
       ylim = c(0.5, nrow(effects) + 0.5), yaxt = "n", ylab = "",
       xlab = "Adjusted odds ratio per one training-set SD",
       pch = 19, col = "#4F6887", main = "Leading numeric associations")
  axis(2, at = y_position, labels = term_label(effects$term), las = 1)
  abline(v = 1, lty = 2, col = "gray60")
  arrows(effects$SD_OR_CI_low, y_position,
         effects$SD_OR_CI_high, y_position,
         angle = 90, code = 3, length = 0.04, col = "#4F6887", lwd = 2)
  dev.off()
}

cm <- test_results$confusion
percent <- 100 * cm / sum(cm)
png(file.path(out_dir, "test_confusion_percent.png"),
    width = 1200, height = 950, res = 160)
par(mar = c(5, 7, 4, 2))
plot(NA, xlim = c(0.5, 2.5), ylim = c(0.5, 2.5), axes = FALSE,
     xlab = "Predicted class", ylab = "", main = "Test confusion matrix at cutoff 0.5")
for (i in 1:2) for (j in 1:2) {
  yy <- 3 - i
  rect(j - 0.5, yy - 0.5, j + 0.5, yy + 0.5,
       col = if (i == j) "#BEDEFF" else "#F2F4F6", border = "white")
  text(j, yy, sprintf("%.2f%%", percent[i, j]), cex = 1.6)
}
axis(1, at = 1:2, labels = colnames(cm), tick = FALSE)
axis(2, at = 2:1, labels = rownames(cm), las = 1, tick = FALSE)
mtext("Actual class", side = 2, line = 5)
mtext("Percentages of all test articles", side = 3, line = 0.5)
dev.off()

png(file.path(out_dir, "training_diagnostics.png"),
    width = 1500, height = 700, res = 150)
par(mfrow = c(1, 2), mar = c(5, 4, 3, 1))
plot(fitted(fit), residuals(fit, type = "deviance"), pch = 20,
     col = adjustcolor("#4F6887", alpha.f = 0.15),
     xlab = "Fitted probability", ylab = "Deviance residual",
     main = "Training residuals")
abline(h = 0, lty = 2, col = "gray60")
plot(cd, type = "h", xlab = "Training observation", ylab = "Cook's distance",
     main = "Influence screening", col = "#4F6887")
abline(h = 4 / nrow(train), lty = 2, col = "red")
dev.off()

# 10. Save the model and reproducibility information
saveRDS(list(model = fit, features = features, numeric_features = num,
             centers = centers, scales = scales, factor_levels = levs,
             classification_threshold = classification_threshold),
        file.path(out_dir, "logistic_model.rds"))
capture.output(sessionInfo(), file = file.path(out_dir, "sessionInfo.txt"))
save_csv(data.frame(input = normalizePath(input_path),
                    md5 = unname(tools::md5sum(input_path))), "input_file.csv")

cat("\nClassification metrics (proportions):\n")
print(metrics, row.names = FALSE)
cat("\nTest confusion matrix: rows = actual, columns = predicted\n")
print(cm)
cat(sprintf("\nTest precision for popular: %.2f%%\n",
            100 * test_results$metrics$precision_popular))
cat(sprintf("Test accuracy: %.2f%%\n", 100 * test_results$metrics$accuracy))
cat("\nSix leading factors:\n")
print(leading, row.names = FALSE)
cat("\nResults saved to:", normalizePath(out_dir), "\n")
