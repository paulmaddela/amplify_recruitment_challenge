# Model inspired by following kernel.
# https://www.kaggle.com/aljaz91/ibm-s-attrition-tackling-class-imbalance-with-gbm#Modeling-(GBM-with-weighting,-SMOTE-and-up-&-down-sampling)

library(rsample)
library(randomForest)
library(caret)
library(e1071)
library(pROC)
library(summarytools)
library(DMwR)

preprocessed_data <- readRDS("output/data/preprocessed_data.rds")
str(preprocessed_data)

preprocessed_data$listing_id <- NULL

# There is over 42% correlation in both the variables, as seen during
# preprocessing correlation checks. Creating a composite feature.
preprocessed_data$composite_score <-
  (preprocessed_data$listing_review_score + preprocessed_data$location_score1) / 2
preprocessed_data$listing_review_score <- NULL
preprocessed_data$location_score1 <- NULL

freq(preprocessed_data$clicked)
freq(preprocessed_data$booked)

# In the whole data, booked rows are only 2.89 % compared to 4.58 % of clicked.
# But, booked is for more important than clicked hence modelling for booked
preprocessed_data$clicked <- NULL

ids <- preprocessed_data[, c("search_id", "timestamp")]

preprocessed_data$search_id <- NULL
preprocessed_data$timestamp <- NULL

preprocessed_data$booked <-
  as.factor(as.character(preprocessed_data$booked))
preprocessed_data$booked <- as.character(preprocessed_data$booked)
preprocessed_data$booked <-
  ifelse(preprocessed_data$booked == 0, "no", "yes")
preprocessed_data$booked <- as.factor(preprocessed_data$booked)
summary(preprocessed_data$booked)

split <-
  rsample::initial_split(preprocessed_data, prop = 0.8, strata = "booked")
split

train <- training(split)
test <- testing(split)

train <- train[1:100000,]
test <- test[1:10000,]

set.seed(1)
folds <- 4
index <- createFolds(factor(train$booked), folds, returnTrain = T)


# Basic GBM
set.seed(1)
ctrl <- trainControl(
  index = index,
  method = "cv",
  summaryFunction = twoClassSummary,
  classProbs = TRUE
)

gbm_fit <- train(
  booked ~ .,
  data = train,
  method = "gbm",
  metric = "ROC",
  trControl = ctrl
)


summary(gbm_fit)
gbm_fit$bestTune
gbm_preds <- predict(gbm_fit, test)
roc_gbm <- roc(as.numeric(test$booked), as.numeric(gbm_preds))
roc_gbm$auc
#saveRDS(gbm_fit,"output/models/gbm_fit.rds")
#gbm_fit <- readRDS("output/models/gbm_fit.rds")

# Weighted GBM
ctrl$seeds <- gbm_fit$control$seeds

model_weights <- ifelse(train$booked == "no",
                        (1 / table(train$booked)[1]) * 0.5,
                        (1 / table(train$booked)[2]) * 0.5)


weighted_fit <- train(
  booked ~ .,
  data = train,
  method = "gbm",
  #                     verbose = FALSE,
  weights = model_weights,
  metric = "ROC",
  trControl = ctrl
)

summary(weighted_fit)
weighted_preds <- predict(weighted_fit, test)
roc_weight <-
  roc(as.numeric(test$booked), as.numeric(weighted_preds))
roc_weight$auc
#saveRDS(weighted_fit,"output/models/weighted_fit.rds")

table(test$booked, weighted_preds)
summary(weighted_fit)
weighted_fit$results


# SMOTE

ctrl$sampling <- "smote"

smote_fit <- train(
  booked ~ .,
  data = train,
  method = "gbm",
  verbose = FALSE,
  metric = "ROC",
  trControl = ctrl
)
summary(smote_fit)
smote_preds <- predict(smote_fit, test)
roc_smote <- roc(as.numeric(test$booked), as.numeric(smote_preds))
roc_smote$auc


# Up

ctrl$sampling <- "up"

up_fit <- train(
  booked ~ .,
  data = train,
  method = "gbm",
  verbose = FALSE,
  metric = "ROC",
  trControl = ctrl
)

up_preds <- predict(up_fit, test)
roc_up <- roc(as.numeric(test$booked), as.numeric(up_preds))
roc_up$auc

# DOWN-sampling

ctrl$sampling <- "down"

down_fit <- train(
  booked ~ .,
  data = train,
  method = "gbm",
  verbose = FALSE,
  metric = "ROC",
  trControl = ctrl
)

down_preds <- predict(down_fit, test)
roc_down <- roc(as.numeric(test$booked), as.numeric(down_preds))
roc_down$auc


plot(
  roc_gbm,
  ylim = c(0, 1),
  print.thres = T,
  print.thres.cex = 0.8,
  main = "ROC curves",
  col = "salmon"
)
plot(
  roc_weight,
  ylim = c(0, 1),
  print.thres = T,
  print.thres.cex = 0.8,
  col = "steelblue",
  add = T
)
plot(
  roc_up,
  ylim = c(0, 1),
  print.thres = T,
  print.thres.cex = 0.8,
  col = "burlywood",
  add = T
)


plot(
  roc_down,
  ylim = c(0, 1),
  print.thres = T,
  print.thres.cex = 0.8,
  col = "darkolivegreen",
  add = T
)
