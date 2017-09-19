# Need to have the imputed dataset imported - 
# need to have file in working directory (get from fb message)
data_imputed <- read.csv("imputed_data.csv", header=TRUE)[-1]
#data_imputed2 <- read.csv("kaggle_rfimputed_data", header=TRUE)[-1]


# Necessary packages
require(tree)
require(randomForest)
require(pROC) #package to compute AUC, might have to install first

#CART
class_tree <- tree(as.factor(SeriousDlqin2yrs)~., data_imputed)
plot(class_tree) ; text(class_tree)

reg_tree <- tree(SeriousDlqin2yrs~., data_imputed)
plot(reg_tree) ; text(reg_tree)


#Random Forest

#Test case on subset
#data_imputed_subset <- data_imputed[1:100000,]
#randomForest(as.factor(SeriousDlqin2yrs)~., data_imputed_subset)
#Run on who data set (doesn't take too long)
kaggle_forest <- randomForest(as.factor(SeriousDlqin2yrs)~., data_imputed)

test_data <- read.csv("cs-test.csv", header=TRUE)[,3:12]

forest_preds <- predict(kaggle_forest, data_imputed[,2:11], type="prob")
forest_preds_val <- c()
for (i in 1:length(forest_preds[,2])) {
  if(forest_preds[i,2] > 0.5) { #decision rule 0.7 (9529), 0.9 (9958), 0.8 (9810)
    forest_preds_val[i] <- 1
  } else{
    forest_preds_val[i] <- 0
  }
}

pred1_real0 <- 0
pred0_real1 <- 0
pred0_real0 <- 0
pred1_real1 <- 0

real_vals <- as.vector(data_imputed$SeriousDlqin2yrs)
for (i in 1:length(real_vals)) {
  if (real_vals[i] == 0 & forest_preds_val[i] == 1) {
    pred1_real0 <- pred1_real0 + 1
  }
  else if (real_vals[i] == 1 & forest_preds_val[i] == 0) {
    pred0_real1 <- pred0_real1 + 1
  }
  else if (real_vals[i] == 1 & forest_preds_val[i] == 1) {
    pred1_real1 <- pred1_real1 + 1
  }
  else {
    pred0_real0 <- pred0_real0 + 1
  }
  
}
pred1_real0
pred0_real1
pred1_real0 + pred0_real1
pred0_real0
pred1_real1

#all_zero <- c(1, rep(0,length(real_vals)-1))

roc_obj <- roc(response = data_imputed$SeriousDlqin2yrs, predictor= forest_preds_val, auc=TRUE, plot=TRUE)
roc_obj$auc

#####

forest_preds <- predict(kaggle_forest, test_dataX_copy, type="prob")

Id <- 1:length(forest_preds[,2])
Probability <- forest_preds[,2]
submission_data1 <- data.frame(Id, Probability)
sum(is.na(submission_data1))

write.csv(submission_data1, "submission_data.csv")

forest_preds_val <- c()
for (i in 1:length(forest_preds[,2])) {
  if(forest_preds[i,2] > 0.5) { #decision rule 0.7 (9529), 0.9 (9958), 0.8 (9810)
    forest_preds_val[i] <- 1
  } else{
    forest_preds_val[i] <- 0
  }
}

roc_obj <- roc(response = data_imputed$SeriousDlqin2yrs, predictor= forest_preds_val, auc=TRUE, plot=TRUE)
roc_obj$auc


#####

kaggle_forest2 <- randomForest(SeriousDlqin2yrs~., data_imputed2)

forest_preds2 <- predict(kaggle_forest2, type="prob")
forest_preds_val2 <- c()
for (i in 1:length(forest_preds2[,2])) {
  if(forest_preds2[i,2] > 0.066) { #decision rule 0.7 (9529), 0.9 (9958), 0.8 (9810)
    forest_preds_val2[i] <- 1
  } else{
    forest_preds_val2[i] <- 0
  }
}

pred1_real02 <- 0
pred0_real12 <- 0
pred0_real02 <- 0
pred1_real12 <- 0

real_vals2 <- as.vector(data_imputed2$SeriousDlqin2yrs)
for (i in 1:length(real_vals2)) {
  if (real_vals2[i] == 0 & forest_preds_val2[i] == 1) {
    pred1_real02 <- pred1_real02 + 1
  }
  else if (real_vals2[i] == 1 & forest_preds_val2[i] == 0) {
    pred0_real12 <- pred0_real12 + 1
  }
  else if (real_vals2[i] == 1 & forest_preds_val2[i] == 1) {
    pred1_real12 <- pred1_real12 + 1
  }
  else {
    pred0_real02 <- pred0_real02 + 1
  }
  
}
pred1_real02
pred0_real12
pred1_real02 + pred0_real12
pred0_real02
pred1_real12

#all_zero <- c(1, rep(0,length(real_vals)-1))

roc_obj2 <- roc(response = data_imputed2$SeriousDlqin2yrs, predictor= forest_preds_val2, auc=TRUE, plot=TRUE)
roc_obj2$auc
