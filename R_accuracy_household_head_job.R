#####Median imputation#####

######MCAR######
imp_median_data_MCAR <- readRDS("imp_median_data_MCAR.rds")

acc_median_MCAR

######MAR######
imp_median_data_MAR <- readRDS("imp_median_data_MAR.rds")

acc_median_MAR

######MNAR######
imp_median_data_MNAR <- readRDS("imp_median_data_MNAR.rds")

acc_median_MNAR

#####Hot-deck imputation#####

######MCAR######
imp_hot_deck_data_MCAR <- readRDS("imp_hot_deck_data_MCAR.rds")

acc_hotdeck_MCAR

######MAR######
imp_hot_deck_data_MAR <- readRDS("imp_hot_deck_data_MAR.rds")

acc_hotdeck_MAR

######MNAR######
imp_hot_deck_data_MNAR <- readRDS("imp_hot_deck_data_MNAR.rds")

acc_hotdeck_MNAR

#####kNN imputation#####

######MCAR######
imp_kNN_data_MCAR <- readRDS("imp_kNN_data_MCAR.rds")

acc_list <- list()

for (i in seq_along(imp_kNN_data_MNAR)) {
  acc_vec <- numeric(length(data_samples_MNAR))
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$household_head_job
    predicted <- imp_kNN_data_MNAR[[i]][[j]]$household_head_job
    acc_vec[j] <- acc(actual, predicted)
  }
  acc_list[[i]] <- round(mean(acc_vec) * 100, 2)
}

acc_kNN_MCAR <- data.frame(na_fraction = seq(5, 70, 5), mean_acc = unlist(acc_list))
acc_kNN_MCAR

######MAR######
imp_kNN_data_MAR <- readRDS("imp_kNN_data_MAR.rds")

acc_list <- list()

for (i in seq_along(imp_kNN_data_MAR)) {
  acc_vec <- numeric(length(data_samples_MAR))
  for (j in seq_along(data_samples_MAR)) {
    actual <- data_samples_MAR[[j]]$household_head_job
    predicted <- imp_kNN_data_MAR[[i]][[j]]$household_head_job
    acc_vec[j] <- acc(actual, predicted)
  }
  acc_list[[i]] <- round(mean(acc_vec) * 100, 2)
}

acc_kNN_MAR <- data.frame(na_fraction = seq(5, 70, 5), mean_acc = unlist(acc_list))
acc_kNN_MAR

######MNAR######
imp_kNN_data_MNAR <- readRDS("imp_kNN_data_MNAR.rds")

acc_list <- list()

for (i in seq_along(imp_kNN_data_MNAR)) {
  acc_vec <- numeric(length(data_samples_MNAR))
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$household_head_job
    predicted <- imp_kNN_data_MNAR[[i]][[j]]$household_head_job
    acc_vec[j] <- acc(actual, predicted)
  }
  acc_list[[i]] <- round(mean(acc_vec) * 100, 2)
}

acc_kNN_MNAR <- data.frame(na_fraction = seq(5, 70, 5), mean_acc = unlist(acc_list))
acc_kNN_MNAR

#####Regression imputation#####

######MCAR######
imp_reg_data_MCAR <- readRDS("imp_reg_data_MCAR.rds")

acc_regression_MCAR

######MAR######
imp_reg_data_MAR <- readRDS("imp_reg_data_MAR.rds")

acc_regression_MAR

######MNAR######
imp_reg_data_MNAR <- readRDS("imp_reg_data_MNAR.rds")

acc_regression_MNAR

#####Random forest imputation#####

######MCAR######
imp_rf_data_MCAR <- readRDS("imp_rf_data_MCAR.rds")

acc_randomforest_MCAR

######MAR######
imp_rf_data_MAR <- readRDS("imp_rf_data_MAR.rds")

acc_randomforest_MAR

######MNAR######
imp_rf_data_MNAR <- readRDS("imp_rf_data_MNAR.rds")

acc_randomforest_MNAR

#####Multiple imputation#####

######MCAR######
imp_mul_data_MCAR <- readRDS("imp_mul_data_MCAR.rds")

acc_multiple_MCAR

######MAR######
imp_mul_data_MAR <- readRDS("imp_mul_data_MAR.rds")

acc_multiple_MAR

######MNAR######
imp_mul_data_MNAR <- readRDS("imp_mul_data_MNAR.rds")

acc_multiple_MNAR