#####Median imputation#####

######MCAR######
imp_median_data_MCAR <- readRDS("imp_median_data_MCAR.rds")

######MAR######
imp_median_data_MAR <- readRDS("imp_median_data_MAR.rds")

######MNAR######
imp_median_data_MNAR <- readRDS("imp_median_data_MNAR.rds")

#####Hot-deck imputation#####

######MCAR######
imp_hot_deck_data_MCAR <- readRDS("imp_hot_deck_data_MCAR.rds")

######MAR######
imp_hot_deck_data_MAR <- readRDS("imp_hot_deck_data_MAR.rds")

######MNAR######
imp_hot_deck_data_MNAR <- readRDS("imp_hot_deck_data_MNAR.rds")

#####kNN imputation#####

######MCAR######
imp_kNN_data_MCAR <- readRDS("imp_kNN_data_MCAR.rds")

acc_list <- list()

for (i in seq_along(imp_kNN_data_MCAR)) {
  acc_vec <- numeric(length(data_samples_MCAR))
  for (j in seq_along(data_samples_MCAR)) {
    actual <- data_samples_MCAR[[j]]$household_head_job
    predicted <- imp_kNN_data_MCAR[[i]][[j]]$household_head_job
    acc_vec[j] <- acc(actual, predicted)
  }
  acc_list[[i]] <- round(mean(acc_vec) * 100, 2)
}

acc_kNN_data_MCAR <- data.frame(na_fraction = seq(5, 70, 5), mean_acc = unlist(acc_list))
acc_kNN_data_MCAR

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

acc_kNN_data_MAR <- data.frame(na_fraction = seq(5, 70, 5), mean_acc = unlist(acc_list))
acc_kNN_data_MAR

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

acc_kNN_data_MNAR <- data.frame(na_fraction = seq(5, 70, 5), mean_acc = unlist(acc_list))
acc_kNN_data_MNAR

#####Regression imputation#####

######MCAR######
imp_reg_data_MCAR <- readRDS("imp_reg_data_MCAR.rds")

######MAR######
imp_reg_data_MAR <- readRDS("imp_reg_data_MAR.rds")

######MNAR######
imp_reg_data_MNAR <- readRDS("imp_reg_data_MNAR.rds")

#####Random forest imputation#####

######MCAR######
imp_rf_data_MCAR <- readRDS("imp_rf_data_MCAR.rds")

######MAR######
imp_rf_data_MAR <- readRDS("imp_rf_data_MAR.rds")

######MNAR######
imp_rf_data_MNAR <- readRDS("imp_rf_data_MNAR.rds")

#####Multiple imputation#####

######MCAR######
imp_mul_data_MCAR <- readRDS("imp_mul_data_MCAR.rds")

######MAR######
imp_mul_data_MAR <- readRDS("imp_mul_data_MAR.rds")

######MNAR######
imp_mul_data_MNAR <- readRDS("imp_mul_data_MNAR.rds")

#####xxx#####

acc_median_MCAR
acc_median_MAR
acc_median_MNAR
acc_hot_deck_MCAR
acc_hot_deck_MAR
acc_hot_deck_MNAR
acc_kNN_MCAR
acc_kNN_MAR
acc_kNN_MNAR
acc_reg_MCAR
acc_reg_MAR
acc_reg_MNAR
acc_rf_MCAR
acc_rf_MAR
acc_rf_MNAR
acc_mul_MCAR
acc_mul_MAR
acc_mul_MNAR