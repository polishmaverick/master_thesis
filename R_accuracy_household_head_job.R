#####Median imputation#####

######MCAR######
imp_median_data_MCAR <- readRDS("imp_median_data_MCAR.rds")

result <- data.frame(mean_acc = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MCAR)))
  
  for (j in seq_along(data_samples_MCAR)) {
    actual <- data_samples_MCAR[[j]]$tot_household_income
    predicted <- imp_median_data_MCAR[[i]][[j]]$tot_household_income
    res_i[j] <- accuracy(actual, predicted)
  }
  
  result[i, "mean_acc"] <- round(mean(res_i) * 100, 2)
}

acc_median_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_acc = result)
acc_median_MCAR

saveRDS(acc_median_MCAR, "acc_median_MCAR.rds")

######MAR######
imp_median_data_MAR <- readRDS("imp_median_data_MAR.rds")

result <- data.frame(mean_acc = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MAR)))
  
  for (j in seq_along(data_samples_MAR)) {
    actual <- data_samples_MAR[[j]]$tot_household_income
    predicted <- imp_median_data_MAR[[i]][[j]]$tot_household_income
    res_i[j] <- accuracy(actual, predicted)
  }
  
  result[i, "mean_acc"] <- round(mean(res_i) * 100, 2)
}

acc_median_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_acc = result)
acc_median_MAR

saveRDS(acc_median_MAR, "acc_median_MAR.rds")

######MNAR######
imp_median_data_MNAR <- readRDS("imp_median_data_MNAR.rds")

result <- data.frame(mean_acc = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MNAR)))
  
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$tot_household_income
    predicted <- imp_median_data_MNAR[[i]][[j]]$tot_household_income
    res_i[j] <- accuracy(actual, predicted)
  }
  
  result[i, "mean_acc"] <- round(mean(res_i) * 100, 2)
}

acc_median_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_acc = result)
acc_median_MNAR

saveRDS(acc_median_MNAR, "acc_median_MNAR.rds")

#####Hot-deck imputation#####

######MCAR######
imp_hot_deck_data_MCAR <- readRDS("imp_hot_deck_data_MCAR.rds")

result <- data.frame(mean_acc = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MCAR)))
  
  for (j in seq_along(data_samples_MCAR)) {
    actual <- data_samples_MCAR[[j]]$tot_household_income
    predicted <- imp_hot_deck_data_MCAR[[i]][[j]]$tot_household_income
    res_i[j] <- accuracy(actual, predicted)
  }
  
  result[i, "mean_acc"] <- round(mean(res_i) * 100, 2)
}

acc_hotdeck_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_acc = result)
acc_hotdeck_MCAR

saveRDS(acc_hotdeck_MCAR, "acc_hotdeck_MCAR.rds")

######MAR######
imp_hot_deck_data_MAR <- readRDS("imp_hot_deck_data_MAR.rds")

result <- data.frame(mean_acc = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MAR)))
  
  for (j in seq_along(data_samples_MAR)) {
    actual <- data_samples_MAR[[j]]$tot_household_income
    predicted <- imp_hot_deck_data_MAR[[i]][[j]]$tot_household_income
    res_i[j] <- accuracy(actual, predicted)
  }
  
  result[i, "mean_acc"] <- round(mean(res_i) * 100, 2)
}

acc_hotdeck_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_acc = result)
acc_hotdeck_MAR

saveRDS(acc_hotdeck_MAR, "acc_hotdeck_MAR.rds")

######MNAR######
imp_hot_deck_data_MNAR <- readRDS("imp_hot_deck_data_MNAR.rds")

result <- data.frame(mean_acc = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MNAR)))
  
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$tot_household_income
    predicted <- imp_hot_deck_data_MNAR[[i]][[j]]$tot_household_income
    res_i[j] <- accuracy(actual, predicted)
  }
  
  result[i, "mean_acc"] <- round(mean(res_i) * 100, 2)
}

acc_hotdeck_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_acc = result)
acc_hotdeck_MNAR

saveRDS(acc_hotdeck_MNAR, "acc_hotdeck_MNAR.rds")

#####kNN imputation#####

######MCAR######
imp_kNN_data_MCAR <- readRDS("imp_kNN_data_MCAR.rds")

result <- data.frame(mean_acc = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MCAR)))
  
  for (j in seq_along(data_samples_MCAR)) {
    actual <- data_samples_MCAR[[j]]$tot_household_income
    predicted <- imp_kNN_data_MCAR[[i]][[j]]$tot_household_income
    res_i[j] <- accuracy(actual, predicted)
  }
  
  result[i, "mean_acc"] <- round(mean(res_i) * 100, 2)
}

acc_kNN_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_acc = result)
acc_kNN_MCAR

saveRDS(acc_kNN_MCAR, "acc_kNN_MCAR.rds")

######MAR######
imp_kNN_data_MAR <- readRDS("imp_kNN_data_MAR.rds")

result <- data.frame(mean_acc = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MAR)))
  
  for (j in seq_along(data_samples_MAR)) {
    actual <- data_samples_MAR[[j]]$tot_household_income
    predicted <- imp_kNN_data_MAR[[i]][[j]]$tot_household_income
    res_i[j] <- accuracy(actual, predicted)
  }
  
  result[i, "mean_acc"] <- round(mean(res_i) * 100, 2)
}

acc_kNN_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_acc = result)
acc_kNN_MAR

saveRDS(acc_kNN_MAR, "acc_kNN_MAR.rds")

######MNAR######
imp_kNN_data_MNAR <- readRDS("imp_kNN_data_MNAR.rds")

result <- data.frame(mean_acc = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MNAR)))
  
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$tot_household_income
    predicted <- imp_kNN_data_MNAR[[i]][[j]]$tot_household_income
    res_i[j] <- accuracy(actual, predicted)
  }
  
  result[i, "mean_acc"] <- round(mean(res_i) * 100, 2)
}

acc_kNN_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_acc = result)
acc_kNN_MNAR

saveRDS(acc_kNN_MNAR, "acc_kNN_MNAR.rds")

#####Regression imputation#####

######MCAR######
imp_lm_data_MNAR <- readRDS("imp_lm_data_MNAR.rds")

result <- data.frame(mean_acc = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MNAR)))
  
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$tot_household_income
    predicted <- imp_lm_data_MNAR[[i]][[j]]$tot_household_income
    res_i[j] <- accuracy(actual, predicted)
  }
  
  result[i, "mean_acc"] <- round(mean(res_i) * 100, 2)
}

acc_reg_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_acc = result)
acc_reg_MNAR

saveRDS(acc_reg_MNAR, "acc_reg_MNAR.rds")

######MAR######
imp_lm_data_MAR <- readRDS("imp_lm_data_MAR.rds")

result <- data.frame(mean_acc = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MAR)))
  
  for (j in seq_along(data_samples_MAR)) {
    actual <- data_samples_MAR[[j]]$tot_household_income
    predicted <- imp_lm_data_MAR[[i]][[j]]$tot_household_income
    res_i[j] <- accuracy(actual, predicted)
  }
  
  result[i, "mean_acc"] <- round(mean(res_i) * 100, 2)
}

acc_reg_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_acc = result)
acc_reg_MAR

saveRDS(acc_reg_MAR, "acc_reg_MAR.rds")

######MNAR######
imp_lm_data_MNAR <- readRDS("imp_lm_data_MNAR.rds")

result <- data.frame(mean_acc = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MNAR)))
  
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$tot_household_income
    predicted <- imp_lm_data_MNAR[[i]][[j]]$tot_household_income
    res_i[j] <- accuracy(actual, predicted)
  }
  
  result[i, "mean_acc"] <- round(mean(res_i) * 100, 2)
}

acc_reg_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_acc = result)
acc_reg_MNAR

saveRDS(acc_reg_MNAR, "acc_reg_MNAR.rds")

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