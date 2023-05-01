#####Median imputation#####

######MCAR######
imp_median_data_MCAR <- readRDS("imp_median_data_MCAR.rds")

mape_median_MCAR

######MAR######
imp_median_data_MAR <- readRDS("imp_median_data_MAR.rds")

mape_median_MAR

######MNAR######
imp_median_data_MNAR <- readRDS("imp_median_data_MNAR.rds")

mape_median_MNAR

#####Hot-deck imputation#####

######MCAR######
imp_hot_deck_data_MCAR <- readRDS("imp_hot_deck_data_MCAR.rds")

mape_hotdeck_MCAR

######MAR######
imp_hot_deck_data_MAR <- readRDS("imp_hot_deck_data_MAR.rds")

mape_hotdeck_MAR

######MNAR######
imp_hot_deck_data_MNAR <- readRDS("imp_hot_deck_data_MNAR.rds")

mape_hotdeck_MNAR

#####kNN imputation#####

######MCAR######
imp_kNN_data_MCAR <- readRDS("imp_kNN_data_MCAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MCAR)))
  
  for (j in seq_along(data_samples_MCAR)) {
    actual <- data_samples_MCAR[[j]]$tot_household_income
    predicted <- imp_kNN_data_MCAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_kNN_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_kNN_MCAR

######MAR######
imp_kNN_data_MAR <- readRDS("imp_kNN_data_MAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MAR)))
  
  for (j in seq_along(data_samples_MAR)) {
    actual <- data_samples_MAR[[j]]$tot_household_income
    predicted <- imp_kNN_data_MAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_kNN_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_kNN_MAR

######MNAR######
imp_kNN_data_MNAR <- readRDS("imp_kNN_data_MNAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MNAR)))
  
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$tot_household_income
    predicted <- imp_kNN_data_MNAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_kNN_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_kNN_MNAR

#####Regression imputation#####

######MCAR######
imp_reg_data_MCAR <- readRDS("imp_reg_data_MCAR.rds")

mape_regression_MCAR

######MAR######
imp_reg_data_MAR <- readRDS("imp_reg_data_MAR.rds")

mape_regression_MAR

######MNAR######
imp_reg_data_MNAR <- readRDS("imp_reg_data_MNAR.rds")

mape_regression_MNAR

#####Random forest imputation#####

######MCAR######
imp_rf_data_MCAR <- readRDS("imp_rf_data_MCAR.rds")

mape_randomforest_MCAR

######MAR######
imp_rf_data_MAR <- readRDS("imp_rf_data_MAR.rds")

mape_randomforest_MAR

######MNAR######
imp_rf_data_MNAR <- readRDS("imp_rf_data_MNAR.rds")

mape_randomforest_MNAR

#####Multiple imputation#####

######MCAR######
imp_mul_data_MCAR <- readRDS("imp_mul_data_MCAR.rds")

mape_multiple_MCAR

######MAR######
imp_mul_data_MAR <- readRDS("imp_mul_data_MAR.rds")

mape_multiple_MAR

######MNAR######
imp_mul_data_MNAR <- readRDS("imp_mul_data_MNAR.rds")

mape_multiple_MNAR