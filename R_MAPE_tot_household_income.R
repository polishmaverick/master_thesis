#####Median imputation#####

######MCAR######
imp_median_data_MCAR <- readRDS("imp_median_data_MCAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MCAR)))
  
  for (j in seq_along(data_samples_MCAR)) {
    actual <- data_samples_MCAR[[j]]$tot_household_income
    predicted <- imp_median_data_MCAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_median_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_median_MCAR

#Saving .rds file
saveRDS(mape_median_MCAR, "mape_median_MCAR.rds")

######MAR######
imp_median_data_MAR <- readRDS("imp_median_data_MAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MAR)))
  
  for (j in seq_along(data_samples_MAR)) {
    actual <- data_samples_MAR[[j]]$tot_household_income
    predicted <- imp_median_data_MAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_median_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_median_MAR

#Saving .rds file
saveRDS(mape_median_MAR, "mape_median_MAR.rds")

######MNAR######
imp_median_data_MNAR <- readRDS("imp_median_data_MNAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MNAR)))
  
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$tot_household_income
    predicted <- imp_median_data_MNAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_median_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_median_MNAR

#Saving .rds file
saveRDS(mape_median_MNAR, "mape_median_MNAR.rds")

#####Hot-deck imputation#####

######MCAR######
imp_hot_deck_data_MCAR <- readRDS("imp_hot_deck_data_MCAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MCAR)))
  
  for (j in seq_along(data_samples_MCAR)) {
    actual <- data_samples_MCAR[[j]]$tot_household_income
    predicted <- imp_hot_deck_data_MCAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_hotdeck_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_hotdeck_MCAR

#Saving .rds file
saveRDS(mape_hotdeck_MCAR, "mape_hotdeck_MCAR.rds")

######MAR######
imp_hot_deck_data_MAR <- readRDS("imp_hot_deck_data_MAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MAR)))
  
  for (j in seq_along(data_samples_MAR)) {
    actual <- data_samples_MAR[[j]]$tot_household_income
    predicted <- imp_hot_deck_data_MAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_hotdeck_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_hotdeck_MAR

#Saving .rds file
saveRDS(mape_hotdeck_MAR, "mape_hotdeck_MAR.rds")

######MNAR######
imp_hot_deck_data_MNAR <- readRDS("imp_hot_deck_data_MNAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MNAR)))
  
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$tot_household_income
    predicted <- imp_hot_deck_data_MNAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_hotdeck_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_hotdeck_MNAR

#Saving .rds file
saveRDS(mape_hotdeck_MNAR, "mape_hotdeck_MNAR.rds")

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

#Saving .rds file
saveRDS(mape_kNN_MCAR, "mape_kNN_MCAR.rds")

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

#Saving .rds file
saveRDS(mape_kNN_MAR, "mape_kNN_MAR.rds")

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

#Saving .rds file
saveRDS(mape_kNN_MNAR, "mape_kNN_MNAR.rds")

#####Regression imputation#####

######MCAR######
imp_reg_data_MCAR <- readRDS("imp_reg_data_MCAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MCAR)))
  
  for (j in seq_along(data_samples_MCAR)) {
    actual <- data_samples_MCAR[[j]]$tot_household_income
    predicted <- imp_reg_data_MCAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_reg_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_reg_MCAR

#Saving .rds file
saveRDS(mape_reg_MCAR, "mape_reg_MCAR.rds")

######MAR######
imp_reg_data_MAR <- readRDS("imp_reg_data_MAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MAR)))
  
  for (j in seq_along(data_samples_MAR)) {
    actual <- data_samples_MAR[[j]]$tot_household_income
    predicted <- imp_reg_data_MAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_reg_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_reg_MAR

#Saving .rds file
saveRDS(mape_reg_MAR, "mape_reg_MAR.rds")

######MNAR######
imp_reg_data_MNAR <- readRDS("imp_reg_data_MNAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MNAR)))
  
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$tot_household_income
    predicted <- imp_reg_data_MNAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_reg_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_reg_MNAR

#Saving .rds file
saveRDS(mape_reg_MNAR, "mape_reg_MNAR.rds")

#####Random forest imputation#####

######MCAR######
imp_rf_data_MCAR <- readRDS("imp_rf_data_MCAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MCAR)))
  
  for (j in seq_along(data_samples_MCAR)) {
    actual <- data_samples_MCAR[[j]]$tot_household_income
    predicted <- imp_rf_data_MCAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_rf_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_rf_MCAR

#Saving .rds file
saveRDS(mape_rf_MCAR, "mape_rf_MCAR.rds")

######MAR######
imp_rf_data_MAR <- readRDS("imp_rf_data_MAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MAR)))
  
  for (j in seq_along(data_samples_MAR)) {
    actual <- data_samples_MAR[[j]]$tot_household_income
    predicted <- imp_rf_data_MAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_rf_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_rf_MAR

#Saving .rds file
saveRDS(mape_rf_MAR, "mape_rf_MAR.rds")

######MNAR######
imp_rf_data_MNAR <- readRDS("imp_rf_data_MNAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MNAR)))
  
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$tot_household_income
    predicted <- imp_rf_data_MNAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_rf_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_rf_MNAR

#Saving .rds file
saveRDS(mape_rf_MNAR, "mape_rf_MNAR.rds")

#####Multiple imputation#####

######MCAR######
imp_mul_data_MCAR <- readRDS("imp_mul_data_MCAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MCAR)))
  
  for (j in seq_along(data_samples_MCAR)) {
    actual <- data_samples_MCAR[[j]]$tot_household_income
    predicted <- imp_mul_data_MCAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_mul_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_mul_MCAR

#Saving .rds file
saveRDS(mape_mul_MCAR, "mape_mul_MCAR.rds")

######MAR######
imp_mul_data_MAR <- readRDS("imp_mul_data_MAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MAR)))
  
  for (j in seq_along(data_samples_MAR)) {
    actual <- data_samples_MAR[[j]]$tot_household_income
    predicted <- imp_mul_data_MAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_mul_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_mul_MAR

#Saving .rds file
saveRDS(mape_mul_MAR, "mape_mul_MAR.rds")

######MNAR######
imp_mul_data_MNAR <- readRDS("imp_mul_data_MNAR.rds")

result <- data.frame(mean_MAPE = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MNAR)))
  
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$tot_household_income
    predicted <- imp_mul_data_MNAR[[i]][[j]]$tot_household_income
    res_i[j] <- mape(actual, predicted)
  }
  
  result[i, "mean_MAPE"] <- round(mean(res_i) * 100, 2)
}

mape_mul_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_MAPE = result)
mape_mul_MNAR

#Saving .rds file
saveRDS(mape_mul_MNAR, "mape_mul_MNAR.rds")