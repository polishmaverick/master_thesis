#####Defining function for calculating median - method: bootstrap#####
bootstrap_median <- function(data, R) {
  set.seed(123)
  boot_result <- boot(data, function(data, indices) median(data[indices]), R = 1000)
  return(mean(boot_result$t))
}

result <- data.frame(mean_bootstrap_median = numeric(14))

#####Median imputation#####

######MCAR######
imp_median_data_MCAR <- readRDS("imp_median_data_MCAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_median_data_MCAR)))
  for (j in seq_along(imp_median_data_MCAR)) {
    res_i[j] <- bootstrap_median(imp_median_data_MCAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_median_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_median_MCAR

#Saving .rds file
saveRDS(median_bootstrap_median_MCAR, "median_bootstrap_median_MCAR.rds")

######MAR######
imp_median_data_MAR <- readRDS("imp_median_data_MAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_median_data_MAR)))
  for (j in seq_along(imp_median_data_MAR)) {
    res_i[j] <- bootstrap_median(imp_median_data_MAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_median_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_median_MAR

#Saving .rds file
saveRDS(median_bootstrap_median_MAR, "median_bootstrap_median_MAR.rds")

######MNAR######
imp_median_data_MNAR <- readRDS("imp_median_data_MNAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_median_data_MNAR)))
  for (j in seq_along(imp_median_data_MNAR)) {
    res_i[j] <- bootstrap_median(imp_median_data_MNAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_median_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_median_MNAR

#Saving .rds file
saveRDS(median_bootstrap_median_MNAR, "median_bootstrap_median_MNAR.rds")

#####Hot-deck imputation#####

######MCAR######
imp_hot_deck_data_MCAR <- readRDS("imp_hot_deck_data_MCAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_hot_deck_data_MCAR)))
  for (j in seq_along(imp_hot_deck_data_MCAR)) {
    res_i[j] <- bootstrap_median(imp_hot_deck_data_MCAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_hot_deck_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_hot_deck_MCAR

#Saving .rds file
saveRDS(median_bootstrap_hot_deck_MCAR, "median_bootstrap_hotdeck_MCAR.rds")

######MAR######
imp_hot_deck_data_MAR <- readRDS("imp_hot_deck_data_MAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_hot_deck_data_MAR)))
  for (j in seq_along(imp_hot_deck_data_MAR)) {
    res_i[j] <- bootstrap_median(imp_hot_deck_data_MAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_hot_deck_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_hot_deck_MAR

#Saving .rds file
saveRDS(median_bootstrap_hot_deck_MAR, "median_bootstrap_hotdeck_MAR.rds")

######MNAR######
imp_hot_deck_data_MNAR <- readRDS("imp_hot_deck_data_MNAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_hot_deck_data_MNAR)))
  for (j in seq_along(imp_hot_deck_data_MNAR)) {
    res_i[j] <- bootstrap_median(imp_hot_deck_data_MNAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_hot_deck_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_hot_deck_MNAR

#Saving .rds file
saveRDS(median_bootstrap_hot_deck_MNAR, "median_bootstrap_hotdeck_MNAR.rds")

#####kNN imputation#####

######MCAR######
imp_kNN_data_MCAR <- readRDS("imp_kNN_data_MCAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_kNN_data_MCAR)))
  for (j in seq_along(imp_kNN_data_MCAR)) {
    res_i[j] <- bootstrap_median(imp_kNN_data_MCAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_kNN_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_kNN_MCAR

#Saving .rds file
saveRDS(median_bootstrap_kNN_MCAR, "median_bootstrap_kNN_MCAR.rds")

######MAR######
imp_kNN_data_MAR <- readRDS("imp_kNN_data_MAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_kNN_data_MAR)))
  for (j in seq_along(imp_kNN_data_MAR)) {
    res_i[j] <- bootstrap_median(imp_kNN_data_MAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_kNN_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_kNN_MAR

#Saving .rds file
saveRDS(median_bootstrap_kNN_MAR, "median_bootstrap_kNN_MAR.rds")

######MNAR######
imp_kNN_data_MNAR <- readRDS("imp_kNN_data_MNAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_kNN_data_MNAR)))
  for (j in seq_along(imp_kNN_data_MNAR)) {
    res_i[j] <- bootstrap_median(imp_kNN_data_MNAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_kNN_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_kNN_MNAR

#Saving .rds file
saveRDS(median_bootstrap_kNN_MNAR, "median_bootstrap_kNN_MNAR.rds")

#####Regression imputation#####

######MCAR######
imp_reg_data_MCAR <- readRDS("imp_reg_data_MCAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_reg_data_MCAR)))
  for (j in seq_along(imp_reg_data_MCAR)) {
    res_i[j] <- bootstrap_median(imp_reg_data_MCAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_reg_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_reg_MCAR

#Saving .rds file
saveRDS(median_bootstrap_reg_MCAR, "median_bootstrap_reg_MCAR.rds")

######MAR######
imp_reg_data_MAR <- readRDS("imp_reg_data_MAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_reg_data_MAR)))
  for (j in seq_along(imp_reg_data_MAR)) {
    res_i[j] <- bootstrap_median(imp_reg_data_MAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_reg_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_reg_MAR

#Saving .rds file
saveRDS(median_bootstrap_reg_MAR, "median_bootstrap_reg_MAR.rds")

######MNAR######
imp_reg_data_MNAR <- readRDS("imp_reg_data_MNAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_reg_data_MNAR)))
  for (j in seq_along(imp_reg_data_MNAR)) {
    res_i[j] <- bootstrap_median(imp_reg_data_MNAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_reg_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_reg_MNAR

#Saving .rds file
saveRDS(median_bootstrap_reg_MNAR, "median_bootstrap_reg_MNAR.rds")

#####Random forest imputation#####

######MCAR######
imp_rf_data_MCAR <- readRDS("imp_rf_data_MCAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_rf_data_MCAR)))
  for (j in seq_along(imp_rf_data_MCAR)) {
    res_i[j] <- bootstrap_median(imp_rf_data_MCAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_rf_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_rf_MCAR

#Saving .rds file
saveRDS(median_bootstrap_rf_MCAR, "median_bootstrap_rf_MCAR.rds")

######MAR######
imp_rf_data_MAR <- readRDS("imp_rf_data_MAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_rf_data_MAR)))
  for (j in seq_along(imp_rf_data_MAR)) {
    res_i[j] <- bootstrap_median(imp_rf_data_MAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_rf_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_rf_MAR

#Saving .rds file
saveRDS(median_bootstrap_rf_MAR, "median_bootstrap_rf_MAR.rds")

######MNAR######
imp_rf_data_MNAR <- readRDS("imp_rf_data_MNAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_rf_data_MNAR)))
  for (j in seq_along(imp_rf_data_MNAR)) {
    res_i[j] <- bootstrap_median(imp_rf_data_MNAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_rf_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_rf_MNAR

#Saving .rds file
saveRDS(median_bootstrap_rf_MNAR, "median_bootstrap_rf_MNAR.rds")

#####Multiple imputation#####

######MCAR######
imp_mul_data_MCAR <- readRDS("imp_mul_data_MCAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_mul_data_MCAR)))
  for (j in seq_along(imp_mul_data_MCAR)) {
    res_i[j] <- bootstrap_median(imp_mul_data_MCAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_mul_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_mul_MCAR

#Saving .rds file
saveRDS(median_bootstrap_mul_MCAR, "median_bootstrap_mul_MCAR.rds")

######MAR######
imp_mul_data_MAR <- readRDS("imp_mul_data_MAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_mul_data_MAR)))
  for (j in seq_along(imp_mul_data_MAR)) {
    res_i[j] <- bootstrap_median(imp_mul_data_MAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_mul_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_mul_MAR

#Saving .rds file
saveRDS(median_bootstrap_mul_MAR, "median_bootstrap_mul_MAR.rds")

######MNAR######
imp_mul_data_MNAR <- readRDS("imp_mul_data_MNAR.rds")

for (i in 1:14) {
  res_i <- numeric(length(seq_along(imp_mul_data_MNAR)))
  for (j in seq_along(imp_mul_data_MNAR)) {
    res_i[j] <- bootstrap_median(imp_mul_data_MNAR[[i]][[j]]$tot_household_income)
  }
  result[i, "mean_bootstrap_median"] <- round(mean(res_i), 2)
}

median_bootstrap_mul_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_bootstrap_median = result)
median_bootstrap_mul_MNAR

#Saving .rds file
saveRDS(median_bootstrap_mul_MNAR, "median_bootstrap_mul_MNAR.rds")