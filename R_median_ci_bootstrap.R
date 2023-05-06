#####Defining function for calculating median confidence interval - method: bootstrap#####
bootstrap_median_ci <- function(data, R) {
  set.seed(123)
  boot_result <- boot(data, function(data, indices) median(data[indices]), R = 1000)
  return(boot_result)
}

result <- data.frame(lower_ci = numeric(14), upper_ci = numeric(14), coverage = integer(14))
population_median <- median(data$tot_household_income)

#####Median imputation#####

######MCAR######
imp_median_data_MCAR <- readRDS("imp_median_data_MCAR.rds")

for (i in 1:14) {
  coverage <- 0
  for (j in seq_along(imp_median_data_MCAR[[i]])) {
    boot_result <- bootstrap_median_ci(imp_median_data_MCAR[[i]][[j]]$tot_household_income, R = 1000)
    boot_ci <- boot.ci(boot_result, conf = 0.95, type = "perc")
    lower_ci <- round(boot_ci$percent[4], 2)
    upper_ci <- round(boot_ci$percent[5], 2)
    
    if (lower_ci <= population_median && upper_ci >= population_median) {
      coverage <- coverage + 1
    }
  }
  
  result[i, "lower_ci"] <- lower_ci
  result[i, "upper_ci"] <- upper_ci
  result[i, "coverage"] <- coverage
}

median_ci_bootstrap_median_MCAR_ci <- data.frame(na_frac = seq(5, 70, 5), lower_ci = result$lower_ci, upper_ci = result$upper_ci, coverage = result$coverage)
median_ci_bootstrap_median_MCAR_ci

#Saving .rds file
saveRDS(median_ci_bootstrap_median_MCAR_ci, "median_ci_bootstrap_median_MCAR_ci.rds")

######MAR######
imp_median_data_MAR_ci <- readRDS("imp_median_data_MAR_ci.rds")

for (i in 1:14) {
  coverage <- 0
  for (j in seq_along(imp_median_data_MAR[[i]])) {
    boot_result <- bootstrap_median_ci(imp_median_data_MAR[[i]][[j]]$tot_household_income, R = 1000)
    boot_ci <- boot.ci(boot_result, conf = 0.95, type = "perc")
    lower_ci <- round(boot_ci$percent[4], 2)
    upper_ci <- round(boot_ci$percent[5], 2)
    
    if (lower_ci <= population_median && upper_ci >= population_median) {
      coverage <- coverage + 1
    }
  }
  
  result[i, "lower_ci"] <- lower_ci
  result[i, "upper_ci"] <- upper_ci
  result[i, "coverage"] <- coverage
}

median_ci_bootstrap_median_MAR_ci <- data.frame(na_frac = seq(5, 70, 5), lower_ci = result$lower_ci, upper_ci = result$upper_ci, coverage = result$coverage)
median_ci_bootstrap_median_MAR_ci

#Saving .rds file
saveRDS(median_ci_bootstrap_median_MAR_ci, "median_ci_bootstrap_median_MAR_ci.rds")

######MNAR######
imp_median_data_MNAR <- readRDS("imp_median_data_MNAR.rds")

for (i in 1:14) {
  coverage <- 0
  for (j in seq_along(imp_median_data_MNAR[[i]])) {
    boot_result <- bootstrap_median_ci(imp_median_data_MNAR[[i]][[j]]$tot_household_income, R = 1000)
    boot_ci <- boot.ci(boot_result, conf = 0.95, type = "perc")
    lower_ci <- round(boot_ci$percent[4], 2)
    upper_ci <- round(boot_ci$percent[5], 2)
    
    if (lower_ci <= population_median && upper_ci >= population_median) {
      coverage <- coverage + 1
    }
  }
  
  result[i, "lower_ci"] <- lower_ci
  result[i, "upper_ci"] <- upper_ci
  result[i, "coverage"] <- coverage
}

median_ci_bootstrap_median_MNAR_ci <- data.frame(na_frac = seq(5, 70, 5), lower_ci = result$lower_ci, upper_ci = result$upper_ci, coverage = result$coverage)
median_ci_bootstrap_median_MNAR_ci

#Saving .rds file
saveRDS(median_ci_bootstrap_median_MNAR_ci, "median_ci_bootstrap_median_MNAR_ci.rds")

#####Hot-deck imputation#####

######MCAR######
imp_hot_deck_data_MCAR <- readRDS("imp_hot_deck_data_MCAR.rds")

for (i in 1:14) {
  coverage <- 0
  for (j in seq_along(imp_hot_deck_data_MCAR[[i]])) {
    boot_result <- bootstrap_median_ci(imp_hot_deck_data_MCAR[[i]][[j]]$tot_household_income, R = 1000)
    boot_ci <- boot.ci(boot_result, conf = 0.95, type = "perc")
    lower_ci <- round(boot_ci$percent[4], 2)
    upper_ci <- round(boot_ci$percent[5], 2)
    
    if (lower_ci <= population_median && upper_ci >= population_median) {
      coverage <- coverage + 1
    }
  }
  
  result[i, "lower_ci"] <- lower_ci
  result[i, "upper_ci"] <- upper_ci
  result[i, "coverage"] <- coverage
}

median_ci_bootstrap_hotdeck_MCAR_ci <- data.frame(na_frac = seq(5, 70, 5), lower_ci = result$lower_ci, upper_ci = result$upper_ci, coverage = result$coverage)
median_ci_bootstrap_hotdeck_MCAR_ci

#Saving .rds file
saveRDS(median_ci_bootstrap_hotdeck_MCAR_ci, "median_ci_bootstrap_hotdeck_MCAR_ci.rds")

######MAR######
imp_hot_deck_data_MAR <- readRDS("imp_hot_deck_data_MAR.rds")

for (i in 1:14) {
  coverage <- 0
  for (j in seq_along(imp_hot_deck_data_MAR[[i]])) {
    boot_result <- bootstrap_median_ci(imp_hot_deck_data_MAR[[i]][[j]]$tot_household_income, R = 1000)
    boot_ci <- boot.ci(boot_result, conf = 0.95, type = "perc")
    lower_ci <- round(boot_ci$percent[4], 2)
    upper_ci <- round(boot_ci$percent[5], 2)
    
    if (lower_ci <= population_median && upper_ci >= population_median) {
      coverage <- coverage + 1
    }
  }
  
  result[i, "lower_ci"] <- lower_ci
  result[i, "upper_ci"] <- upper_ci
  result[i, "coverage"] <- coverage
}

median_ci_bootstrap_hotdeck_MAR_ci <- data.frame(na_frac = seq(5, 70, 5), lower_ci = result$lower_ci, upper_ci = result$upper_ci, coverage = result$coverage)
median_ci_bootstrap_hotdeck_MAR_ci

#Saving .rds file
saveRDS(median_ci_bootstrap_hotdeck_MAR_ci, "median_ci_bootstrap_hotdeck_MAR_ci.rds")

######MNAR######
imp_hot_deck_data_MNAR <- readRDS("imp_hot_deck_data_MNAR.rds")

for (i in 1:14) {
  coverage <- 0
  for (j in seq_along(imp_hot_deck_data_MNAR[[i]])) {
    boot_result <- bootstrap_median_ci(imp_hot_deck_data_MNAR[[i]][[j]]$tot_household_income, R = 1000)
    boot_ci <- boot.ci(boot_result, conf = 0.95, type = "perc")
    lower_ci <- round(boot_ci$percent[4], 2)
    upper_ci <- round(boot_ci$percent[5], 2)
    
    if (lower_ci <= population_median && upper_ci >= population_median) {
      coverage <- coverage + 1
    }
  }
  
  result[i, "lower_ci"] <- lower_ci
  result[i, "upper_ci"] <- upper_ci
  result[i, "coverage"] <- coverage
}

median_ci_bootstrap_hotdeck_MNAR_ci <- data.frame(na_frac = seq(5, 70, 5), lower_ci = result$lower_ci, upper_ci = result$upper_ci, coverage = result$coverage)
median_ci_bootstrap_hotdeck_MNAR_ci

#Saving .rds file
saveRDS(median_ci_bootstrap_hotdeck_MNAR_ci, "median_ci_bootstrap_hotdeck_MNAR_ci.rds")

#####kNN imputation#####

######MCAR######
imp_kNN_data_MCAR <- readRDS("imp_kNN_data_MCAR.rds")

for (i in 1:14) {
  coverage <- 0
  for (j in seq_along(imp_kNN_data_MCAR[[i]])) {
    boot_result <- bootstrap_median_ci(imp_kNN_data_MCAR[[i]][[j]]$tot_household_income, R = 1000)
    boot_ci <- boot.ci(boot_result, conf = 0.95, type = "perc")
    lower_ci <- round(boot_ci$percent[4], 2)
    upper_ci <- round(boot_ci$percent[5], 2)
    
    if (lower_ci <= population_median && upper_ci >= population_median) {
      coverage <- coverage + 1
    }
  }
  
  result[i, "lower_ci"] <- lower_ci
  result[i, "upper_ci"] <- upper_ci
  result[i, "coverage"] <- coverage
}

median_ci_bootstrap_kNN_MCAR_ci <- data.frame(na_frac = seq(5, 70, 5), lower_ci = result$lower_ci, upper_ci = result$upper_ci, coverage = result$coverage)
median_ci_bootstrap_kNN_MCAR_ci

#Saving .rds file
saveRDS(median_ci_bootstrap_kNN_MCAR_ci, "median_ci_bootstrap_kNN_MCAR_ci.rds")

######MAR######
imp_kNN_data_MAR <- readRDS("imp_kNN_data_MAR.rds")

for (i in 1:14) {
  coverage <- 0
  for (j in seq_along(imp_kNN_data_MAR[[i]])) {
    boot_result <- bootstrap_median_ci(imp_kNN_data_MAR[[i]][[j]]$tot_household_income, R = 1000)
    boot_ci <- boot.ci(boot_result, conf = 0.95, type = "perc")
    lower_ci <- round(boot_ci$percent[4], 2)
    upper_ci <- round(boot_ci$percent[5], 2)
    
    if (lower_ci <= population_median && upper_ci >= population_median) {
      coverage <- coverage + 1
    }
  }
  
  result[i, "lower_ci"] <- lower_ci
  result[i, "upper_ci"] <- upper_ci
  result[i, "coverage"] <- coverage
}

median_ci_bootstrap_kNN_MAR_ci <- data.frame(na_frac = seq(5, 70, 5), lower_ci = result$lower_ci, upper_ci = result$upper_ci, coverage = result$coverage)
median_ci_bootstrap_kNN_MAR_ci

#Saving .rds file
saveRDS(median_ci_bootstrap_kNN_MAR_ci, "median_ci_bootstrap_kNN_MAR_ci.rds")

######MNAR######
imp_kNN_data_MNAR <- readRDS("imp_kNN_data_MNAR.rds")

for (i in 1:14) {
  coverage <- 0
  for (j in seq_along(imp_kNN_data_MNAR[[i]])) {
    boot_result <- bootstrap_median_ci(imp_kNN_data_MNAR[[i]][[j]]$tot_household_income, R = 1000)
    boot_ci <- boot.ci(boot_result, conf = 0.95, type = "perc")
    lower_ci <- round(boot_ci$percent[4], 2)
    upper_ci <- round(boot_ci$percent[5], 2)
    
    if (lower_ci <= population_median && upper_ci >= population_median) {
      coverage <- coverage + 1
    }
  }
  
  result[i, "lower_ci"] <- lower_ci
  result[i, "upper_ci"] <- upper_ci
  result[i, "coverage"] <- coverage
}

median_ci_bootstrap_kNN_MNAR_ci <- data.frame(na_frac = seq(5, 70, 5), lower_ci = result$lower_ci, upper_ci = result$upper_ci, coverage = result$coverage)
median_ci_bootstrap_kNN_MNAR_ci

#Saving .rds file
saveRDS(median_ci_bootstrap_kNN_MNAR_ci, "median_ci_bootstrap_kNN_MNAR_ci.rds")

#####Regression imputation#####

######MCAR######
imp_reg_data_MCAR <- readRDS("imp_reg_data_MCAR.rds")

for (i in 1:14) {
  coverage <- 0
  for (j in seq_along(imp_reg_data_MCAR[[i]])) {
    boot_result <- bootstrap_median_ci(imp_reg_data_MCAR[[i]][[j]]$tot_household_income, R = 1000)
    boot_ci <- boot.ci(boot_result, conf = 0.95, type = "perc")
    lower_ci <- round(boot_ci$percent[4], 2)
    upper_ci <- round(boot_ci$percent[5], 2)
    
    if (lower_ci <= population_median && upper_ci >= population_median) {
      coverage <- coverage + 1
    }
  }
  
  result[i, "lower_ci"] <- lower_ci
  result[i, "upper_ci"] <- upper_ci
  result[i, "coverage"] <- coverage
}

median_ci_bootstrap_reg_MCAR_ci <- data.frame(na_frac = seq(5, 70, 5), lower_ci = result$lower_ci, upper_ci = result$upper_ci, coverage = result$coverage)
median_ci_bootstrap_reg_MCAR_ci

#Saving .rds file
saveRDS(median_ci_bootstrap_reg_MCAR_ci, "median_ci_bootstrap_reg_MCAR_ci.rds")

######MAR######
imp_reg_data_MAR <- readRDS("imp_reg_data_MAR.rds")

for (i in 1:14) {
  coverage <- 0
  for (j in seq_along(imp_reg_data_MAR[[i]])) {
    boot_result <- bootstrap_median_ci(imp_reg_data_MAR[[i]][[j]]$tot_household_income, R = 1000)
    boot_ci <- boot.ci(boot_result, conf = 0.95, type = "perc")
    lower_ci <- round(boot_ci$percent[4], 2)
    upper_ci <- round(boot_ci$percent[5], 2)
    
    if (lower_ci <= population_median && upper_ci >= population_median) {
      coverage <- coverage + 1
    }
  }
  
  result[i, "lower_ci"] <- lower_ci
  result[i, "upper_ci"] <- upper_ci
  result[i, "coverage"] <- coverage
}

median_ci_bootstrap_reg_MAR_ci <- data.frame(na_frac = seq(5, 70, 5), lower_ci = result$lower_ci, upper_ci = result$upper_ci, coverage = result$coverage)
median_ci_bootstrap_reg_MAR_ci

#Saving .rds file
saveRDS(median_ci_bootstrap_reg_MAR_ci, "median_ci_bootstrap_reg_MAR_ci.rds")

######MNAR######
imp_reg_data_MNAR <- readRDS("imp_reg_data_MNAR.rds")

for (i in 1:14) {
  coverage <- 0
  for (j in seq_along(imp_reg_data_MNAR[[i]])) {
    boot_result <- bootstrap_median_ci(imp_reg_data_MNAR[[i]][[j]]$tot_household_income, R = 1000)
    boot_ci <- boot.ci(boot_result, conf = 0.95, type = "perc")
    lower_ci <- round(boot_ci$percent[4], 2)
    upper_ci <- round(boot_ci$percent[5], 2)
    
    if (lower_ci <= population_median && upper_ci >= population_median) {
      coverage <- coverage + 1
    }
  }
  
  result[i, "lower_ci"] <- lower_ci
  result[i, "upper_ci"] <- upper_ci
  result[i, "coverage"] <- coverage
}

median_ci_bootstrap_reg_MNAR_ci <- data.frame(na_frac = seq(5, 70, 5), lower_ci = result$lower_ci, upper_ci = result$upper_ci, coverage = result$coverage)
median_ci_bootstrap_reg_MNAR_ci

#Saving .rds file
saveRDS(median_ci_bootstrap_reg_MNAR_ci, "median_ci_bootstrap_reg_MNAR_ci.rds")

#####Random forest imputation#####

######MCAR######


######MAR######


######MNAR######


#####Multiple imputation#####

######MCAR######


######MAR######


######MNAR######