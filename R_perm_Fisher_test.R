#####Hot-deck imputation#####

######MCAR######
imp_hot_deck_data_MCAR <- readRDS("imp_hot_deck_data_MCAR.rds")

results <- data.frame(na_frac = seq(5, 70, by = 5), value = integer(length(seq(5, 70, by = 5))))

set.seed(123)
for (i in 1:length(imp_hot_deck_data_MCAR)) {
  value <- 0
  for (j in 1:100) {
    contingency_table <- table(imp_hot_deck_data_MCAR[[i]][[j]]$household_head_job, 
                               imp_hot_deck_data_MCAR[[i]][[j]]$house_electricity)
    test_result <- fisher.test(contingency_table)
    if (test_result$p.value < 0.05) {
      value <- value + 1
    }
  }
  results$value[results$na_frac == i*5] <- value
}

#Saving .rds file
saveRDS(results, "fisher_hotdeck_MCAR.rds")

######MAR######
imp_hot_deck_data_MAR <- readRDS("imp_hot_deck_data_MAR.rds")

results <- data.frame(na_frac = seq(5, 70, by = 5), value = integer(length(seq(5, 70, by = 5))))

set.seed(123)
for (i in 1:length(imp_hot_deck_data_MAR)) {
  value <- 0
  for (j in 1:100) {
    contingency_table <- table(imp_hot_deck_data_MAR[[i]][[j]]$household_head_job, 
                               imp_hot_deck_data_MAR[[i]][[j]]$house_electricity)
    test_result <- fisher.test(contingency_table)
    if (test_result$p.value < 0.05) {
      value <- value + 1
    }
  }
  results$value[results$na_frac == i*5] <- value
}

#Saving .rds file
saveRDS(results, "fisher_hotdeck_MAR.rds")

######MNAR######
imp_hot_deck_data_MNAR <- readRDS("imp_hot_deck_data_MNAR.rds")

results <- data.frame(na_frac = seq(5, 70, by = 5), value = integer(length(seq(5, 70, by = 5))))

set.seed(123)
for (i in 1:length(imp_hot_deck_data_MNAR)) {
  value <- 0
  for (j in 1:100) {
    contingency_table <- table(imp_hot_deck_data_MNAR[[i]][[j]]$household_head_job, 
                               imp_hot_deck_data_MNAR[[i]][[j]]$house_electricity)
    test_result <- fisher.test(contingency_table)
    if (test_result$p.value < 0.05) {
      value <- value + 1
    }
  }
  results$value[results$na_frac == i*5] <- value
}

#Saving .rds file
saveRDS(results, "fisher_hotdeck_MNAR.rds")

#####kNN imputation#####

######MCAR######
imp_kNN_data_MCAR <- readRDS("imp_kNN_data_MCAR.rds")

results <- data.frame(na_frac = seq(5, 70, by = 5), value = integer(length(seq(5, 70, by = 5))))

set.seed(123)
for (i in 1:length(imp_kNN_data_MCAR)) {
  value <- 0
  for (j in 1:100) {
    contingency_table <- table(imp_kNN_data_MCAR[[i]][[j]]$household_head_job, 
                               imp_kNN_data_MCAR[[i]][[j]]$house_electricity)
    test_result <- fisher.test(contingency_table)
    if (test_result$p.value < 0.05) {
      value <- value + 1
    }
  }
  results$value[results$na_frac == i*5] <- value
}

#Saving .rds file
saveRDS(results, "fisher_kNN_MCAR.rds")

######MAR######
imp_kNN_data_MAR <- readRDS("imp_kNN_data_MAR.rds")

results <- data.frame(na_frac = seq(5, 70, by = 5), value = integer(length(seq(5, 70, by = 5))))

set.seed(123)
for (i in 1:length(imp_kNN_data_MAR)) {
  value <- 0
  for (j in 1:100) {
    contingency_table <- table(imp_kNN_data_MAR[[i]][[j]]$household_head_job, 
                               imp_kNN_data_MAR[[i]][[j]]$house_electricity)
    test_result <- fisher.test(contingency_table)
    if (test_result$p.value < 0.05) {
      value <- value + 1
    }
  }
  results$value[results$na_frac == i*5] <- value
}

#Saving .rds file
saveRDS(results, "fisher_kNN_MAR.rds")

######MNAR######
imp_kNN_data_MNAR <- readRDS("imp_kNN_data_MNAR.rds")

results <- data.frame(na_frac = seq(5, 70, by = 5), value = integer(length(seq(5, 70, by = 5))))

set.seed(123)
for (i in 1:length(imp_kNN_data_MNAR)) {
  value <- 0
  for (j in 1:100) {
    contingency_table <- table(imp_kNN_data_MNAR[[i]][[j]]$household_head_job, 
                               imp_kNN_data_MNAR[[i]][[j]]$house_electricity)
    test_result <- fisher.test(contingency_table)
    if (test_result$p.value < 0.05) {
      value <- value + 1
    }
  }
  results$value[results$na_frac == i*5] <- value
}


#Saving .rds file
saveRDS(results, "fisher_kNN_MNAR.rds")

#####Regression imputation#####

######MCAR######
imp_reg_data_MCAR <- readRDS("imp_reg_data_MCAR.rds")

results <- data.frame(na_frac = seq(5, 70, by = 5), value = integer(length(seq(5, 70, by = 5))))

set.seed(123)
for (i in 1:length(imp_reg_data_MCAR)) {
  value <- 0
  for (j in 1:100) {
    contingency_table <- table(imp_reg_data_MCAR[[i]][[j]]$household_head_job, 
                               imp_reg_data_MCAR[[i]][[j]]$house_electricity)
    test_result <- fisher.test(contingency_table)
    if (test_result$p.value < 0.05) {
      value <- value + 1
    }
  }
  results$value[results$na_frac == i*5] <- value
}

#Saving .rds file
saveRDS(results, "fisher_reg_MCAR.rds")

######MAR######
imp_reg_data_MAR <- readRDS("imp_reg_data_MAR.rds")

results <- data.frame(na_frac = seq(5, 70, by = 5), value = integer(length(seq(5, 70, by = 5))))

set.seed(123)
for (i in 1:length(imp_reg_data_MAR)) {
  value <- 0
  for (j in 1:100) {
    contingency_table <- table(imp_reg_data_MAR[[i]][[j]]$household_head_job, 
                               imp_reg_data_MAR[[i]][[j]]$house_electricity)
    test_result <- fisher.test(contingency_table)
    if (test_result$p.value < 0.05) {
      value <- value + 1
    }
  }
  results$value[results$na_frac == i*5] <- value
}

#Saving .rds file
saveRDS(results, "fisher_reg_MAR.rds")

######MNAR######
imp_reg_data_MNAR <- readRDS("imp_reg_data_MNAR.rds")

results <- data.frame(na_frac = seq(5, 70, by = 5), value = integer(length(seq(5, 70, by = 5))))

set.seed(123)
for (i in 1:length(imp_reg_data_MNAR)) {
  value <- 0
  for (j in 1:100) {
    contingency_table <- table(imp_reg_data_MNAR[[i]][[j]]$household_head_job, 
                               imp_reg_data_MNAR[[i]][[j]]$house_electricity)
    test_result <- fisher.test(contingency_table)
    if (test_result$p.value < 0.05) {
      value <- value + 1
    }
  }
  results$value[results$na_frac == i*5] <- value
}


#Saving .rds file
saveRDS(results, "fisher_reg_MNAR.rds")

#####Random forest imputation#####

######MCAR######
imp_rf_data_MCAR <- readRDS("imp_rf_data_MCAR.rds")

results <- data.frame(na_frac = seq(5, 70, by = 5), value = integer(length(seq(5, 70, by = 5))))

set.seed(123)
for (i in 1:length(imp_rf_data_MCAR)) {
  value <- 0
  for (j in 1:100) {
    contingency_table <- table(imp_rf_data_MCAR[[i]][[j]]$household_head_job, 
                               imp_rf_data_MCAR[[i]][[j]]$house_electricity)
    test_result <- fisher.test(contingency_table)
    if (test_result$p.value < 0.05) {
      value <- value + 1
    }
  }
  results$value[results$na_frac == i*5] <- value
}

#Saving .rds file
saveRDS(results, "fisher_rf_MCAR.rds")

######MAR######
imp_rf_data_MAR <- readRDS("imp_rf_data_MAR.rds")

results <- data.frame(na_frac = seq(5, 70, by = 5), value = integer(length(seq(5, 70, by = 5))))

set.seed(123)
for (i in 1:length(imp_rf_data_MAR)) {
  value <- 0
  for (j in 1:100) {
    contingency_table <- table(imp_rf_data_MAR[[i]][[j]]$household_head_job, 
                               imp_rf_data_MAR[[i]][[j]]$house_electricity)
    test_result <- fisher.test(contingency_table)
    if (test_result$p.value < 0.05) {
      value <- value + 1
    }
  }
  results$value[results$na_frac == i*5] <- value
}

#Saving .rds file
saveRDS(results, "fisher_rf_MAR.rds")

######MNAR######
imp_rf_data_MNAR <- readRDS("imp_rf_data_MNAR.rds")

results <- data.frame(na_frac = seq(5, 70, by = 5), value = integer(length(seq(5, 70, by = 5))))

set.seed(123)
for (i in 1:length(imp_rf_data_MNAR)) {
  value <- 0
  for (j in 1:100) {
    contingency_table <- table(imp_rf_data_MNAR[[i]][[j]]$household_head_job, 
                               imp_rf_data_MNAR[[i]][[j]]$house_electricity)
    test_result <- fisher.test(contingency_table)
    if (test_result$p.value < 0.05) {
      value <- value + 1
    }
  }
  results$value[results$na_frac == i*5] <- value
}

#Saving .rds file
saveRDS(results, "fisher_rf_MNAR.rds")

#####Multiple imputation#####

######MCAR######
imp_mul_data_MCAR <- readRDS("imp_mul_data_MCAR.rds")

results <- data.frame(na_frac = seq(5, 70, by = 5), value = integer(length(seq(5, 70, by = 5))))

set.seed(123)
for (i in 1:length(imp_mul_data_MCAR)) {
  value <- 0
  for (j in 1:100) {
    contingency_table <- table(imp_mul_data_MCAR[[i]][[j]]$household_head_job, 
                               imp_mul_data_MCAR[[i]][[j]]$house_electricity)
    test_result <- fisher.test(contingency_table)
    if (test_result$p.value < 0.05) {
      value <- value + 1
    }
  }
  results$value[results$na_frac == i*5] <- value
}

#Saving .rds file
saveRDS(results, "fisher_mul_MCAR.rds")

######MAR######
imp_mul_data_MAR <- readRDS("imp_mul_data_MAR.rds")

results <- data.frame(na_frac = seq(5, 70, by = 5), value = integer(length(seq(5, 70, by = 5))))

set.seed(123)
for (i in 1:length(imp_mul_data_MAR)) {
  value <- 0
  for (j in 1:100) {
    contingency_table <- table(imp_mul_data_MAR[[i]][[j]]$household_head_job, 
                               imp_mul_data_MAR[[i]][[j]]$house_electricity)
    test_result <- fisher.test(contingency_table)
    if (test_result$p.value < 0.05) {
      value <- value + 1
    }
  }
  results$value[results$na_frac == i*5] <- value
}

#Saving .rds file
saveRDS(results, "fisher_mul_MAR.rds")

######MNAR######
imp_mul_data_MNAR <- readRDS("imp_mul_data_MNAR.rds")

results <- data.frame(na_frac = seq(5, 70, by = 5), value = integer(length(seq(5, 70, by = 5))))

set.seed(123)
for (i in 1:length(imp_mul_data_MNAR)) {
  value <- 0
  for (j in 1:100) {
    contingency_table <- table(imp_mul_data_MNAR[[i]][[j]]$household_head_job, 
                               imp_mul_data_MNAR[[i]][[j]]$house_electricity)
    test_result <- fisher.test(contingency_table)
    if (test_result$p.value < 0.05) {
      value <- value + 1
    }
  }
  results$value[results$na_frac == i*5] <- value
}

#Saving .rds file
saveRDS(results, "fisher_mul_MNAR.rds")

######Without NA's######
value <- 0

for (i in 1:100) {
  contingency_table <- table(data_samples_MCAR[[1]]$household_head_job, 
                             data_samples_MCAR[[1]]$house_electricity)
  test_result <- fisher.test(contingency_table)
  if (test_result$p.value < 0.05) {
    value <- value + 1
  }
}

value