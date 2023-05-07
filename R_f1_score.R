#####Defining function#####

#Unbalanced classes
round(table(data$house_type_wall) / length(data$house_type_wall) * 100, 2)

#Creating function - calculating F1-score macro (same weights for each class)
calculate_macro_f1_score <- function(actual, predicted) {
  classes <- unique(actual)
  f1_scores <- sapply(classes, function(x) {
    confusion_matrix <- table(predicted, actual)
    tp <- confusion_matrix[x, x]
    fp <- sum(confusion_matrix[x, ]) - tp
    fn <- sum(confusion_matrix[, x]) - tp
    
    precision <- tp / (tp + fp)
    recall <- tp / (tp + fn)
    
    f1_score <- 2 * (precision * recall) / (precision + recall)
    return(f1_score)
  })
  
  f1_scores <- f1_scores[!is.na(f1_scores)]
  return(mean(f1_scores, na.rm = TRUE))
}

#####Median imputation#####

#Not applicable

#####Hot-deck imputation#####

######MCAR######
imp_hot_deck_data_MCAR <- readRDS("imp_hot_deck_data_MCAR.rds")

result <- data.frame(mean_f1_macro = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MCAR)))
  
  for (j in seq_along(data_samples_MCAR)) {
    actual <- data_samples_MCAR[[j]]$house_type_wall
    predicted <- imp_hot_deck_data_MCAR[[i]][[j]]$house_type_wall
    res_i[j] <- calculate_macro_f1_score(actual, predicted)
  }
  
  result[i, "mean_f1_macro"] <- round(mean(res_i, na.rm = TRUE) * 100, 2)
}

f1_hotdeck_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_f1_macro = result)
f1_hotdeck_MCAR

#Saving .rds file
saveRDS(f1_hotdeck_MCAR, "f1_hotdeck_MCAR.rds")

######MAR######
imp_hot_deck_data_MAR <- readRDS("imp_hot_deck_data_MAR.rds")

result <- data.frame(mean_f1_macro = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MAR)))
  
  for (j in seq_along(data_samples_MAR)) {
    actual <- data_samples_MAR[[j]]$house_type_wall
    predicted <- imp_hot_deck_data_MAR[[i]][[j]]$house_type_wall
    res_i[j] <- calculate_macro_f1_score(actual, predicted)
  }
  
  result[i, "mean_f1_macro"] <- round(mean(res_i, na.rm = TRUE) * 100, 2)
}

f1_hotdeck_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_f1_macro = result)
f1_hotdeck_MAR

#Saving .rds file
saveRDS(f1_hotdeck_MAR, "f1_hotdeck_MAR.rds")

######MNAR######
imp_hot_deck_data_MNAR <- readRDS("imp_hot_deck_data_MNAR.rds")

result <- data.frame(mean_f1_macro = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MNAR)))
  
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$house_type_wall
    predicted <- imp_hot_deck_data_MNAR[[i]][[j]]$house_type_wall
    res_i[j] <- calculate_macro_f1_score(actual, predicted)
  }
  
  result[i, "mean_f1_macro"] <- round(mean(res_i, na.rm = TRUE) * 100, 2)
}

f1_hotdeck_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_f1_macro = result)
f1_hotdeck_MNAR

#Saving .rds file
saveRDS(f1_hotdeck_MNAR, "f1_hotdeck_MNAR.rds")

#####kNN imputation#####

######MCAR######
imp_kNN_data_MCAR <- readRDS("imp_kNN_data_MCAR.rds")

result <- data.frame(mean_f1_macro = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MCAR)))
  
  for (j in seq_along(data_samples_MCAR)) {
    actual <- data_samples_MCAR[[j]]$house_type_wall
    predicted <- imp_kNN_data_MCAR[[i]][[j]]$house_type_wall
    res_i[j] <- calculate_macro_f1_score(actual, predicted)
  }
  
  result[i, "mean_f1_macro"] <- round(mean(res_i, na.rm = TRUE) * 100, 2)
}

f1_kNN_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_f1_macro = result)
f1_kNN_MCAR

#Saving .rds file
saveRDS(f1_kNN_MCAR, "f1_kNN_MCAR.rds")

######MAR######
imp_kNN_data_MAR <- readRDS("imp_kNN_data_MAR.rds")

result <- data.frame(mean_f1_macro = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MAR)))
  
  for (j in seq_along(data_samples_MAR)) {
    actual <- data_samples_MAR[[j]]$house_type_wall
    predicted <- imp_kNN_data_MAR[[i]][[j]]$house_type_wall
    res_i[j] <- calculate_macro_f1_score(actual, predicted)
  }
  
  result[i, "mean_f1_macro"] <- round(mean(res_i, na.rm = TRUE) * 100, 2)
}

f1_kNN_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_f1_macro = result)
f1_kNN_MAR

#Saving .rds file
saveRDS(f1_kNN_MAR, "f1_kNN_MAR.rds")

######MNAR######
imp_kNN_data_MNAR <- readRDS("imp_kNN_data_MNAR.rds")

result <- data.frame(mean_f1_macro = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MNAR)))
  
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$house_type_wall
    predicted <- imp_kNN_data_MNAR[[i]][[j]]$house_type_wall
    res_i[j] <- calculate_macro_f1_score(actual, predicted)
  }
  
  result[i, "mean_f1_macro"] <- round(mean(res_i, na.rm = TRUE) * 100, 2)
}

f1_kNN_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_f1_macro = result)
f1_kNN_MNAR

#Saving .rds file
saveRDS(f1_kNN_MNAR, "f1_kNN_MNAR.rds")

#####Regression imputation#####

######MCAR######
imp_reg_data_MCAR <- readRDS("imp_reg_data_MCAR.rds")

result <- data.frame(mean_f1_macro = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MCAR)))
  
  for (j in seq_along(data_samples_MCAR)) {
    actual <- data_samples_MCAR[[j]]$house_type_wall
    predicted <- imp_reg_data_MCAR[[i]][[j]]$house_type_wall
    res_i[j] <- calculate_macro_f1_score(actual, predicted)
  }
  
  result[i, "mean_f1_macro"] <- round(mean(res_i, na.rm = TRUE) * 100, 2)
}

f1_reg_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_f1_macro = result)
f1_reg_MCAR

#Saving .rds file
saveRDS(f1_reg_MCAR, "f1_reg_MCAR.rds")

######MAR######
imp_reg_data_MAR <- readRDS("imp_reg_data_MAR.rds")

result <- data.frame(mean_f1_macro = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MAR)))
  
  for (j in seq_along(data_samples_MAR)) {
    actual <- data_samples_MAR[[j]]$house_type_wall
    predicted <- imp_reg_data_MAR[[i]][[j]]$house_type_wall
    res_i[j] <- calculate_macro_f1_score(actual, predicted)
  }
  
  result[i, "mean_f1_macro"] <- round(mean(res_i, na.rm = TRUE) * 100, 2)
}

f1_reg_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_f1_macro = result)
f1_reg_MAR

#Saving .rds file
saveRDS(f1_reg_MAR, "f1_reg_MAR.rds")

######MNAR######
imp_reg_data_MNAR <- readRDS("imp_reg_data_MNAR.rds")

result <- data.frame(mean_f1_macro = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MNAR)))
  
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$house_type_wall
    predicted <- imp_reg_data_MNAR[[i]][[j]]$house_type_wall
    res_i[j] <- calculate_macro_f1_score(actual, predicted)
  }
  
  result[i, "mean_f1_macro"] <- round(mean(res_i, na.rm = TRUE) * 100, 2)
}

f1_reg_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_f1_macro = result)
f1_reg_MNAR

#Saving .rds file
saveRDS(f1_reg_MNAR, "f1_reg_MNAR.rds")

#####Random forest imputation#####

######MCAR######
imp_rf_data_MCAR <- readRDS("imp_rf_data_MCAR.rds")

result <- data.frame(mean_f1_macro = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MCAR)))
  
  for (j in seq_along(data_samples_MCAR)) {
    actual <- data_samples_MCAR[[j]]$house_type_wall
    predicted <- imp_rf_data_MCAR[[i]][[j]]$house_type_wall
    res_i[j] <- calculate_macro_f1_score(actual, predicted)
  }
  
  result[i, "mean_f1_macro"] <- round(mean(res_i, na.rm = TRUE) * 100, 2)
}

f1_rf_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_f1_macro = result)
f1_rf_MCAR

#Saving .rds file
saveRDS(f1_rf_MCAR, "f1_rf_MCAR.rds")

######MAR######
imp_rf_data_MAR <- readRDS("imp_rf_data_MAR.rds")

result <- data.frame(mean_f1_macro = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MAR)))
  
  for (j in seq_along(data_samples_MAR)) {
    actual <- data_samples_MAR[[j]]$house_type_wall
    predicted <- imp_rf_data_MAR[[i]][[j]]$house_type_wall
    res_i[j] <- calculate_macro_f1_score(actual, predicted)
  }
  
  result[i, "mean_f1_macro"] <- round(mean(res_i, na.rm = TRUE) * 100, 2)
}

f1_rf_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_f1_macro = result)
f1_rf_MAR

#Saving .rds file
saveRDS(f1_rf_MAR, "f1_rf_MAR.rds")

######MNAR######
imp_rf_data_MNAR <- readRDS("imp_rf_data_MNAR.rds")

result <- data.frame(mean_f1_macro = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MNAR)))
  
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$house_type_wall
    predicted <- imp_rf_data_MNAR[[i]][[j]]$house_type_wall
    res_i[j] <- calculate_macro_f1_score(actual, predicted)
  }
  
  result[i, "mean_f1_macro"] <- round(mean(res_i, na.rm = TRUE) * 100, 2)
}

f1_rf_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_f1_macro = result)
f1_rf_MNAR

#Saving .rds file
saveRDS(f1_rf_MNAR, "f1_rf_MNAR.rds")

#####Multiple imputation#####

######MCAR######
imp_mul_data_MCAR <- readRDS("imp_mul_data_MCAR.rds")

result <- data.frame(mean_f1_macro = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MCAR)))
  
  for (j in seq_along(data_samples_MCAR)) {
    actual <- data_samples_MCAR[[j]]$house_type_wall
    predicted <- imp_mul_data_MCAR[[i]][[j]]$house_type_wall
    res_i[j] <- calculate_macro_f1_score(actual, predicted)
  }
  
  result[i, "mean_f1_macro"] <- round(mean(res_i, na.rm = TRUE) * 100, 2)
}

f1_mul_MCAR <- data.frame(na_frac = seq(5, 70, 5), mean_f1_macro = result)
f1_mul_MCAR

#Saving .rds file
saveRDS(f1_mul_MCAR, "f1_mul_MCAR.rds")

######MAR######
imp_mul_data_MAR <- readRDS("imp_mul_data_MAR.rds")

result <- data.frame(mean_f1_macro = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MAR)))
  
  for (j in seq_along(data_samples_MAR)) {
    actual <- data_samples_MAR[[j]]$house_type_wall
    predicted <- imp_mul_data_MAR[[i]][[j]]$house_type_wall
    res_i[j] <- calculate_macro_f1_score(actual, predicted)
  }
  
  result[i, "mean_f1_macro"] <- round(mean(res_i, na.rm = TRUE) * 100, 2)
}

f1_mul_MAR <- data.frame(na_frac = seq(5, 70, 5), mean_f1_macro = result)
f1_mul_MAR

#Saving .rds file
saveRDS(f1_mul_MAR, "f1_mul_MAR.rds")

######MNAR######
imp_mul_data_MNAR <- readRDS("imp_mul_data_MNAR.rds")

result <- data.frame(mean_f1_macro = numeric(14))

for (i in 1:14) {
  res_i <- numeric(length(seq_along(data_samples_MNAR)))
  
  for (j in seq_along(data_samples_MNAR)) {
    actual <- data_samples_MNAR[[j]]$house_type_wall
    predicted <- imp_mul_data_MNAR[[i]][[j]]$house_type_wall
    res_i[j] <- calculate_macro_f1_score(actual, predicted)
  }
  
  result[i, "mean_f1_macro"] <- round(mean(res_i, na.rm = TRUE) * 100, 2)
}

f1_mul_MNAR <- data.frame(na_frac = seq(5, 70, 5), mean_f1_macro = result)
f1_mul_MNAR

#Saving .rds file
saveRDS(f1_mul_MNAR, "f1_mul_MNAR.rds")
