#####Median imputation#####

######MCAR######
set.seed(123)
imp_median_data_MCAR <- list()

system.time({
  for (i in seq_along(data_MCAR_list)) {
    temp_list <- list()
    for (j in seq_len(length(data_MCAR_list[[i]]))) {
      data <- data_MCAR_list[[i]][[j]]
      data$tot_household_income <-
        ifelse(
          is.na(data$tot_household_income),
          median(data$tot_household_income, na.rm = TRUE),
          data$tot_household_income
        )
      temp_list[[j]] <- data
    }
    imp_median_data_MCAR[[i]] <- temp_list
  }
})

saveRDS(imp_median_data_MCAR, "imp_median_data_MCAR.rds")

######MAR######
set.seed(123)
imp_median_data_MAR <- list()

system.time({
  for (i in seq_along(data_MAR_list)) {
    temp_list <- list()
    for (j in seq_len(length(data_MAR_list[[i]]))) {
      data <- data_MAR_list[[i]][[j]]
      data$tot_household_income <-
        ifelse(
          is.na(data$tot_household_income),
          median(data$tot_household_income, na.rm = TRUE),
          data$tot_household_income
        )
      temp_list[[j]] <- data
    }
    imp_median_data_MAR[[i]] <- temp_list
  }
})

saveRDS(imp_median_data_MAR, "imp_median_data_MAR.rds")

######MNAR######
set.seed(123)
imp_median_data_MNAR <- list()

system.time({
  for (i in seq_along(data_MNAR_list)) {
    temp_list <- list()
    for (j in seq_len(length(data_MNAR_list[[i]]))) {
      data <- data_MNAR_list[[i]][[j]]
      data$tot_household_income <-
        ifelse(
          is.na(data$tot_household_income),
          median(data$tot_household_income, na.rm = TRUE),
          data$tot_household_income
        )
      temp_list[[j]] <- data
    }
    imp_median_data_MNAR[[i]] <- temp_list
  }
})

saveRDS(imp_median_data_MNAR, "imp_median_data_MNAR.rds")

#####Hot-deck imputation#####

######Choosing domain_var######

#Finding best domain variable - MNAR 70% NA's test
factor_var <- c(names(data)[sapply(data, is.factor)])
missing_var <- c("tot_household_income", "main_source_income", "household_head_job", "house_type_wall")
domains <- setdiff(factor_var, missing_var)

median_of_medians_list <- numeric(length(domains))
for (i in seq_along(domains)) {
  results_list <- lapply(data_MNAR_list[[14]], function(x) {
    hotdeck(x, variable = c("tot_household_income",
                            "main_source_income", 
                            "household_head_job", 
                            "house_type_wall"), 
            domain_var = domains[i])
  })
  medians_list <- sapply(results_list, function(dataset) median(dataset$tot_household_income, na.rm = TRUE))
  median_of_medians_list[i] <- median(medians_list, na.rm = TRUE)
}

#Calculating the precentage difference between max and min tot_household_income dependent on choosed domain
paste(round((max(median_of_medians_list) - min(median_of_medians_list)) / max(median_of_medians_list) * 100, 2), "%")

#Define variables 
var = c("tot_household_income", "main_source_income", "household_head_job", "house_type_wall")
domain = c("region")

######MCAR######
set.seed(123)
imp_hot_deck_data_MCAR <- list()

system.time(
  for (i in seq_along(data_MCAR_list)) {
    temp_list <- list()
    for (j in seq_len(length(data_MCAR_list[[i]]))) {
      temp_list[[j]] <- hotdeck(data_MCAR_list[[i]][[j]], 
                                variable = var,
                                domain_var = domain)
    }
    imp_hot_deck_data_MCAR[[i]] <- temp_list
  }
)

saveRDS(imp_hot_deck_data_MCAR, "imp_hot_deck_data_MCAR.rds")

######MAR######
set.seed(123)
imp_hot_deck_data_MAR <- list()

system.time(
  for (i in seq_along(data_MAR_list)) {
    temp_list <- list()
    for (j in seq_len(length(data_MAR_list[[i]]))) {
      temp_list[[j]] <- hotdeck(data_MAR_list[[i]][[j]], 
                                variable = var,
                                domain_var = domain)
    }
    imp_hot_deck_data_MAR[[i]] <- temp_list
  }
)

saveRDS(imp_hot_deck_data_MAR, "imp_hot_deck_data_MAR.rds")

######MNAR######
set.seed(123)
imp_hot_deck_data_MNAR <- list()

system.time(
  for (i in seq_along(data_MNAR_list)) {
    temp_list <- list()
    for (j in seq_len(length(data_MNAR_list[[i]]))) {
      temp_list[[j]] <- hotdeck(data_MNAR_list[[i]][[j]], 
                                variable = var,
                                domain_var = domain)
    }
    imp_hot_deck_data_MNAR[[i]] <- temp_list
  }
)
  
saveRDS(imp_hot_deck_data_MNAR, "imp_hot_deck_data_MNAR.rds")

#####kNN imputation#####

######Choosing best k-value#######

#Defining k-values
k_values <- c(1, 2, 15)
results_list <- list()

#Choosing best k - test
for (i in k_values) {
  test_imp <- kNN(
    t,
    k = i,
    variable = c(
      "tot_household_income",
      "house_type_wall",
      "house_type_wall",
      "house_type_wall"
    ),
    imp_var = FALSE
  )
  results_list[[as.character(i)]] <-
    table(test_imp$house_type_wall) / sum(table(test_imp$house_type_wall))
}

for (i in k_values) {
  print(paste("Results for k =", i))
  print(results_list[[as.character(i)]])
}

######MCAR######
set.seed(123)
imp_kNN_data_MCAR <- list()

system.time(
  for (i in seq_along(data_MCAR_list)) {
    temp_list <- list()
    for (j in seq_len(length(data_MCAR_list[[i]]))) {
      temp_list[[j]] <- kNN(data_MCAR_list[[i]][[j]], 
                            k = 2, 
                            variable = c("tot_household_income", "main_source_income", "household_head_job", "house_type_wall"),
                            imp_var = FALSE)
    }
    imp_kNN_data_MCAR[[i]] <- temp_list
  }
)

saveRDS(imp_kNN_data_MCAR, "imp_kNN_data_MCAR.rds")

######MAR######
set.seed(123)
imp_kNN_data_MAR <- list()

system.time(
  for (i in seq_along(data_MAR_list)) {
    temp_list <- list()
    for (j in seq_len(length(data_MAR_list[[i]]))) {
      temp_list[[j]] <- kNN(data_MAR_list[[i]][[j]], 
                            k = 2, 
                            variable = c("tot_household_income", "main_source_income", "household_head_job", "house_type_wall"),
                            imp_var = FALSE)
    }
    imp_kNN_data_MAR[[i]] <- temp_list
  }
)

saveRDS(imp_kNN_data_MAR, "imp_kNN_data_MAR.rds")

######MNAR######
set.seed(123)
imp_kNN_data_MNAR <- list()

system.time(
  for (i in seq_along(data_MNAR_list)) {
    temp_list <- list()
    for (j in seq_len(length(data_MNAR_list[[i]]))) {
      temp_list[[j]] <- kNN(data_MNAR_list[[i]][[j]], 
                            k = 2, 
                            variable = c("tot_household_income", "main_source_income", "household_head_job", "house_type_wall"),
                            imp_var = FALSE)
    }
    imp_kNN_data_MNAR[[i]] <- temp_list
  }
)

saveRDS(imp_kNN_data_MNAR, "imp_kNN_data_MNAR.rds")

#####Regression imputation#####

######Linear regression model - var: tot_household_income######

######MCAR######
set.seed(123)
imp_lm_data_MCAR <- list()

system.time(for (i in seq_along(data_MCAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MCAR_list[[i]]))) {
    temp_list[[j]] <- impute_lm(
      data_MCAR_list[[i]][[j]],
      tot_household_income ~ housing_exp + miscellaneous_goods_exp + tot_income_enterpreneurial_act
    )
  }
  imp_lm_data_MCAR[[i]] <- temp_list
})

imp_lm_data_MCAR[[14]][[100]]

#Saving .rds file
saveRDS(imp_lm_data_MCAR, "imp_lm_data_MCAR.rds")

######MAR######
set.seed(123)
imp_lm_data_MAR <- list()

system.time(for (i in seq_along(data_MAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MAR_list[[i]]))) {
    temp_list[[j]] <- impute_lm(
      data_MAR_list[[i]][[j]],
      tot_household_income ~ tot_food_exp + housing_exp + miscellaneous_goods_exp
    )
  }
  imp_lm_data_MAR[[i]] <- temp_list
})

imp_lm_data_MAR[[14]][[100]]

#Saving .rds file
saveRDS(imp_lm_data_MAR, "imp_lm_data_MAR.rds")

######MNAR######
set.seed(123)
imp_lm_data_MNAR <- list()

system.time(for (i in seq_along(data_MNAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MNAR_list[[i]]))) {
    temp_list[[j]] <- impute_lm(
      data_MNAR_list[[i]][[j]],
      tot_household_income ~ housing_exp + miscellaneous_goods_exp + tot_income_enterpreneurial_act
    )
  }
  imp_lm_data_MNAR[[i]] <- temp_list
})

#Saving .rds file
saveRDS(imp_lm_data_MNAR, "imp_lm_data_MNAR.rds")

######Logistic regression model - var: household_head_job######

######MCAR######
set.seed(123)
imp_glm_data_MCAR <- list()

system.time(for (i in seq_along(data_MCAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MCAR_list[[i]]))) {
    data <- data_MCAR_list[[i]][[j]]
    
    independent_vars <- c("tot_income_enterpreneurial_act", "household_head_age", "num_washing_machine")
    
    formula <- as.formula(paste("household_head_job ~", paste(independent_vars, collapse = " + ")))
    
    data_subset <- data[complete.cases(data[independent_vars]), ]
    
    glm_model <- glm(formula, data = data_subset, family = binomial())
    
    missing_indices <- is.na(data$household_head_job)
    predicted_values <- predict(glm_model, newdata = data[missing_indices, ], type = "response")
    imputed_values <- as.logical(ifelse(predicted_values > 0.5, TRUE, FALSE))
    
    data$household_head_job[missing_indices] <- imputed_values
    
    temp_list[[j]] <- data
  }
  imp_glm_data_MCAR[[i]] <- temp_list
})

#Saving .rds file
saveRDS(imp_glm_data_MCAR, "imp_glm_data_MCAR.rds")

######MAR######
set.seed(123)
imp_glm_data_MAR <- list()

system.time(for (i in seq_along(data_MAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MAR_list[[i]]))) {
    data <- data_MAR_list[[i]][[j]]
    
    independent_vars <- c("household_head_age", "num_family_member_employed", "house_num_tv")
    
    formula <- as.formula(paste("household_head_job ~", paste(independent_vars, collapse = " + ")))
    
    data_subset <- data[complete.cases(data[independent_vars]), ]
    
    glm_model <- glm(formula, data = data_subset, family = binomial())
    
    missing_indices <- is.na(data$household_head_job)
    predicted_values <- predict(glm_model, newdata = data[missing_indices, ], type = "response")
    imputed_values <- as.logical(ifelse(predicted_values > 0.5, TRUE, FALSE))
    
    data$household_head_job[missing_indices] <- imputed_values
    
    temp_list[[j]] <- data
  }
  imp_glm_data_MAR[[i]] <- temp_list
})

#Saving .rds file
saveRDS(imp_glm_data_MAR, "imp_glm_data_MAR.rds")

######MNAR######
set.seed(123)
imp_glm_data_MNAR <- list()

system.time(for (i in seq_along(data_MNAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MNAR_list[[i]]))) {
    data <- data_MNAR_list[[i]][[j]]
    
    independent_vars <- c("housing_exp", "tot_income_enterpreneurial_act", "household_head_age")
    
    formula <- as.formula(paste("household_head_job ~", paste(independent_vars, collapse = " + ")))
    
    data_subset <- data[complete.cases(data[independent_vars]), ]
    
    glm_model <- glm(formula, data = data_subset, family = binomial())
    
    missing_indices <- is.na(data$household_head_job)
    predicted_values <- predict(glm_model, newdata = data[missing_indices, ], type = "response")
    imputed_values <- as.logical(ifelse(predicted_values > 0.5, TRUE, FALSE))
    
    data$household_head_job[missing_indices] <- imputed_values
    
    temp_list[[j]] <- data
  }
  imp_glm_data_MNAR[[i]] <- temp_list
})

#Saving .rds file
saveRDS(imp_glm_data_MNAR, "imp_glm_data_MNAR.rds")

######Multinomial logistic regression model - var: main_source_income######

######MCAR######
set.seed(123)
imp_multinom_data_MCAR <- list()

system.time(for (i in seq_along(data_MCAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MCAR_list[[i]]))) {
    data <- data_MCAR_list[[i]][[j]]
    
    independent_vars <- c("housing_exp", "tot_income_enterpreneurial_act", "num_family_member_employed")
    
    formula <- as.formula(paste("main_source_income ~", paste(independent_vars, collapse = " + ")))
    
    data_subset <- data[complete.cases(data[independent_vars]), ]
    
    multinom_model <- nnet::multinom(formula, data = data_subset, MaxNWts = 1000)
    
    missing_indices <- is.na(data$main_source_income)
    predicted_values <- predict(multinom_model, newdata = data[missing_indices, ], type = "class")
    
    data$main_source_income[missing_indices] <- predicted_values
    
    temp_list[[j]] <- data
  }
  imp_multinom_data_MCAR[[i]] <- temp_list
})

#Saving .rds file
saveRDS(imp_multinom_data_MCAR, "imp_multinom_data_MCAR.rds")

######MAR######
set.seed(123)
imp_multinom_data_MAR <- list()

system.time(for (i in seq_along(data_MAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MAR_list[[i]]))) {
    data <- data_MAR_list[[i]][[j]]
    
    independent_vars <- c("housing_exp", "tot_income_enterpreneurial_act", "num_family_member_employed")
    
    formula <- as.formula(paste("main_source_income ~", paste(independent_vars, collapse = " + ")))
    
    data_subset <- data[complete.cases(data[independent_vars]), ]
    
    multinom_model <- nnet::multinom(formula, data = data_subset, MaxNWts = 1000)
    
    missing_indices <- is.na(data$main_source_income)
    predicted_values <- predict(multinom_model, newdata = data[missing_indices, ], type = "class")
    
    data$main_source_income[missing_indices] <- predicted_values
    
    temp_list[[j]] <- data
  }
  imp_multinom_data_MAR[[i]] <- temp_list
})

#Saving .rds file
saveRDS(imp_multinom_data_MAR, "imp_multinom_data_MAR.rds")

######MNAR######
set.seed(123)
imp_multinom_data_MNAR <- list()

system.time(for (i in seq_along(data_MNAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MNAR_list[[i]]))) {
    data <- data_MNAR_list[[i]][[j]]
    
    independent_vars <- c("meat_exp", "tot_income_enterpreneurial_act", "num_family_member_employed")
    
    formula <- as.formula(paste("main_source_income ~", paste(independent_vars, collapse = " + ")))
    
    data_subset <- data[complete.cases(data[independent_vars]), ]
    
    multinom_model <- nnet::multinom(formula, data = data_subset, MaxNWts = 1000)
    
    missing_indices <- is.na(data$main_source_income)
    predicted_values <- predict(multinom_model, newdata = data[missing_indices, ], type = "class")
    
    data$main_source_income[missing_indices] <- predicted_values
    
    temp_list[[j]] <- data
  }
  imp_multinom_data_MNAR[[i]] <- temp_list
})

#Saving .rds file
saveRDS(imp_multinom_data_MNAR, "imp_multinom_data_MNAR.rds")

######Ordinal logistic regression model - var: house_type_wall######

######MCAR######
set.seed(123)
imp_polr_data_MCAR <- list()

system.time(for (i in seq_along(data_MCAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MCAR_list[[i]]))) {
    data <- data_MCAR_list[[i]][[j]]
    
    independent_vars <- c("housing_exp", "house_area", "house_num_bedrooms")
    
    formula <- as.formula(paste("house_type_wall ~", paste(independent_vars, collapse = " + ")))
    
    data_subset <- data[complete.cases(data[independent_vars]), ]
    
    polr_model <- polr(formula, data = data_subset, Hess = TRUE)
    
    missing_indices <- is.na(data$house_type_wall)
    predicted_values <- predict(polr_model, newdata = data[missing_indices, ], type = "class")
    
    data$house_type_wall[missing_indices] <- predicted_values
    
    temp_list[[j]] <- data
  }
  imp_polr_data_MCAR[[i]] <- temp_list
})

#Saving .rds file
saveRDS(imp_polr_data_MCAR, "imp_polr_data_MCAR.rds")

######MAR######
set.seed(123)
imp_polr_data_MAR <- list()

system.time(for (i in seq_along(data_MAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MAR_list[[i]]))) {
    data <- data_MAR_list[[i]][[j]]
    
    independent_vars <- c("housing_exp", "house_area", "house_num_bedrooms")
    
    formula <- as.formula(paste("house_type_wall ~", paste(independent_vars, collapse = " + ")))
    
    data_subset <- data[complete.cases(data[independent_vars]), ]
    
    polr_model <- polr(formula, data = data_subset, Hess = TRUE)
    
    missing_indices <- is.na(data$house_type_wall)
    predicted_values <- predict(polr_model, newdata = data[missing_indices, ], type = "class")
    
    data$house_type_wall[missing_indices] <- predicted_values
    
    temp_list[[j]] <- data
  }
  imp_polr_data_MAR[[i]] <- temp_list
})

#Saving .rds file
saveRDS(imp_polr_data_MAR, "imp_polr_data_MAR.rds")

######MNAR######
set.seed(123)
imp_polr_data_MNAR <- list()

system.time(for (i in seq_along(data_MNAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MNAR_list[[i]]))) {
    data <- data_MNAR_list[[i]][[j]]
    
    independent_vars <- c("housing_exp", "house_num_bedrooms")
    
    formula <- as.formula(paste("house_type_wall ~", paste(independent_vars, collapse = " + ")))
    
    data_subset <- data[complete.cases(data[independent_vars]), ]
    
    polr_model <- polr(formula, data = data_subset, Hess = TRUE)
    
    missing_indices <- is.na(data$house_type_wall)
    predicted_values <- predict(polr_model, newdata = data[missing_indices, ], type = "class")
    
    data$house_type_wall[missing_indices] <- predicted_values
    
    temp_list[[j]] <- data
  }
  imp_polr_data_MNAR[[i]] <- temp_list
})

#Saving .rds file
saveRDS(imp_polr_data_MNAR, "imp_polr_data_MNAR.rds")

#####Random forest imputation#####

######MCAR######
imp_rf_data_MCAR_list <-
  
######MAR######
imp_rf_data_MAR_list <-
  
######MNAR######
imp_rf_data_MNAR_list <-  
  
#####Multiple imputation#####

######MCAR######
imp_mul_data_MCAR_list <-

######MAR######
imp_mul_data_MAR_list <-

######MNAR######
imp_mul_data_MNAR_list <-