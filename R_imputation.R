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

######Choosing best linear model######
#Preparing data for test
t <- data_MNAR_list[[14]][[3]] %>% 
  select(-ID) %>% 
  as.data.frame() %>% 
  mutate_if(is.logical, as.factor)

#Defining dependent and explanatory variables
dependent_var <- t$tot_household_income
explanatory_var <- c("tot_food_exp",
                     "bread_cereales_exp",
                     "rice_exp",
                     "meat_exp",
                     "fish_exp",
                     "fruit_exp",
                     "vegetables_exp",
                     "restaurant_hotels_exp",
                     "alcohol_exp",
                     "tobacco_exp",
                     "clothing_exp",
                     "housing_exp",
                     "house_rent_exp",
                     "medicare_exp",
                     "transportation_exp",
                     "communication_exp",
                     "education_exp",
                     "miscellaneous_goods_exp",
                     "special_occasions_exp",
                     "crop_farming_gardening_exp",
                     "tot_income_enterpreneurial_act",
                     "household_head_age",
                     "num_family_member",
                     "num_family_member_0_5",
                     "num_family_member_5_17",
                     "num_family_ember_employed",
                     "house_area",
                     "house_age",
                     "house_num_bedrooms",
                     "house_num_tv",
                     "house_num_cd_dvd",
                     "house_num_stereo",
                     "house_num_refrigator",
                     "num_washing_machine",
                     "house_num_airconditioner",
                     "house_num_car",
                     "house_num_landline_phone",
                     "house_num_smartphone",
                     "house_num_computer",
                     "house_num_oven",
                     "house_num_boat",
                     "house_num_motorcycle_tricycle")

#Calculating AIC
calculate_AIC <- function(vars) {
  formula <-
    as.formula(paste("t$tot_household_income ~ ", paste(vars, collapse = "+")))
  model <- lm(formula, data = t)
  AIC(model)
}

#AIC function
results_AIC <- c()
for (i in 1:3) {
  com <- combn(explanatory_var, i)
  for (j in 1:ncol(com)) {
    result <- calculate_AIC(com[,j])
    results_AIC <- c(results_AIC, result)
    names(results_AIC)[length(results_AIC)] <- paste(com[,j], collapse = "_")
  }
}

#Choosing best model
head(results_AIC[order(results_AIC)], 5)
best_AIC <- min(results_AIC)
results_AIC[which(results_AIC == best_AIC)]


######MCAR######
set.seed(123)
imp_lm_data_MCAR <- list()

system.time(for (i in seq_along(data_MCAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MCAR_list[[i]]))) {
    temp_list[[j]] <- impute_lm(
      data_MCAR_list[[i]][[j]],
      tot_household_income ~ tot_food_exp + housing_exp + crop_farming_gardening_exp
    )
  }
  imp_lm_data_MCAR[[i]] <- temp_list
})

imp_lm_data_MCAR[[14]][[100]]

saveRDS(imp_lm_data_MCAR, "imp_lm_data_MCAR.rds")

######MAR######
set.seed(123)
imp_lm_data_MAR <- list()

system.time(for (i in seq_along(data_MAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MAR_list[[i]]))) {
    temp_list[[j]] <- impute_lm(
      data_MAR_list[[i]][[j]],
      tot_household_income ~ tot_food_exp + housing_exp + crop_farming_gardening_exp
    )
  }
  imp_lm_data_MAR[[i]] <- temp_list
})

imp_lm_data_MAR[[14]][[100]]

saveRDS(imp_lm_data_MAR, "imp_lm_data_MAR.rds")

######MNAR######
set.seed(123)
imp_lm_data_MNAR <- list()

system.time(for (i in seq_along(data_MNAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MNAR_list[[i]]))) {
    temp_list[[j]] <- impute_lm(
      data_MNAR_list[[i]][[j]],
      tot_household_income ~ tot_food_exp + housing_exp + crop_farming_gardening_exp
    )
  }
  imp_lm_data_MNAR[[i]] <- temp_list
})

saveRDS(imp_lm_data_MNAR, "imp_lm_data_MNAR.rds")

######Choosing best glm model - var: household_head_job######
#Preparing data for test
t <- data_MCAR_list[[1]][[3]] %>% 
  select(-ID) %>% 
  as.data.frame() %>% 
  mutate_if(is.logical, as.factor)

#Defining dependent and explanatory variables
dependent_var <- t$household_head_job
explanatory_var <- c("tot_food_exp",
                     "bread_cereales_exp",
                     "rice_exp",
                     "meat_exp",
                     "fish_exp",
                     "fruit_exp",
                     "vegetables_exp",
                     "restaurant_hotels_exp",
                     "alcohol_exp",
                     "tobacco_exp",
                     "clothing_exp",
                     "housing_exp",
                     "house_rent_exp",
                     "medicare_exp",
                     "transportation_exp",
                     "communication_exp",
                     "education_exp",
                     "miscellaneous_goods_exp",
                     "special_occasions_exp",
                     "crop_farming_gardening_exp",
                     "tot_income_enterpreneurial_act",
                     "household_head_age",
                     "num_family_member",
                     "num_family_member_0_5",
                     "num_family_member_5_17",
                     "num_family_ember_employed",
                     "house_area",
                     "house_age",
                     "house_num_bedrooms",
                     "house_num_tv",
                     "house_num_cd_dvd",
                     "house_num_stereo",
                     "house_num_refrigator",
                     "num_washing_machine",
                     "house_num_airconditioner",
                     "house_num_car",
                     "house_num_landline_phone",
                     "house_num_smartphone",
                     "house_num_computer",
                     "house_num_oven",
                     "house_num_boat",
                     "house_num_motorcycle_tricycle")

#Calculating AIC
calculate_AIC <- function(vars) {
  formula <-
    as.formula(paste("t$household_head_job ~ ", paste(vars, collapse = "+")))
  model <- glm(formula, data = t, family = binomial())
  AIC(model)
}

#AIC function
results_AIC <- c()
for (i in 1:3) {
  com <- combn(explanatory_var, i)
  for (j in 1:ncol(com)) {
    result <- calculate_AIC(com[,j])
    results_AIC <- c(results_AIC, result)
    names(results_AIC)[length(results_AIC)] <- paste(com[,j], collapse = "_")
  }
}

#Choosing best model
head(results_AIC[order(results_AIC)], 5)
best_AIC <- min(results_AIC)
results_AIC[which(results_AIC == best_AIC)]

######MCAR######
set.seed(123)
imp_glm_data_MCAR <- list()

system.time(for (i in seq_along(data_MCAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MCAR_list[[i]]))) {
    data <- data_MCAR_list[[i]][[j]]
    
    independent_vars <- c("household_head_age", "tot_income_enterpreneurial_act", "housing_exp")
    
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
    
    independent_vars <- c("household_head_age", "tot_income_enterpreneurial_act", "housing_exp")
    
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
    
    independent_vars <- c("household_head_age", "tot_income_enterpreneurial_act", "housing_exp")
    
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

######Choosing best glm model - var: main_source_income######
#Preparing data for test
t <- data_MCAR_list[[1]][[3]] %>% 
  select(-ID) %>% 
  as.data.frame() %>% 
  mutate_if(is.logical, as.factor)

#Defining dependent and explanatory variables
dependent_var <- t$main_source_income
explanatory_var <- c("tot_food_exp",
                     "bread_cereales_exp",
                     "rice_exp",
                     "meat_exp",
                     "fish_exp",
                     "fruit_exp",
                     "vegetables_exp",
                     "restaurant_hotels_exp",
                     "alcohol_exp",
                     "tobacco_exp",
                     "clothing_exp",
                     "housing_exp",
                     "house_rent_exp",
                     "medicare_exp",
                     "transportation_exp",
                     "communication_exp",
                     "education_exp",
                     "miscellaneous_goods_exp",
                     "special_occasions_exp",
                     "crop_farming_gardening_exp",
                     "tot_income_enterpreneurial_act",
                     "household_head_age",
                     "num_family_member",
                     "num_family_member_0_5",
                     "num_family_member_5_17",
                     "num_family_ember_employed",
                     "house_area",
                     "house_age",
                     "house_num_bedrooms",
                     "house_num_tv",
                     "house_num_cd_dvd",
                     "house_num_stereo",
                     "house_num_refrigator",
                     "num_washing_machine",
                     "house_num_airconditioner",
                     "house_num_car",
                     "house_num_landline_phone",
                     "house_num_smartphone",
                     "house_num_computer",
                     "house_num_oven",
                     "house_num_boat",
                     "house_num_motorcycle_tricycle")

#Calculating AIC
calculate_AIC <- function(vars) {
  formula <-
    as.formula(paste("t$main_source_income ~ ", paste(vars, collapse = "+")))
  model <- glm(formula, data = t, family = binomial())
  AIC(model)
}

#AIC function
results_AIC <- c()
for (i in 1:3) {
  com <- combn(explanatory_var, i)
  for (j in 1:ncol(com)) {
    result <- calculate_AIC(com[,j])
    results_AIC <- c(results_AIC, result)
    names(results_AIC)[length(results_AIC)] <- paste(com[,j], collapse = "_")
  }
}

#Choosing best model
head(results_AIC[order(results_AIC)], 5)
best_AIC <- min(results_AIC)
results_AIC[which(results_AIC == best_AIC)]

######MCAR######
set.seed(123)
imp_glm_data_MCAR <- list()

system.time(for (i in seq_along(data_MCAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MCAR_list[[i]]))) {
    data <- data_MCAR_list[[i]][[j]]
    
    independent_vars <- c("housing_exp", "tot_income_enterpreneurial_act", "transportation_exp")
    
    formula <- as.formula(paste("main_source_income ~", paste(independent_vars, collapse = " + ")))
    
    data_subset <- data[complete.cases(data[independent_vars]), ]
    
    glm_model <- glm(formula, data = data_subset, family = binomial())
    
    missing_indices <- is.na(data$main_source_income)
    predicted_values <- predict(glm_model, newdata = data[missing_indices, ], type = "response")
    imputed_values <- as.logical(ifelse(predicted_values > 0.5, TRUE, FALSE))
    
    data$main_source_income[missing_indices] <- imputed_values
    
    temp_list[[j]] <- data
  }
  imp_glm_data_MCAR[[i]] <- temp_list
})

#Saving .rds file
saveRDS(imp_glm_data_MCAR, "imp_glm1_data_MCAR.rds")

######MAR######
set.seed(123)
imp_glm_data_MAR <- list()

system.time(for (i in seq_along(data_MAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MAR_list[[i]]))) {
    data <- data_MAR_list[[i]][[j]]
    
    independent_vars <- c("housing_exp", "tot_income_enterpreneurial_act", "transportation_exp")
    
    formula <- as.formula(paste("main_source_income ~", paste(independent_vars, collapse = " + ")))
    
    data_subset <- data[complete.cases(data[independent_vars]), ]
    
    glm_model <- glm(formula, data = data_subset, family = binomial())
    
    missing_indices <- is.na(data$main_source_income)
    predicted_values <- predict(glm_model, newdata = data[missing_indices, ], type = "response")
    imputed_values <- as.logical(ifelse(predicted_values > 0.5, TRUE, FALSE))
    
    data$main_source_income[missing_indices] <- imputed_values
    
    temp_list[[j]] <- data
  }
  imp_glm_data_MAR[[i]] <- temp_list
})

#Saving .rds file
saveRDS(imp_glm_data_MAR, "imp_glm1_data_MAR.rds")

######MNAR######
set.seed(123)
imp_glm_data_MNAR <- list()

system.time(for (i in seq_along(data_MNAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MNAR_list[[i]]))) {
    data <- data_MNAR_list[[i]][[j]]
    
    independent_vars <- c("housing_exp", "tot_income_enterpreneurial_act", "transportation_exp")
    
    formula <- as.formula(paste("main_source_income ~", paste(independent_vars, collapse = " + ")))
    
    data_subset <- data[complete.cases(data[independent_vars]), ]
    
    glm_model <- glm(formula, data = data_subset, family = binomial())
    
    missing_indices <- is.na(data$main_source_income)
    predicted_values <- predict(glm_model, newdata = data[missing_indices, ], type = "response")
    imputed_values <- as.logical(ifelse(predicted_values > 0.5, TRUE, FALSE))
    
    data$main_source_income[missing_indices] <- imputed_values
    
    temp_list[[j]] <- data
  }
  imp_glm_data_MNAR[[i]] <- temp_list
})

#Saving .rds file
saveRDS(imp_glm_data_MNAR, "imp_glm1_data_MNAR.rds")

######Choosing best glm model - var: house_type_wall######
#Preparing data for test
t <- data_MCAR_list[[1]][[3]] %>% 
  select(-ID) %>% 
  as.data.frame() %>% 
  mutate_if(is.logical, as.factor)

#Defining dependent and explanatory variables
dependent_var <- t$house_type_wall
explanatory_var <- c("tot_food_exp",
                     "bread_cereales_exp",
                     "rice_exp",
                     "meat_exp",
                     "fish_exp",
                     "fruit_exp",
                     "vegetables_exp",
                     "restaurant_hotels_exp",
                     "alcohol_exp",
                     "tobacco_exp",
                     "clothing_exp",
                     "housing_exp",
                     "house_rent_exp",
                     "medicare_exp",
                     "transportation_exp",
                     "communication_exp",
                     "education_exp",
                     "miscellaneous_goods_exp",
                     "special_occasions_exp",
                     "crop_farming_gardening_exp",
                     "tot_income_enterpreneurial_act",
                     "household_head_age",
                     "num_family_member",
                     "num_family_member_0_5",
                     "num_family_member_5_17",
                     "num_family_ember_employed",
                     "house_area",
                     "house_age",
                     "house_num_bedrooms",
                     "house_num_tv",
                     "house_num_cd_dvd",
                     "house_num_stereo",
                     "house_num_refrigator",
                     "num_washing_machine",
                     "house_num_airconditioner",
                     "house_num_car",
                     "house_num_landline_phone",
                     "house_num_smartphone",
                     "house_num_computer",
                     "house_num_oven",
                     "house_num_boat",
                     "house_num_motorcycle_tricycle")

#Calculating AIC
calculate_AIC <- function(vars) {
  formula <-
    as.formula(paste("t$house_type_wall ~ ", paste(vars, collapse = "+")))
  model <- glm(formula, data = t, family = binomial())
  AIC(model)
}

#AIC function
results_AIC <- c()
for (i in 1:3) {
  com <- combn(explanatory_var, i)
  for (j in 1:ncol(com)) {
    result <- calculate_AIC(com[,j])
    results_AIC <- c(results_AIC, result)
    names(results_AIC)[length(results_AIC)] <- paste(com[,j], collapse = "_")
  }
}

#Choosing best model
head(results_AIC[order(results_AIC)], 5)
best_AIC <- min(results_AIC)
results_AIC[which(results_AIC == best_AIC)]

######MCAR######
set.seed(123)
imp_glm_data_MCAR <- list()

system.time(for (i in seq_along(data_MCAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MCAR_list[[i]]))) {
    data <- data_MCAR_list[[i]][[j]]
    
    independent_vars <- c("housing_exp", "num_family_member", "house_num_bedrooms")
    
    formula <- as.formula(paste("house_type_wall ~", paste(independent_vars, collapse = " + ")))
    
    data_subset <- data[complete.cases(data[independent_vars]), ]
    
    glm_model <- glm(formula, data = data_subset, family = binomial())
    
    missing_indices <- is.na(data$house_type_wall)
    predicted_values <- predict(glm_model, newdata = data[missing_indices, ], type = "response")
    imputed_values <- as.logical(ifelse(predicted_values > 0.5, TRUE, FALSE))
    
    data$house_type_wall[missing_indices] <- imputed_values
    
    temp_list[[j]] <- data
  }
  imp_glm_data_MCAR[[i]] <- temp_list
})

#Saving .rds file
saveRDS(imp_glm_data_MCAR, "imp_glm2_data_MCAR.rds")

######MAR######
set.seed(123)
imp_glm_data_MAR <- list()

system.time(for (i in seq_along(data_MAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MAR_list[[i]]))) {
    data <- data_MAR_list[[i]][[j]]
    
    independent_vars <- c("housing_exp", "num_family_member", "house_num_bedrooms")
    
    formula <- as.formula(paste("house_type_wall ~", paste(independent_vars, collapse = " + ")))
    
    data_subset <- data[complete.cases(data[independent_vars]), ]
    
    glm_model <- glm(formula, data = data_subset, family = binomial())
    
    missing_indices <- is.na(data$house_type_wall)
    predicted_values <- predict(glm_model, newdata = data[missing_indices, ], type = "response")
    imputed_values <- as.logical(ifelse(predicted_values > 0.5, TRUE, FALSE))
    
    data$house_type_wall[missing_indices] <- imputed_values
    
    temp_list[[j]] <- data
  }
  imp_glm_data_MAR[[i]] <- temp_list
})

#Saving .rds file
saveRDS(imp_glm_data_MAR, "imp_glm2_data_MAR.rds")

######MNAR######
set.seed(123)
imp_glm_data_MNAR <- list()

system.time(for (i in seq_along(data_MNAR_list)) {
  temp_list <- list()
  for (j in seq_len(length(data_MNAR_list[[i]]))) {
    data <- data_MNAR_list[[i]][[j]]
    
    independent_vars <- c("housing_exp", "num_family_member", "house_num_bedrooms")
    
    formula <- as.formula(paste("house_type_wall ~", paste(independent_vars, collapse = " + ")))
    
    data_subset <- data[complete.cases(data[independent_vars]), ]
    
    glm_model <- glm(formula, data = data_subset, family = binomial())
    
    missing_indices <- is.na(data$house_type_wall)
    predicted_values <- predict(glm_model, newdata = data[missing_indices, ], type = "response")
    imputed_values <- as.logical(ifelse(predicted_values > 0.5, TRUE, FALSE))
    
    data$house_type_wall[missing_indices] <- imputed_values
    
    temp_list[[j]] <- data
  }
  imp_glm_data_MNAR[[i]] <- temp_list
})

#Saving .rds file
saveRDS(imp_glm_data_MNAR, "imp_glm2_data_MNAR.rds")

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