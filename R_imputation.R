#####Median imputation#####

######MCAR######
set.seed(123)
system.time(imp_median_data_MCAR <- map(unlist(data_MCAR_list, recursive = FALSE), ~ .x %>% 
                                               mutate(across(where(is.numeric), 
                                                             ~replace_na(., median(., na.rm = TRUE))))))

saveRDS(imp_median_data_MCAR, "imp_median_data_MCAR.rds")

######MAR######
set.seed(123)
system.time(imp_median_data_MAR <- map(unlist(data_MAR_list, recursive = FALSE), ~ .x %>% 
                                              mutate(across(where(is.numeric), 
                                                            ~replace_na(., median(., na.rm = TRUE))))))

saveRDS(imp_median_data_MAR, "imp_median_data_MAR.rds")

######MNAR######
set.seed(123)
system.time(imp_median_data_MNAR <- map(unlist(data_MNAR_list, recursive = FALSE), ~ .x %>% 
                                               mutate(across(where(is.numeric), 
                                                             ~replace_na(., median(., na.rm = TRUE))))))

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

######MCAR######
imp_reg_data_MCAR_list <-
  
######MNAR######
imp_reg_data_MAR_list <-

######MNAR######
imp_reg_data_MNAR_list <-

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