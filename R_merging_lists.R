#MCAR
#Importing datasets
imp_lm_data_MCAR <- readRDS("C:/Dane/Studia/S II/Praca magisterska/R/datasets after imputation/imp_lm_data_MCAR.rds")
imp_glm_data_MCAR <- readRDS("C:/Dane/Studia/S II/Praca magisterska/R/datasets after imputation/imp_glm_data_MCAR.rds")
imp_multinom_data_MCAR <- readRDS("C:/Dane/Studia/S II/Praca magisterska/R/datasets after imputation/imp_multinom_data_MCAR.rds")
imp_polr_data_MCAR <- readRDS("C:/Dane/Studia/S II/Praca magisterska/R/datasets after imputation/imp_polr_data_MCAR.rds")

#Merging datasets
imp_reg_data_MCAR <- list()

for (i in seq_along(imp_lm_data_MCAR)) {
  imp_reg_data_MCAR[[i]] <- list()
  
  for (j in seq_along(imp_lm_data_MCAR[[i]])) {
    lm_col <- imp_lm_data_MCAR[[i]][[j]]$tot_household_income
    glm_col <- imp_glm_data_MCAR[[i]][[j]]$household_head_job
    multinom_col <- imp_multinom_data_MCAR[[i]][[j]]$main_source_income
    polr_col <- imp_polr_data_MCAR[[i]][[j]]$house_type_wall
    
    reg_df <- data.frame(tot_household_income = lm_col,
                         household_head_job = glm_col,
                         main_source_income = multinom_col,
                         house_type_wall = polr_col)
    
    imp_reg_data_MCAR[[i]][[j]] <- reg_df
  }
}

#Checking results
table_result <- table(imp_reg_data_MCAR[[14]][[100]]$tot_household_income - imp_lm_data_MCAR[[14]][[100]]$tot_household_income)[1]
summary_result1 <- summary(imp_reg_data_MCAR[[14]][[100]]$household_head_job == imp_glm_data_MCAR[[14]][[100]]$household_head_job)["TRUE"]
summary_result2 <- summary(imp_reg_data_MCAR[[14]][[100]]$main_source_income == imp_multinom_data_MCAR[[14]][[100]]$main_source_income)["TRUE"]
summary_result3 <- summary(imp_reg_data_MCAR[[14]][[100]]$house_type_wall == imp_polr_data_MCAR[[14]][[100]]$house_type_wall)["TRUE"]

if (table_result == 2077 && summary_result1 == 2077 && summary_result2 == 2077 && summary_result3 == 2077) {
  cat("Validation success!")
} else {
  cat("Validation failed.")
}

#MAR
#Importing datasets
imp_lm_data_MAR <- readRDS("C:/Dane/Studia/S II/Praca magisterska/R/datasets after imputation/imp_lm_data_MAR.rds")
imp_glm_data_MAR <- readRDS("C:/Dane/Studia/S II/Praca magisterska/R/datasets after imputation/imp_glm_data_MAR.rds")
imp_multinom_data_MAR <- readRDS("C:/Dane/Studia/S II/Praca magisterska/R/datasets after imputation/imp_multinom_data_MAR.rds")
imp_polr_data_MAR <- readRDS("C:/Dane/Studia/S II/Praca magisterska/R/datasets after imputation/imp_polr_data_MAR.rds")

#Merging datasets
imp_reg_data_MAR <- list()

for (i in seq_along(imp_lm_data_MAR)) {
  imp_reg_data_MAR[[i]] <- list()
  
  for (j in seq_along(imp_lm_data_MAR[[i]])) {
    lm_col <- imp_lm_data_MAR[[i]][[j]]$tot_household_income
    glm_col <- imp_glm_data_MAR[[i]][[j]]$household_head_job
    multinom_col <- imp_multinom_data_MAR[[i]][[j]]$main_source_income
    polr_col <- imp_polr_data_MAR[[i]][[j]]$house_type_wall
    
    reg_df <- data.frame(tot_household_income = lm_col,
                         household_head_job = glm_col,
                         main_source_income = multinom_col,
                         house_type_wall = polr_col)
    
    imp_reg_data_MAR[[i]][[j]] <- reg_df
  }
}

#Checking results
table_result <- table(imp_reg_data_MAR[[14]][[100]]$tot_household_income - imp_lm_data_MAR[[14]][[100]]$tot_household_income)[1]
summary_result1 <- summary(imp_reg_data_MAR[[14]][[100]]$household_head_job == imp_glm_data_MAR[[14]][[100]]$household_head_job)["TRUE"]
summary_result2 <- summary(imp_reg_data_MAR[[14]][[100]]$main_source_income == imp_multinom_data_MAR[[14]][[100]]$main_source_income)["TRUE"]
summary_result3 <- summary(imp_reg_data_MAR[[14]][[100]]$house_type_wall == imp_polr_data_MAR[[14]][[100]]$house_type_wall)["TRUE"]

if (table_result == 2077 && summary_result1 == 2077 && summary_result2 == 2077 && summary_result3 == 2077) {
  cat("Validation success!")
} else {
  cat("Validation failed.")
}

#MNAR
#Importing datasets
imp_lm_data_MNAR <- readRDS("C:/Dane/Studia/S II/Praca magisterska/R/datasets after imputation/imp_lm_data_MNAR.rds")
imp_glm_data_MNAR <- readRDS("C:/Dane/Studia/S II/Praca magisterska/R/datasets after imputation/imp_glm_data_MNAR.rds")
imp_multinom_data_MNAR <- readRDS("C:/Dane/Studia/S II/Praca magisterska/R/datasets after imputation/imp_multinom_data_MNAR.rds")
imp_polr_data_MNAR <- readRDS("C:/Dane/Studia/S II/Praca magisterska/R/datasets after imputation/imp_polr_data_MNAR.rds")

#Merging datasets
imp_reg_data_MNAR <- list()

for (i in seq_along(imp_lm_data_MNAR)) {
  imp_reg_data_MNAR[[i]] <- list()
  
  for (j in seq_along(imp_lm_data_MNAR[[i]])) {
    lm_col <- imp_lm_data_MNAR[[i]][[j]]$tot_household_income
    glm_col <- imp_glm_data_MNAR[[i]][[j]]$household_head_job
    multinom_col <- imp_multinom_data_MNAR[[i]][[j]]$main_source_income
    polr_col <- imp_polr_data_MNAR[[i]][[j]]$house_type_wall
    
    reg_df <- data.frame(tot_household_income = lm_col,
                         household_head_job = glm_col,
                         main_source_income = multinom_col,
                         house_type_wall = polr_col)
    
    imp_reg_data_MNAR[[i]][[j]] <- reg_df
  }
}

#Checking results
table_result <- table(imp_reg_data_MNAR[[14]][[100]]$tot_household_income - imp_lm_data_MNAR[[14]][[100]]$tot_household_income)[1]
summary_result1 <- summary(imp_reg_data_MNAR[[14]][[100]]$household_head_job == imp_glm_data_MNAR[[14]][[100]]$household_head_job)["TRUE"]
summary_result2 <- summary(imp_reg_data_MNAR[[14]][[100]]$main_source_income == imp_multinom_data_MNAR[[14]][[100]]$main_source_income)["TRUE"]
summary_result3 <- summary(imp_reg_data_MNAR[[14]][[100]]$house_type_wall == imp_polr_data_MNAR[[14]][[100]]$house_type_wall)["TRUE"]

if (table_result == 2077 && summary_result1 == 2077 && summary_result2 == 2077 && summary_result3 == 2077) {
  cat("Validation success!")
} else {
  cat("Validation failed.")
}