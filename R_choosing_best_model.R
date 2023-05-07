######Defining model test variables######
#Defining explanatory variables
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

#####Choosing best linear regression model - var: tot_household_income#####

#Preparing function for calculating AIC
calculate_AIC <- function(vars) {
  formula <-
    as.formula(paste("t$tot_household_income ~ ", paste(vars, collapse = "+")))
  model <- lm(formula, data = t)
  AIC(model)
}

######MCAR######
#Preparing data for test
t <- data_MCAR_list[[14]][[3]] %>% 
  dplyr::select(-ID) %>% 
  as.data.frame() %>% 
  mutate_if(is.logical, as.factor)

#Defining dependent variable
dependent_var <- t$tot_household_income

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

#housing_exp_miscellaneous_goods_exp_tot_income_enterpreneurial_act

######MAR######
#Preparing data for test
t <- data_MAR_list[[14]][[3]] %>% 
  dplyr::select(-ID) %>% 
  as.data.frame() %>% 
  mutate_if(is.logical, as.factor)

#Defining dependent variable
dependent_var <- t$tot_household_income

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

#tot_food_exp_housing_exp_miscellaneous_goods_exp

######MNAR######
#Preparing data for test
t <- data_MNAR_list[[14]][[3]] %>% 
  dplyr::select(-ID) %>% 
  as.data.frame() %>% 
  mutate_if(is.logical, as.factor)

#Defining dependent variable
dependent_var <- t$tot_household_income

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

#housing_exp_miscellaneous_goods_exp_tot_income_enterpreneurial_act

#####Choosing best logistic regression model - var: household_head_job#####

#Preparing function for calculating AIC
calculate_AIC <- function(vars) {
  formula <-
    as.formula(paste("t$household_head_job ~ ", paste(vars, collapse = "+")))
  model <- glm(formula, data = t, family = binomial())
  AIC(model)
}

######MCAR######
#Preparing data for test
t <- data_MCAR_list[[14]][[3]] %>% 
  dplyr::select(-ID) %>% 
  as.data.frame() %>% 
  mutate_if(is.logical, as.factor)

#Defining dependent variable
dependent_var <- t$household_head_job

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

#tot_income_enterpreneurial_act_household_head_age_num_washing_machine 

######MAR######
#Preparing data for test
t <- data_MAR_list[[14]][[3]] %>% 
  dplyr::select(-ID) %>% 
  as.data.frame() %>% 
  mutate_if(is.logical, as.factor)

#Defining dependent variable
dependent_var <- t$household_head_job

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

#household_head_age_num_family_ember_employed_house_num_tv

######MNAR######
#Preparing data for test
t <- data_MNAR_list[[14]][[3]] %>% 
  dplyr::select(-ID) %>% 
  as.data.frame() %>% 
  mutate_if(is.logical, as.factor)

#Defining dependent variable
dependent_var <- t$household_head_job

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

#housing_exp_tot_income_enterpreneurial_act_household_head_age

#####Choosing best multinomial logistic regression model - var: main_source_income#####

#Preparing function for calculating AIC
calculate_AIC <- function(vars) {
  formula <-
    as.formula(paste("main_source_income ~ ", paste(vars, collapse = "+")))
  model <- nnet::multinom(formula, data = t, trace = FALSE)
  AIC(model)
}

######MCAR######
#Preparing data for test
t <- data_MCAR_list[[14]][[3]] %>% 
  dplyr::select(-ID) %>% 
  as.data.frame() %>% 
  mutate_if(is.logical, as.factor)

#Defining dependent variable
dependent_var <- t$main_source_income

#AIC function
results_AIC <- c()
for (i in 1:3) {
  com <- combn(explanatory_var, i)
  for (j in 1:ncol(com)) {
    result <- calculate_AIC(com[, j])
    results_AIC <- c(results_AIC, result)
    names(results_AIC)[length(results_AIC)] <- paste(com[, j], collapse = "_")
  }
}

#Choosing best model
head(results_AIC[order(results_AIC)], 5)
best_AIC <- min(results_AIC)
results_AIC[which(results_AIC == best_AIC)]

#housing_exp_tot_income_enterpreneurial_act_num_family_ember_employed

######MAR######
#Preparing data for test
t <- data_MAR_list[[14]][[3]] %>% 
  dplyr::select(-ID) %>% 
  as.data.frame() %>% 
  mutate_if(is.logical, as.factor)

#Defining dependent variable
dependent_var <- t$main_source_income

#AIC function
results_AIC <- c()
for (i in 1:3) {
  com <- combn(explanatory_var, i)
  for (j in 1:ncol(com)) {
    result <- calculate_AIC(com[, j])
    results_AIC <- c(results_AIC, result)
    names(results_AIC)[length(results_AIC)] <- paste(com[, j], collapse = "_")
  }
}

#Choosing best model
head(results_AIC[order(results_AIC)], 5)
best_AIC <- min(results_AIC)
results_AIC[which(results_AIC == best_AIC)]

#housing_exp_tot_income_enterpreneurial_act_num_family_ember_employed 

######MNAR######
#Preparing data for test
t <- data_MNAR_list[[14]][[3]] %>% 
  dplyr::select(-ID) %>% 
  as.data.frame() %>% 
  mutate_if(is.logical, as.factor)

#Defining dependent variable
dependent_var <- t$main_source_income

#AIC function
results_AIC <- c()
for (i in 1:3) {
  com <- combn(explanatory_var, i)
  for (j in 1:ncol(com)) {
    result <- calculate_AIC(com[, j])
    results_AIC <- c(results_AIC, result)
    names(results_AIC)[length(results_AIC)] <- paste(com[, j], collapse = "_")
  }
}

#Choosing best model
head(results_AIC[order(results_AIC)], 5)
best_AIC <- min(results_AIC)
results_AIC[which(results_AIC == best_AIC)]

#meat_exp_tot_income_enterpreneurial_act_num_family_ember_employed

#####Choosing best ordinal logistic regression model - var: house_type_wall#####

#Preparing function for calculating AIC
calculate_AIC <- function(vars) {
  formula <-
    as.formula(paste("house_type_wall ~ ", paste(vars, collapse = "+")))
  model <- MASS::polr(formula, data = t, Hess = TRUE)
  AIC(model)
}

######MCAR######
#Preparing data for test
t <- data_MCAR_list[[14]][[3]] %>% 
  dplyr::select(-ID) %>% 
  as.data.frame() %>% 
  mutate_if(is.logical, as.factor)

#Defining dependent variable
dependent_var <- t$house_type_wall

#AIC function
results_AIC <- c()
for (i in 1:3) {
  com <- combn(explanatory_var, i)
  for (j in 1:ncol(com)) {
    result <- calculate_AIC(com[, j])
    results_AIC <- c(results_AIC, result)
    names(results_AIC)[length(results_AIC)] <- paste(com[, j], collapse = "_")
  }
}

#Choosing best model
head(results_AIC[order(results_AIC)], 5)
best_AIC <- min(results_AIC)
results_AIC[which(results_AIC == best_AIC)]

#

######MAR######
#Preparing data for test
t <- data_MAR_list[[14]][[3]] %>% 
  dplyr::select(-ID) %>% 
  as.data.frame() %>% 
  mutate_if(is.logical, as.factor)

#Defining dependent variable
dependent_var <- t$house_type_wall

#Choosing best model
head(results_AIC[order(results_AIC)], 5)
best_AIC <- min(results_AIC)
results_AIC[which(results_AIC == best_AIC)]

#

######MNAR######
#Preparing data for test
t <- data_MNAR_list[[14]][[3]] %>% 
  dplyr::select(-ID) %>% 
  as.data.frame() %>% 
  mutate_if(is.logical, as.factor)

#Defining dependent variable
dependent_var <- t$house_type_wall

#AIC function
results_AIC <- c()
for (i in 1:3) {
  com <- combn(explanatory_var, i)
  for (j in 1:ncol(com)) {
    result <- calculate_AIC(com[, j])
    results_AIC <- c(results_AIC, result)
    names(results_AIC)[length(results_AIC)] <- paste(com[, j], collapse = "_")
  }
}

#Choosing best model
head(results_AIC[order(results_AIC)], 5)
best_AIC <- min(results_AIC)
results_AIC[which(results_AIC == best_AIC)]

#
#####Summary#####

#lm
tot_household_income ~ housing_exp + miscellaneous_goods_exp + tot_income_enterpreneurial_act
#housing_exp miscellaneous_goods_exp tot_income_enterpreneurial_act

tot_household_income ~ tot_food_exp + housing_exp + miscellaneous_goods_exp
#tot_food_exp housing_exp miscellaneous_goods_exp

tot_household_income ~ housing_exp + miscellaneous_goods_exp + tot_income_enterpreneurial_act
#housing_exp miscellaneous_goods_exp tot_income_enterpreneurial_act

#glm
c("tot_income_enterpreneurial_act", "household_head_age", "num_washing_machine")
#tot_income_enterpreneurial_act household_head_age num_washing_machine

c("household_head_age", "num_family_member_employed", "house_num_tv")
#household_head_age num_family_member_employed house_num_tv

c("housing_exp", "tot_income_enterpreneurial_act", "household_head_age")
#housing_exp tot_income_enterpreneurial_act household_head_age

#multinom
c("housing_exp", "tot_income_enterpreneurial_act", "num_family_member_employed")
#housing_exp tot_income_enterpreneurial_act num_family_member_employed

c("housing_exp", "tot_income_enterpreneurial_act", "num_family_member_employed")
#housing_exp tot_income_enterpreneurial_act num_family_member_employed

c("meat_exp", "tot_income_enterpreneurial_act", "num_family_member_employed")
#meat_exp tot_income_enterpreneurial_act num_family_member_employed

#ordinal
c("housing_exp", "house_area_house", "num_bedrooms")
#housing_exp house_area_house num_bedrooms

c("housing_exp", "house_area_house", "num_bedrooms")
#housing_exp house_area_house num_bedrooms

c("housing_exp", "house_num_bedrooms")
#housing_exp house_num_bedrooms
#####Multiple imputation#####
methods <- c("pmm", "midastouch", "sample", "cart", "rf")

df <- data_MNAR_list[[14]][[100]]

df[] <- lapply(df, function(x) {
  if (is.numeric(x)) {
    return(x / 100000)
  } else {
    return(x)
  }
})

evaluation_results <- data.frame(Method = character(), Indicator = numeric())

for (method_name in methods) {
  method <- make.method(df)
  method["household_head_job"] <- method_name
  
  mice_mod <- mice(df, m = 1, method = method, maxit = 1, seed = 123)
  
  dfn <- complete(mice_mod, action = 1)
  
  dfn[] <- lapply(dfn, function(x) {
    if (is.numeric(x)) {
      return(x * 100000)
    } else {
      return(x)
    }
  })
  
  ind <- mape(data_samples_MNAR[[100]]$household_head_job, dfn$household_head_job)
  
  evaluation_results <- rbind(evaluation_results, data.frame(Method = method_name, Indicator = ind))
}

evaluation_results