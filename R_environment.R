#####Packages#####

#Installing and loading packages
packages <- c(
  "Amelia",
  "boot",
  "bootstrap",
  "coin",
  "corrplot",
  "dplyr",
  "ggplot2",
  "lattice",
  "MASS",
  "Metrics",
  "mi",
  "mice",
  "missForest",
  "missMethods",
  "mltools",
  "nnet",
  "parallel",
  "perm",
  "RColorBrewer",
  "readxl",
  "reshape2",
  "RVAideMemoire",
  "sampling",
  "simputation",
  "sqldf",
  "tidyverse",
  "VIM",
  "viridis",
  "writexl")

for (package_name in packages) {
  if (!require(package_name, character.only = TRUE)) {
    install.packages(package_name)
    library(package_name, character.only = TRUE)
  }
}

rm(package_name, packages)

#####Data#####

#Loading dataset
data <- read_excel("data.xlsx")

#Defining class of variables
data$ID <- as.character(data$ID)
data$tot_household_income <- as.numeric(data$tot_household_income)
data$region <- as.factor(data$region)
data$tot_food_exp <- as.numeric(data$tot_food_exp)
data$main_source_income <- as.factor(data$main_source_income)
data$household_agricultural_indic <- as.factor(data$household_agricultural_indic)
data$bread_cereales_exp <- as.numeric(data$bread_cereales_exp)
data$rice_exp <- as.numeric(data$rice_exp)
data$meat_exp <- as.numeric(data$meat_exp)
data$fish_exp <- as.numeric(data$fish_exp)
data$fruit_exp <- as.numeric(data$fruit_exp)
data$vegetables_exp <- as.numeric(data$vegetables_exp)
data$restaurant_hotels_exp <- as.numeric(data$restaurant_hotels_exp)
data$alcohol_exp <- as.numeric(data$alcohol_exp)
data$tobacco_exp <- as.numeric(data$tobacco_exp)
data$clothing_exp <- as.numeric(data$clothing_exp)
data$housing_exp <- as.numeric(data$housing_exp)
data$house_rent_exp <- as.numeric(data$house_rent_exp)
data$medicare_exp <- as.numeric(data$medicare_exp)
data$transportation_exp <- as.numeric(data$transportation_exp)
data$communication_exp <- as.numeric(data$communication_exp)
data$education_exp <- as.numeric(data$education_exp)
data$miscellaneous_goods_exp <- as.numeric(data$miscellaneous_goods_exp)
data$special_occasions_exp <- as.numeric(data$special_occasions_exp)
data$crop_farming_gardening_exp <- as.numeric(data$crop_farming_gardening_exp)
data$tot_income_enterpreneurial_act <- as.numeric(data$tot_income_enterpreneurial_act)
data$household_head_sex <- as.factor(data$household_head_sex)
data$household_head_age <- as.numeric(data$household_head_age)
data$household_head_martial_status <- as.factor(data$household_head_martial_status)
data$household_head_education <- as.factor(data$household_head_education)
data$household_head_job <- as.logical(as.numeric(data$household_head_job))
data$type_household <- as.factor(data$type_household)
data$num_family_member <- as.numeric(data$num_family_member)
data$num_family_member_0_5 <- as.numeric(data$num_family_member_0_5)
data$num_family_member_5_17 <- as.numeric(data$num_family_member_5_17)
data$num_family_member_employed <- as.numeric(data$num_family_ember_employed)
data$house_type <- as.factor(data$house_type)
data$house_type_roof <- as.factor(data$house_type_roof)
data$house_type_wall <- as.factor(data$house_type_wall)
data$house_area <- as.numeric(data$house_area)
data$house_age <- as.numeric(data$house_age)
data$house_num_bedrooms <- as.numeric(data$house_num_bedrooms)
data$house_tenure_status <- as.factor(data$house_tenure_status)
data$house_toilet_facilities <- as.factor(data$house_toilet_facilities)
data$house_electricity <- as.logical(data$house_electricity)
data$main_source_water_supply <- as.factor(data$main_source_water_supply)
data$house_num_tv <- as.numeric(data$house_num_tv)
data$house_num_cd_dvd <- as.numeric(data$house_num_cd_dvd)
data$house_num_stereo <- as.numeric(data$house_num_stereo)
data$house_num_refrigator <- as.numeric(data$house_num_refrigator)
data$num_washing_machine <- as.numeric(data$num_washing_machine)
data$house_num_airconditioner <- as.numeric(data$house_num_airconditioner)
data$house_num_car <- as.numeric(data$house_num_car)
data$house_num_landline_phone <- as.numeric(data$house_num_landline_phone)
data$house_num_smartphone <- as.numeric(data$house_num_smartphone)
data$house_num_computer <- as.numeric(data$house_num_computer)
data$house_num_oven <- as.numeric(data$house_num_oven)
data$house_num_boat <- as.numeric(data$house_num_boat)
data$house_num_motorcycle_tricycle <- as.numeric(data$house_num_motorcycle_tricycle)

#Loading data samples
data_samples_MCAR <- readRDS("data_samples_MCAR.rds")
data_samples_MAR <- readRDS("data_samples_MAR.rds")
data_samples_MNAR <- readRDS("data_samples_MNAR.rds")

#Reordering levels - var: house_type_wall
proper_order <- c("Not applicable", "Salvaged", "Very Light", "Light", "Quite strong", "Strong")

#Data list
#MCAR
for (i in seq_along(data_MCAR_list)) {
  for (j in seq_along(data_MCAR_list[[i]])) {
    data_MCAR_list[[i]][[j]]$house_type_wall <- factor(data_MCAR_list[[i]][[j]]$house_type_wall, levels = proper_order)
  }
}

#MAR
for (i in seq_along(data_MAR_list)) {
  for (j in seq_along(data_MAR_list[[i]])) {
    data_MAR_list[[i]][[j]]$house_type_wall <- factor(data_MAR_list[[i]][[j]]$house_type_wall, levels = proper_order)
  }
}

#MNAR
for (i in seq_along(data_MNAR_list)) {
  for (j in seq_along(data_MNAR_list[[i]])) {
    data_MNAR_list[[i]][[j]]$house_type_wall <- factor(data_MNAR_list[[i]][[j]]$house_type_wall, levels = proper_order)
  }
}

#Data samples
#MCAR
for (i in seq_along(data_samples_MCAR)) {
  data_samples_MCAR[[i]]$house_type_wall <- factor(data_samples_MCAR[[i]]$house_type_wall, levels = proper_order)
}

#MAR
for (i in seq_along(data_samples_MAR)) {
  data_samples_MAR[[i]]$house_type_wall <- factor(data_samples_MAR[[i]]$house_type_wall, levels = proper_order)
}

#MNAR
for (i in seq_along(data_samples_MNAR)) {
  data_samples_MNAR[[i]]$house_type_wall <- factor(data_samples_MNAR[[i]]$house_type_wall, levels = proper_order)
}

#####Importing datasets after imputation#####
readRDS("imp_median_data_MCAR.rds") -> imp_median_data_MCAR
readRDS("imp_median_data_MAR.rds") -> imp_median_data_MAR
readRDS("imp_median_data_MNAR.rds") -> imp_median_data_MNAR
readRDS("imp_hot_deck_data_MCAR.rds") -> imp_hotdeck_data_MCAR
readRDS("imp_hot_deck_data_MAR.rds") -> imp_hotdeck_data_MAR
readRDS("imp_hot_deck_data_MNAR.rds") -> imp_hotdeck_data_MNAR
readRDS("imp_kNN_data_MCAR.rds") -> imp_kNN_data_MCAR
readRDS("imp_kNN_data_MAR.rds") -> imp_kNN_data_MAR
readRDS("imp_kNN_data_MNAR.rds") -> imp_kNN_data_MNAR
readRDS("imp_reg_data_MCAR.rds") -> imp_reg_data_MCAR
readRDS("imp_reg_data_MAR.rds") -> imp_reg_data_MAR
readRDS("imp_reg_data_MNAR.rds") -> imp_reg_data_MNAR
readRDS("imp_rf_data_MCAR.rds") -> imp_rf_data_MCAR
readRDS("imp_rf_data_MAR.rds") -> imp_rf_data_MAR
readRDS("imp_rf_data_MNAR.rds") -> imp_rf_data_MNAR
readRDS("imp_mul_data_MCAR.rds") -> imp_mul_data_MCAR
readRDS("imp_mul_data_MAR.rds") -> imp_mul_data_MAR
readRDS("imp_mul_data_MNAR.rds") -> imp_mul_data_MNAR