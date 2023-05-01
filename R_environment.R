#####Packages#####

#Installing and loading packages
if(require("Amelia")) {
  library("Amelia")
} else {
  install.packages("Amelia")
  library("Amelia")
}

if(require("boot")) {
  library("boot")
} else {
  install.packages("boot")
  library("boot")
}

if(require("coin")) {
  library("coin")
} else {
  install.packages("coin")
  library("coin")
}

if(require("dplyr")) {
  library("dplyr")
} else {
  install.packages("dplyr")
  library("dplyr")
}

if(require("ggplot2")) {
  library("ggplot2")
} else {
  install.packages("ggplot2")
  library("ggplot2")
}

if(require("lattice")) {
  library("lattice")
} else {
  install.packages("lattice")
  library("lattice")

if(require("Metrics")) {
  library("Metrics")
} else {
  install.packages("Metrics")
  library("Metrics")
}

if(require("mi")) {
  library("mi")
} else {
  install.packages("mi")
  library("mi")
}

if(require("mice")) {
  library("mice")
} else {
  install.packages("mice")
  library("mice")
}

if(require("missForest")) {
  library("missForest")
} else {
  install.packages("missForest")
  library("missForest")
}

if(require("missMethods")) {
  library("missMethods")
} else {
  install.packages("missMethods")
  library("missMethods")
}

if(require("mltools")) {
  library("mltools")
} else {
  install.packages("mltools")
  library("mltools")
}

if(require("perm")) {
  library("perm")
} else {
  install.packages("perm")
  library("perm")
}

if(require("RColorBrewer")) {
  library("RColorBrewer")
} else {
  install.packages("RColorBrewer")
  library("RColorBrewer")
}

if(require("readxl")) {
  library("readxl")
} else {
  install.packages("readxl")
  library("readxl")
}

if(require("RVAideMemoire")) {
  library("RVAideMemoire")
} else {
  install.packages("RVAideMemoire")
  library("RVAideMemoire")
}

if(require("sampling")) {
  library("sampling")
} else {
  install.packages("sampling")
  library("sampling")
}

if(require("simputation")) {
  library("simputation")
} else {
  install.packages("simputation")
  library("simputation")
}

if(require("sqldf")) {
  library("sqldf")
} else {
  install.packages("sqldf")
  library("sqldf")
}

if(require("tidyverse")) {
  library("tidyverse")
} else {
  install.packages("tidyverse")
  library("tidyverse")
}

if(require("VIM")) {
  library("VIM")
} else {
  install.packages("VIM")
  library("VIM")
}

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