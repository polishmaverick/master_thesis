#####
#Cleaning data

#View the number of rows in the dataset
nrow(data)

#Add ID to each observation in the dataset
data <- tibble::rowid_to_column(data, "ID")

#Find variables with missing values
colnames(data)[colSums(is.na(data)) > 0]
mv <- c(which(colnames(data) == "Household Head Occupation"),
        which(colnames(data) == "Household Head Class of Worker"))

#Remove variables with missing values
data <- data[,-mv]

#Print first 10 rows in the dataset without missing values
head(data, 10)

#Print variables of dataset
ls(data)

#Change values in some variables

#Change values in variable region
data[data == "I - Ilocos Region"] <- "Ilocos Region"
data[data == "II - Cagayan Valley"] <- "Cagayan Valley"
data[data == "III - Central Luzon"] <- "Central Luzon"
data[data == "IVA - CALABARZON"] <- "CALABARZON"
data[data == "IVB - MIMAROPA"] <- "MIMAROPA"
data[data == "V - Bicol Region"] <- "Bicol Regio"
data[data == "VI - Western Visayas"] <- "Western Visayas"
data[data == "VII - Central Visayas"] <- "Central Visayas"
data[data == "VIII - Eastern Visayas"] <- "Eastern Visayas"
data[data == "IX - Zasmboanga Peninsula"] <- "Zamboanga Peninsula"
data[data == "X - Northern Mindanao"] <- "Northern Mindanao"
data[data == "XI - Davao Region"] <- "Davao Region"
data[data == "XII - SOCCSKSARGEN"] <- "SOCCSKSARGEN"
data[data == " ARMM"] <- "BARMM"
data[data == "NCR"] <- "NCR"
data[data == "CAR"] <- "CAR"
data[data == "Caraga"] <- "Caraga"

#Change values in variable main_source_income
data[data == "Wage/Salaries"] <- "Salary"
data[data == "Other sources of Income"] <- "Other sources of income"
data[data == "Enterpreneurial Activities"] <- "Enterpreneurial activities"

#Change values in variable household_head_job
data[data == "With Job/Business"] <- "1"
data[data == "No Job/Business"] <- "0"

#Change values in variable type_household
data[data == "Extended Family"] <- "Extended family"
data[data == "Single Family"] <- "Single family"
data[data == "Two or More Nonrelated Persons/Members"] <- "Nonrelated persons"

#Change values in variable house_type_wall
data[data == "Quite Strong"] <- "Quite strong"
data[data == "NOt applicable"] <- "Not applicable"

#Prepare vector of new column names
new_colnames <- c("ID",
                  "tot_household_income",
                  "region",
                  "tot_food_exp",
                  "main_source_income",
                  "household_agricultural_indic",
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
                  "household_head_sex",
                  "household_head_age",
                  "household_head_martial_status",
                  "household_head_education",
                  "household_head_job",
                  "type_household",
                  "num_family_member",
                  "num_family_member_0_5",
                  "num_family_member_5_17",
                  "num_family_ember_employed",
                  "house_type",
                  "house_type_roof",
                  "house_type_wall",
                  "house_area",
                  "house_age",
                  "house_num_bedrooms",
                  "house_tenure_status",
                  "house_toilet_facilities",
                  "house_electricity",
                  "main_source_water_supply",
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

#Change column names with for loop
for(i in 1:ncol(data))
{ 
  #Replace column names
  colnames(data)[i] <- new_colnames[i]
  #Check column names
  print(colnames(data))
}