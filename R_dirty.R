#####Plots#####

######Plot 1######

#Choosing only numeric variables
numeric_cols = c(which(names(data) == "tot_household_income"), grep("_exp", names(data)))
data_numeric <- data[, numeric_cols]

#Calculating correlations
correlations <- cor(data_numeric)

#Defining color palette
my_palette <- colorRampPalette(brewer.pal(9, "PuBuGn"))(100)

#Creating corrplot
corrplot(correlations,
         method = "color",
         type = "upper",
         order = "hclust",
         tl.col = "black",
         col = my_palette,
         addCoef.col = "black",
         cl.pos = "n",
         diag = FALSE,
         number.cex = 0.8)


#Creating data frame with correlations
corr_df <- as.data.frame(as.table(correlations))
corr_df <- data.frame(var1 = corr_df$Var1,
                      var2 = corr_df$Var2,
                      cor = corr_df$Freq)

#Sorting correlation
corr_df <- corr_df[order(-corr_df$cor), ]
corr_df

######Plot 2######

#Calculating median for each region
median_by_region <- aggregate(data$tot_household_income, by=list(data$region), median)
median_by_region <- median_by_region[order(median_by_region$x), ]

ggplot(median_by_region, aes(x = reorder(Group.1, x), y = x)) +
  geom_bar(stat = "identity",
           fill = brewer.pal(n = 8, name = "Blues")[7],
           color = "black") +
  #scale_y_continuous(labels = comma) +
  labs(x = "Region",
       y = "Median household income",
       title = NULL,
       size = 5) +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 0.5,
                                   hjust = 1,
                                   size = 24,
                                   color = "black"),
        axis.text.y = element_text(size = 24,
                                   color = "black"),
        axis.title = element_text(size = 24,
                                  color = "black",
                                  face = "bold"),
        panel.background = element_rect(fill = "white")) +
  coord_flip()

######Plot 3######

#Calculating percentage share of each category
income_counts <- table(data$main_source_income)
income_percentages <- round(100*income_counts/sum(income_counts))

#Creating labels
income_labels <- paste0(names(income_counts), "\n(", income_percentages, "%)")

#Creating plot
pie(income_counts,
    col = brewer.pal(n = 3, name = "Blues"),
    cex = 3,
    labels = income_labels)

######Missing data plots######
as.data.frame(data_MNAR_list$'60'[1])%>% aggr(combined = FALSE, numbers = TRUE, only.miss = TRUE)
as.data.frame(data_MNAR_list$'60'[1]) %>% select(household_head_sex, tot_household_income) %>% spineMiss(col = c("steelblue", "forestgreen"))
as.data.frame(data_MNAR_list$'60'[1]) %>% mosaicMiss(highlight = "tot_household_income", plotvars = c("household_head_sex", "house_electricity"))
#####lm#####

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
  formula <- as.formula(paste("t$tot_household_income ~ ", paste(vars, collapse = "+")))
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

#####glm#####

#Preparing data for test
t <- data_MNAR_list[[14]][[1]] %>% 
  select(-ID) %>% 
  as.data.frame() %>% 
  mutate_if(is.logical, as.factor)

#Building model
logreg_model <- glm(t$household_head_job ~ t$tot_income_enterpreneurial_act + t$transportation_exp, data = t, family = binomial)

#Creating predictions
preds <- predict(logreg_model, type = "response")
table(preds)
preds <- ifelse(preds >= 0.778677, 1, 0)
table(preds)

#Check
round((1-(table(preds)["0"]/table(preds)["1"])), 3)
round((1-(table(t$household_head_job)["FALSE"]/table(t$household_head_job)["TRUE"])), 3)

#####kNN - test#####
round(table(data$house_type_wall)/sum(table(data$house_type_wall)), 2)

k_values <- c(1, 2, 15)
results_list <- list()

for (i in k_values) {
  test_imp <- kNN(t, 
                  k = i, 
                  variable = c("tot_household_income", "house_type_wall", "house_type_wall", "house_type_wall"),
                  imp_var = FALSE)
  results_list[[as.character(i)]] <- table(test_imp$house_type_wall)/sum(table(test_imp$house_type_wall))
}

for (i in k_values) {
  print(paste("Results for k =", i))
  print(results_list[[as.character(i)]])
}

#kNN - test na zbiorze
imp_kNN_data_MNAR_list <- list()

for (j in seq_along(data_MCAR_list[[14]])) {
  imp_kNN_data_MNAR_list[[j]] <- kNN(data_MCAR_list[[14]][[j]], 
                                     k = 2, 
                                     variable = c("tot_household_income", "main_source_income", "household_head_job", "house_type_wall"),
                                     imp_var = FALSE)
}

#Data imputation quality evaluation - RMSE
na_type_fraction <-
  rep(
    c("MCAR median",
      "MAR median",
      "MNAR median",
      "MCAR hot-deck",
      "MAR hot-deck",
      "MNAR hot-deck",
      "MCAR kNN",
      "MAR kNN",
      "MNAR kNN",
      "MCAR regression",
      "MAR regression",
      "MNAR regression",
      "MCAR random forest",
      "MAR random forest",
      "MNAR random forest",
      "MCAR multiple",
      "MAR multiple",
      "MNAR multiple"),
    2
  )
value_size <- rep(c("min", "max"), each = 18)
value <-
  c(
    0.01,
    0.21,
    0.14,
    0.16,
    0.18,
    0.2,
    0.22,
    0.24,
    0.26,
    0.28,
    0.3,
    0.32,
    0.34,
    0.36,
    0.38,
    0.4,
    0.42,
    0.44,
    4.366666667,
    4.573333333,
    0.78,
    0.986666667,
    1.193333333,
    1.4,
    1.606666667,
    1.813333333,
    2.02,
    2.226666667,
    2.433333333,
    2.64,
    2.846666667,
    3.053333333,
    3.26,
    3.466666667,
    3.673333333,
    3.65
  )
paired <- c(rep(1:18), rep(1:18))

#Creating data frame
value_dumbbell <- data.frame(na_type_fraction, value_size, value, paired)
value_dumbbell

#Adding variable value_max
value_dumbbell <- value_dumbbell %>%
  group_by(na_type_fraction, paired) %>%
  mutate(value_max = max(value))

#Adding variable interval
value_dumbbell <- value_dumbbell %>%
  group_by(na_type_fraction, paired) %>%
  mutate(interval = max(value) - min(value))

#Colors
colors <- c("#203864", #max
            "#C40404") #min
                     
#Plot
value_dumbbell %>%
  ggplot(aes(x = value, y = reorder(na_type_fraction, interval))) +
  geom_line(aes(group = paired), color = "#000000") +
  geom_point(aes(color = value_size), size = 7, shape = 18) +
  labs(x = "Value", y = "Mechanizm powstania braków danych i technika imputacji") +
  theme_minimal() +
  scale_color_manual(values = colors, name = "MAPE size")+
  theme(text = element_text(size = 20, color = "#000000"))

#####Templates#####
mape_median_data_MCAR
mape_median_data_MAR
mape_median_data_MNAR

mape_hot_deck_data_MCAR
mape_hot_deck_data_MAR
mape_hot_deck_data_MNAR

mape_kNN_data_MCAR
mape_kNN_data_MAR
mape_kNN_data_MNAR

mape_reg_data_MCAR
mape_reg_data_MAR
mape_reg_data_MNAR

###

acc_median_data_MCAR
acc_median_data_MAR
acc_median_data_MNAR

acc_hot_deck_data_MCAR
acc_hot_deck_data_MAR
acc_hot_deck_data_MNAR

acc_kNN_data_MCAR
acc_kNN_data_MAR
acc_kNN_data_MNAR

acc_reg_data_MCAR
acc_reg_data_MAR
acc_reg_data_MNAR

acc_rf_data_MCAR
acc_rf_data_MAR
acc_rf_data_MNAR

acc_mul_data_MCAR
acc_mul_data_MAR
acc_mul_data_MNAR
mape_rf_data_MCAR
mape_rf_data_MAR
mape_rf_data_MNAR

mape_mul_data_MCAR
mape_mul_data_MAR
mape_mul_data_MNAR

#####Levelplot#####
#X axis labels
labels <- c("MCAR median",
            "MAR median",
            "MNAR median",
            "MCAR hot-deck",
            "MAR hot-deck",
            "MNAR hot-deck",
            "MCAR kNN",
            "MAR kNN",
            "MNAR kNN",
            "MCAR regression",
            "MAR regression",
            "MNAR regression",
            "MCAR random forest",
            "MAR random forest",
            "MNAR random forest",
            "MCAR multiple",
            "MAR multiple",
            "MNAR multiple")

hypothesis <- expand.grid(X = labels, Y = seq(5, 70, 5))

hypothesis <- transform(hypothesis, X = factor(X))
hypothesis$Z <- runif(nrow(hypothesis)) * 100

my_palette <- colorRampPalette(brewer.pal(9, "Blues"))(100)

#Levelplot
levelplot(Z ~ X * Y,
          data = hypothesis,
          xlab = list(label = "Mechanizm powstania braków danych i technika imputacji", fontsize = 20),
          ylab = list(label = "Frakcja braków danych (w %)", fontsize = 20),
          scales = list(x = list(rot = 45, cex = 1.4), y = list(cex = 1.4)),
          col.regions = my_palette,
          colorkey = list(labels = list(cex = 1.4),
                          at = seq(0, 100, length.out = 100), 
                          title = "% przypadków odrzucenia hipotezy H0"))