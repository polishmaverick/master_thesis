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