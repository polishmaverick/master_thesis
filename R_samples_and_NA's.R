######Prepation for producing NA's#####

#Declaring number of iterations
n <- 100

#Defining vector of proportions of missing values
prop <- c(seq(0.05, 0.7, 0.05))

#Creating empty lists
data_MCAR_list <- list()
data_MAR_list <- list()
data_MNAR_list <- list()

#Defining parameter values of function producing missing values

#Choosing all types of variables
numeric_NA <- c(which(names(data) == "tot_household_income"))
ordinal_NA <- grep("house_type_wall", names(data))
factor_NA <- grep("main_source_income", names(data))
logical_NA <- grep("household_head_job", names(data))

#Defining cols_mis value
cols_mis <- c(numeric_NA, ordinal_NA, factor_NA, logical_NA)

#Defining cols_ctrl value
setdiff(c(2:(length(data)-1)), cols_mis)
set.seed(123)
cols_ctrl <- sample(setdiff(c(2:(length(data)-1)), cols_mis), length(cols_mis))

#####Producing NA's - type: MCAR#####

#Set.seed for the same results
set.seed(123)

#Generate 100 samples
data_samples_MCAR <- lapply(1:n, function(i) {
  data_s <- sample(nrow(data), round(0.05 * nrow(data)))
  data_sample <- data[data_s, ]
  return(data_sample)
})

#Saving .rds file
#saveRDS(data_samples_MCAR, "data_samples_MCAR.rds")

#Generating datasets with missing values type MCAR
for (p in prop) {
  data_MCAR <- lapply(data_samples_MCAR, function(data_samples_MCAR)
  {
    data_missing <- delete_MCAR(ds = data_samples_MCAR,
                                p  = p,
                                cols_mis = cols_mis)
    return(data_missing)
  })
  data_MCAR_list[[as.character(p * 100)]] <- data_MCAR
}

#Saving .rds file
#saveRDS(data_MCAR_list, "data_MCAR_list.rds")

#####Producing NA's - type: MAR#####

#Set.seed for the same results
set.seed(123)

#Generating 100 samples
data_samples_MAR <- lapply(1:n, function(i) {
  data_s <- sample(nrow(data), round(0.05 * nrow(data)))
  data_sample <- data[data_s, ]
  return(data_sample)
})

#Saving .rds file
#saveRDS(data_samples_MAR, "data_samples_MAR.rds")

#Loop - MAR
for (p in prop) {
  data_MAR <- lapply(data_samples_MAR, function(data_samples_MAR)
  {
    data_missing <- delete_MAR_1_to_x(ds = data_samples_MAR,
                                      p  = p,
                                      cols_mis = cols_mis,
                                      cols_ctrl = cols_ctrl,
                                      x = 0.65)
    return(data_missing)
  })
  data_MAR_list[[as.character(p * 100)]] <- data_MAR
}

#Saving .rds file
#saveRDS(data_MAR_list, "data_MAR_list.rds")

#####Producing NA's - type: MNAR#####

#Set.seed for the same results
set.seed(123)

#Generating 1000 samples
data_samples_MNAR <- lapply(1:n, function(i) {
  data_s <- sample(nrow(data), round(0.05 * nrow(data)))
  data_sample <- data[data_s, ]
  return(data_sample)
})

#Saving .rds file
#saveRDS(data_samples_MNAR, "data_samples_MNAR.rds")

#Loop - MNAR
for (p in prop) {
  data_MNAR <- lapply(data_samples_MNAR, function(data_samples_MNAR)
  {
    data_missing <- delete_MNAR_1_to_x(ds = data_samples_MNAR,
                                       p  = p,
                                       cols_mis = cols_mis,
                                       cols_ctrl = cols_ctrl,
                                       x = 0.65)
    return(data_missing)
  })
  data_MNAR_list[[as.character(p * 100)]] <- data_MNAR
}

#Saving .rds file
#saveRDS(data_MNAR_list, "data_MNAR_list.rds")