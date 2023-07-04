#####Total household income######
######Function for extracting min and max values - MAPE######
create_dataframe_mape <- function(data) {
  
  min_value <- min(data$mean_MAPE)
  max_value <- max(data$mean_MAPE)
  
  mechanism <- sub(".*_(.*)$", "\\1", deparse(substitute(data)))
  method <- sub("^mape_(.*)_(.*)$", "\\1 \\2", deparse(substitute(data)))
  mechanism_technique <- paste(mechanism, method, sep = " ")
  mechanism_technique <- paste(strsplit(mechanism_technique, " ")[[1]][1:2], collapse = " ")
  
  df <- rbind(data.frame(mechanism_technique = mechanism_technique, value_size = "min", value = min_value, paired = 1),
              data.frame(mechanism_technique = mechanism_technique, value_size = "max", value = max_value, paired = 1))
  
  colnames(df)[3] <- "value"
  return(df)
}

######Preparing data######  
#Loading and transforming MAPE datasets
mape_median_MCAR <- readRDS("mape_median_MCAR.rds")
mape_median_MCAR_df <- create_dataframe_mape(mape_median_MCAR)

mape_median_MAR <- readRDS("mape_median_MAR.rds")
mape_median_MAR_df <- create_dataframe_mape(mape_median_MAR)

mape_median_MNAR <- readRDS("mape_median_MNAR.rds")
mape_median_MNAR_df <- create_dataframe_mape(mape_median_MNAR)

mape_hotdeck_MCAR <- readRDS("mape_hotdeck_MCAR.rds")
mape_hotdeck_MCAR_df <- create_dataframe_mape(mape_hotdeck_MCAR)

mape_hotdeck_MAR <- readRDS("mape_hotdeck_MAR.rds")
mape_hotdeck_MAR_df <- create_dataframe_mape(mape_hotdeck_MAR)

mape_hotdeck_MNAR <- readRDS("mape_hotdeck_MNAR.rds")
mape_hotdeck_MNAR_df <- create_dataframe_mape(mape_hotdeck_MNAR)

mape_kNN_MCAR <- readRDS("mape_kNN_MCAR.rds")
mape_kNN_MCAR_df <- create_dataframe_mape(mape_kNN_MCAR)

mape_kNN_MAR <- readRDS("mape_kNN_MAR.rds")
mape_kNN_MAR_df <- create_dataframe_mape(mape_kNN_MAR)

mape_kNN_MNAR <- readRDS("mape_kNN_MNAR.rds")
mape_kNN_MNAR_df <- create_dataframe_mape(mape_kNN_MNAR)

mape_reg_MCAR <- readRDS("mape_reg_MCAR.rds")
mape_reg_MCAR_df <- create_dataframe_mape(mape_reg_MCAR)

mape_reg_MAR <- readRDS("mape_reg_MAR.rds")
mape_reg_MAR_df <- create_dataframe_mape(mape_reg_MAR)

mape_reg_MNAR <- readRDS("mape_reg_MNAR.rds")
mape_reg_MNAR_df <- create_dataframe_mape(mape_reg_MNAR)

mape_rf_MCAR <- readRDS("mape_rf_MCAR.rds")
mape_rf_MCAR_df <- create_dataframe_mape(mape_rf_MCAR)

mape_rf_MAR <- readRDS("mape_rf_MAR.rds")
mape_rf_MAR_df <- create_dataframe_mape(mape_rf_MAR)

mape_rf_MNAR <- readRDS("mape_rf_MNAR.rds")
mape_rf_MNAR_df <- create_dataframe_mape(mape_rf_MNAR)

mape_mul_MCAR <- readRDS("mape_mul_MCAR.rds")
mape_mul_MCAR_df <- create_dataframe_mape(mape_mul_MCAR)

mape_mul_MAR <- readRDS("mape_mul_MAR.rds")
mape_mul_MAR_df <- create_dataframe_mape(mape_mul_MAR)

mape_mul_MNAR <- readRDS("mape_mul_MNAR.rds")
mape_mul_MNAR_df <- create_dataframe_mape(mape_mul_MNAR)

#Creating dataset for dumbell plot
df_list <- list(mape_median_MCAR_df, mape_median_MAR_df, mape_median_MNAR_df,
                mape_hotdeck_MCAR_df, mape_hotdeck_MAR_df, mape_hotdeck_MNAR_df,
                mape_kNN_MCAR_df, mape_kNN_MAR_df, mape_kNN_MNAR_df,
                mape_reg_MCAR_df, mape_reg_MAR_df, mape_reg_MNAR_df,
                mape_rf_MCAR_df, mape_rf_MAR_df, mape_rf_MNAR_df,
                mape_mul_MCAR_df, mape_mul_MAR_df, mape_mul_MNAR_df)

# Połącz wszystkie data.frame w jedną ramkę danych
value_dumbbell <- do.call(rbind, df_list)

value_dumbbell$paired <- rep(1:(36/2), each = 2)[order(rep(1:(36/2), each = 2))]

#Adding variable value_max
value_dumbbell <- value_dumbbell %>%
  group_by(mechanism_technique, paired) %>%
  mutate(value_max = max(value))

#Adding variable interval
value_dumbbell <- value_dumbbell %>%
  group_by(mechanism_technique, paired) %>%
  mutate(interval = max(value) - min(value))

######Creating plot######    
#Colors
colors <- c("#413d7b", "#348fa7")

#Labels
new_labels <- rev(c("MNAR hot-deck",
                "MAR hot-deck",
                "MNAR kNN",
                "MAR kNN",
                "MNAR mediana",
                "MAR mediana",
                "MCAR hot-deck",
                "MCAR mediana",
                "MNAR random forest",
                "MAR random forest",
                "MCAR random forest",
                "MNAR regresyjna",
                "MCAR kNN",
                "MNAR wielokrotna",
                "MAR wielokrotna",
                "MCAR wielokrotna",
                "MCAR regresyjna",
                "MAR regresyjna"))

#Plot
value_dumbbell %>%
  ggplot(aes(x = value, y = reorder(mechanism_technique, value_max))) +
  geom_line(aes(group = paired), color = "#348fa7", linewidth = 2) +
  geom_point(aes(color = value_size), size = 10, shape = 18) +
  labs(x = "MAPE (%)", y = "Mechanizm powstania braków\ndanych i metoda imputacji") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 160, 20)) +
  scale_y_discrete(labels = new_labels) +
  scale_color_manual(values = colors, name = "Wartość w grupie\n(mechanizm i metoda)") +
  theme(text = element_text(size = 20, color = "#000000"),
        axis.text = element_text(size = 25, color = "#000000"),
        axis.title = element_text(size = 25, color = "#000000"))
#####Household head job######
######Function for extracting min and max values - Accuracy######
create_dataframe_acc <- function(data) {
  
  min_value <- min(data$mean_acc)
  max_value <- max(data$mean_acc)
  
  mechanism <- sub(".*_(.*)$", "\\1", deparse(substitute(data)))
  method <- sub("^acc_(.*)_(.*)$", "\\1 \\2", deparse(substitute(data)))
  mechanism_technique <- paste(mechanism, method, sep = " ")
  mechanism_technique <- paste(strsplit(mechanism_technique, " ")[[1]][1:2], collapse = " ")
  
  df <- rbind(data.frame(mechanism_technique = mechanism_technique, value_size = "min", value = min_value, paired = 1),
              data.frame(mechanism_technique = mechanism_technique, value_size = "max", value = max_value, paired = 1))
  
  colnames(df)[3] <- "value"
  return(df)
}

######Preparing data######  
#Loading and transforming acc datasets
acc_hotdeck_MCAR <- readRDS("acc_hotdeck_MCAR.rds")
acc_hotdeck_MCAR_df <- create_dataframe_acc(acc_hotdeck_MCAR)

acc_hotdeck_MAR <- readRDS("acc_hotdeck_MAR.rds")
acc_hotdeck_MAR_df <- create_dataframe_acc(acc_hotdeck_MAR)

acc_hotdeck_MNAR <- readRDS("acc_hotdeck_MNAR.rds")
acc_hotdeck_MNAR_df <- create_dataframe_acc(acc_hotdeck_MNAR)

acc_kNN_MCAR <- readRDS("acc_kNN_MCAR.rds")
acc_kNN_MCAR_df <- create_dataframe_acc(acc_kNN_MCAR)

acc_kNN_MAR <- readRDS("acc_kNN_MAR.rds")
acc_kNN_MAR_df <- create_dataframe_acc(acc_kNN_MAR)

acc_kNN_MNAR <- readRDS("acc_kNN_MNAR.rds")
acc_kNN_MNAR_df <- create_dataframe_acc(acc_kNN_MNAR)

acc_reg_MCAR <- readRDS("acc_reg_MCAR.rds")
acc_reg_MCAR_df <- create_dataframe_acc(acc_reg_MCAR)

acc_reg_MAR <- readRDS("acc_reg_MAR.rds")
acc_reg_MAR_df <- create_dataframe_acc(acc_reg_MAR)

acc_reg_MNAR <- readRDS("acc_reg_MNAR.rds")
acc_reg_MNAR_df <- create_dataframe_acc(acc_reg_MNAR)

acc_rf_MCAR <- readRDS("acc_rf_MCAR.rds")
acc_rf_MCAR_df <- create_dataframe_acc(acc_rf_MCAR)

acc_rf_MAR <- readRDS("acc_rf_MAR.rds")
acc_rf_MAR_df <- create_dataframe_acc(acc_rf_MAR)

acc_rf_MNAR <- readRDS("acc_rf_MNAR.rds")
acc_rf_MNAR_df <- create_dataframe_acc(acc_rf_MNAR)

acc_mul_MCAR <- readRDS("acc_mul_MCAR.rds")
acc_mul_MCAR_df <- create_dataframe_acc(acc_mul_MCAR)

acc_mul_MAR <- readRDS("acc_mul_MAR.rds")
acc_mul_MAR_df <- create_dataframe_acc(acc_mul_MAR)

acc_mul_MNAR <- readRDS("acc_mul_MNAR.rds")
acc_mul_MNAR_df <- create_dataframe_acc(acc_mul_MNAR)

#Creating dataset for dumbell plot
value_dumbbell <- list(
  acc_hotdeck_MCAR_df,
  acc_hotdeck_MAR_df,
  acc_hotdeck_MNAR_df,
  acc_kNN_MCAR_df,
  acc_kNN_MAR_df,
  acc_kNN_MNAR_df,
  acc_reg_MCAR_df,
  acc_reg_MAR_df,
  acc_reg_MNAR_df,
  acc_rf_MCAR_df,
  acc_rf_MAR_df,
  acc_rf_MNAR_df,
  acc_mul_MCAR_df,
  acc_mul_MAR_df,
  acc_mul_MNAR_df)

value_dumbbell <- do.call(rbind, value_dumbbell)

value_dumbbell$paired <- rep(1:(30/2), each = 2)[order(rep(1:(30/2), each = 2))]

#Adding variable value_max
value_dumbbell <- value_dumbbell %>%
  group_by(mechanism_technique, paired) %>%
  mutate(value_max = max(value))

#Adding variable interval
value_dumbbell <- value_dumbbell %>%
  group_by(mechanism_technique, paired) %>%
  mutate(interval = max(value) - min(value))

######Creating plot######  
#Colors
colors <- c("#413d7b", "#348fa7")

#Labels
new_labels <- c("MNAR hot-deck",
                "MNAR kNN",
                "MAR kNN",
                "MAR hot-deck",
                "MCAR hot-deck",
                "MCAR kNN",
                "MCAR wielokrotna",
                "MAR wielokrotna",
                "MCAR random forest",
                "MAR random forest",
                "MNAR wielokrotna",
                "MCAR regresyjna",
                "MAR regresyjna",
                "MNAR random forest",
                "MNAR regresyjna")

#Plot
value_dumbbell %>%
  ggplot(aes(x = value, y = reorder(mechanism_technique, value_max))) +
  geom_line(aes(group = paired), color = "#348fa7", linewidth = 2) +
  geom_point(aes(color = value_size), size = 10, shape = 18) +
  labs(x = "Accuracy", y = "Mechanizm powstania braków\ndanych i metoda imputacji") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 160, 10)) +
  scale_y_discrete(labels = new_labels) +
  scale_color_manual(values = colors, name = "Wartość w grupie\n(mechanizm i metoda)") +
  theme(text = element_text(size = 20, color = "#000000"),
        axis.text = element_text(size = 25, color = "#000000"),
        axis.title = element_text(size = 25, color = "#000000"))
#####Main source income######
######Function for extracting min and max values - Accuracy######
create_dataframe_acc <- function(data) {
  
  min_value <- min(data$mean_acc)
  max_value <- max(data$mean_acc)
  
  mechanism <- sub(".*_(.*)$", "\\1", deparse(substitute(data)))
  method <- sub("^acc_(.*)_(.*)$", "\\1 \\2", deparse(substitute(data)))
  mechanism_technique <- paste(mechanism, method, sep = " ")
  mechanism_technique <- paste(strsplit(mechanism_technique, " ")[[1]][1:2], collapse = " ")
  
  df <- rbind(data.frame(mechanism_technique = mechanism_technique, value_size = "min", value = min_value, paired = 1),
              data.frame(mechanism_technique = mechanism_technique, value_size = "max", value = max_value, paired = 1))
  
  colnames(df)[3] <- "value"
  return(df)
}

######Preparing data######  
#Loading and transforming acc datasets
acc_hotdeck_MCAR <- readRDS("acc_hotdeck_MCAR.rds")
acc_hotdeck_MCAR_df <- create_dataframe_acc(acc_hotdeck_MCAR)

acc_hotdeck_MAR <- readRDS("acc_hotdeck_MAR.rds")
acc_hotdeck_MAR_df <- create_dataframe_acc(acc_hotdeck_MAR)

acc_hotdeck_MNAR <- readRDS("acc_hotdeck_MNAR.rds")
acc_hotdeck_MNAR_df <- create_dataframe_acc(acc_hotdeck_MNAR)

acc_kNN_MCAR <- readRDS("acc_kNN_MCAR.rds")
acc_kNN_MCAR_df <- create_dataframe_acc(acc_kNN_MCAR)

acc_kNN_MAR <- readRDS("acc_kNN_MAR.rds")
acc_kNN_MAR_df <- create_dataframe_acc(acc_kNN_MAR)

acc_kNN_MNAR <- readRDS("acc_kNN_MNAR.rds")
acc_kNN_MNAR_df <- create_dataframe_acc(acc_kNN_MNAR)

acc_reg_MCAR <- readRDS("acc_reg_MCAR.rds")
acc_reg_MCAR_df <- create_dataframe_acc(acc_reg_MCAR)

acc_reg_MAR <- readRDS("acc_reg_MAR.rds")
acc_reg_MAR_df <- create_dataframe_acc(acc_reg_MAR)

acc_reg_MNAR <- readRDS("acc_reg_MNAR.rds")
acc_reg_MNAR_df <- create_dataframe_acc(acc_reg_MNAR)

acc_rf_MCAR <- readRDS("acc_rf_MCAR.rds")
acc_rf_MCAR_df <- create_dataframe_acc(acc_rf_MCAR)

acc_rf_MAR <- readRDS("acc_rf_MAR.rds")
acc_rf_MAR_df <- create_dataframe_acc(acc_rf_MAR)

acc_rf_MNAR <- readRDS("acc_rf_MNAR.rds")
acc_rf_MNAR_df <- create_dataframe_acc(acc_rf_MNAR)

acc_mul_MCAR <- readRDS("acc_mul_MCAR.rds")
acc_mul_MCAR_df <- create_dataframe_acc(acc_mul_MCAR)

acc_mul_MAR <- readRDS("acc_mul_MAR.rds")
acc_mul_MAR_df <- create_dataframe_acc(acc_mul_MAR)

acc_mul_MNAR <- readRDS("acc_mul_MNAR.rds")
acc_mul_MNAR_df <- create_dataframe_acc(acc_mul_MNAR)

#Creating dataset for dumbell plot
value_dumbbell <- list(
  acc_hotdeck_MCAR_df,
  acc_hotdeck_MAR_df,
  acc_hotdeck_MNAR_df,
  acc_kNN_MCAR_df,
  acc_kNN_MAR_df,
  acc_kNN_MNAR_df,
  acc_reg_MCAR_df,
  acc_reg_MAR_df,
  acc_reg_MNAR_df,
  acc_rf_MCAR_df,
  acc_rf_MAR_df,
  acc_rf_MNAR_df,
  acc_mul_MCAR_df,
  acc_mul_MAR_df,
  acc_mul_MNAR_df)

value_dumbbell <- do.call(rbind, value_dumbbell)

value_dumbbell$paired <- rep(1:(30/2), each = 2)[order(rep(1:(30/2), each = 2))]

#Adding variable value_max
value_dumbbell <- value_dumbbell %>%
  group_by(mechanism_technique, paired) %>%
  mutate(value_max = max(value))

#Adding variable interval
value_dumbbell <- value_dumbbell %>%
  group_by(mechanism_technique, paired) %>%
  mutate(interval = max(value) - min(value))

######Creating plot######      
#Colors
colors <- c("#413d7b", "#348fa7")

#Labels
new_labels <- c("MAR hot-deck",
                "MAR kNN",
                "MNAR kNN",
                "MNAR hot-deck",
                "MCAR hot-deck",
                "MCAR kNN",
                "MNAR random forest",
                "MAR wielokrotna",
                "MAR random forest",
                "MNAR wielokrotna",
                "MCAR wielokrotna",
                "MCAR random forest",
                "MNAR regresyjna",
                "MCAR regresyjna",
                "MAR regresyjna")

#Plot
value_dumbbell %>%
  ggplot(aes(x = value, y = reorder(mechanism_technique, value_max))) +
  geom_line(aes(group = paired), color = "#348fa7", linewidth = 2) +
  geom_point(aes(color = value_size), size = 10, shape = 18) +
  labs(x = "Accuracy", y = "Mechanizm powstania braków\ndanych i metoda imputacji") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 160, 20)) +
  scale_y_discrete(labels = new_labels) +
  scale_color_manual(values = colors, name = "Wartość w grupie\n(mechanizm i metoda)") +
  theme(text = element_text(size = 20, color = "#000000"),
        axis.text = element_text(size = 25, color = "#000000"),
        axis.title = element_text(size = 25, color = "#000000"))

#####House type roof#####
######Function for extracting min and max values - F1-score######
create_dataframe_f1 <- function(data) {
  
  min_value <- min(data$mean_f1)
  max_value <- max(data$mean_f1)
  
  mechanism <- sub(".*_(.*)$", "\\1", deparse(substitute(data)))
  method <- sub("^f1_(.*)_(.*)$", "\\1 \\2", deparse(substitute(data)))
  mechanism_technique <- paste(mechanism, method, sep = " ")
  mechanism_technique <- paste(strsplit(mechanism_technique, " ")[[1]][1:2], collapse = " ")
  
  df <- rbind(data.frame(mechanism_technique = mechanism_technique, value_size = "min", value = min_value, paired = 1),
              data.frame(mechanism_technique = mechanism_technique, value_size = "max", value = max_value, paired = 1))
  
  colnames(df)[3] <- "value"
  return(df)
}

######Preparing data######
#Loading and transforming f1 datasets
f1_hotdeck_MCAR <- readRDS("f1_hotdeck_MCAR.rds")
f1_hotdeck_MCAR_df <- create_dataframe_f1(f1_hotdeck_MCAR)

f1_hotdeck_MAR <- readRDS("f1_hotdeck_MAR.rds")
f1_hotdeck_MAR_df <- create_dataframe_f1(f1_hotdeck_MAR)

f1_hotdeck_MNAR <- readRDS("f1_hotdeck_MNAR.rds")
f1_hotdeck_MNAR_df <- create_dataframe_f1(f1_hotdeck_MNAR)

f1_kNN_MCAR <- readRDS("f1_kNN_MCAR.rds")
f1_kNN_MCAR_df <- create_dataframe_f1(f1_kNN_MCAR)

f1_kNN_MAR <- readRDS("f1_kNN_MAR.rds")
f1_kNN_MAR_df <- create_dataframe_f1(f1_kNN_MAR)

f1_kNN_MNAR <- readRDS("f1_kNN_MNAR.rds")
f1_kNN_MNAR_df <- create_dataframe_f1(f1_kNN_MNAR)

f1_reg_MCAR <- readRDS("f1_reg_MCAR.rds")
f1_reg_MCAR_df <- create_dataframe_f1(f1_reg_MCAR)

f1_reg_MAR <- readRDS("f1_reg_MAR.rds")
f1_reg_MAR_df <- create_dataframe_f1(f1_reg_MAR)

f1_reg_MNAR <- readRDS("f1_reg_MNAR.rds")
f1_reg_MNAR_df <- create_dataframe_f1(f1_reg_MNAR)

f1_rf_MCAR <- readRDS("f1_rf_MCAR.rds")
f1_rf_MCAR_df <- create_dataframe_f1(f1_rf_MCAR)

f1_rf_MAR <- readRDS("f1_rf_MAR.rds")
f1_rf_MAR_df <- create_dataframe_f1(f1_rf_MAR)

f1_rf_MNAR <- readRDS("f1_rf_MNAR.rds")
f1_rf_MNAR_df <- create_dataframe_f1(f1_rf_MNAR)

f1_mul_MCAR <- readRDS("f1_mul_MCAR.rds")
f1_mul_MCAR_df <- create_dataframe_f1(f1_mul_MCAR)

f1_mul_MAR <- readRDS("f1_mul_MAR.rds")
f1_mul_MAR_df <- create_dataframe_f1(f1_mul_MAR)

f1_mul_MNAR <- readRDS("f1_mul_MNAR.rds")
f1_mul_MNAR_df <- create_dataframe_f1(f1_mul_MNAR)

#Creating dataset for dumbell plot
value_dumbbell <- list(
  f1_hotdeck_MCAR_df,
  f1_hotdeck_MAR_df,
  f1_hotdeck_MNAR_df,
  f1_kNN_MCAR_df,
  f1_kNN_MAR_df,
  f1_kNN_MNAR_df,
  f1_reg_MCAR_df,
  f1_reg_MAR_df,
  f1_reg_MNAR_df,
  f1_rf_MCAR_df,
  f1_rf_MAR_df,
  f1_rf_MNAR_df,
  f1_mul_MCAR_df,
  f1_mul_MAR_df,
  f1_mul_MNAR_df)

value_dumbbell <- do.call(rbind, value_dumbbell)

value_dumbbell$paired <- rep(1:(30/2), each = 2)[order(rep(1:(30/2), each = 2))]

#Adding variable value_max
value_dumbbell <- value_dumbbell %>%
  group_by(mechanism_technique, paired) %>%
  mutate(value_max = max(value))

#Adding variable interval
value_dumbbell <- value_dumbbell %>%
  group_by(mechanism_technique, paired) %>%
  mutate(interval = max(value) - min(value))

######Creating plot######    
#Colors
colors <- c("#413d7b", "#348fa7")

#Labels
new_labels <- c("MNAR random forest",
                "MAR random forest",
                "MCAR kNN",
                "MCAR hot-deck",
                "MAR kNN",
                "MAR hot-deck",
                "MNAR kNN",
                "MNAR hot-deck",
                "MAR wielokrotna",
                "MCAR wielokrotna",
                "MCAR random forest",
                "MAR regresyjna",
                "MNAR wielokrotna",
                "MCAR regresyjna",
                "MNAR regresyjna")

#Plot
value_dumbbell %>%
  ggplot(aes(x = value, y = reorder(mechanism_technique, value_max))) +
  geom_line(aes(group = paired), color = "#348fa7", linewidth = 2) +
  geom_point(aes(color = value_size), size = 10, shape = 18) +
  labs(x = "F1-score", y = "Mechanizm powstania braków\ndanych i metoda imputacji") +
  theme_minimal() +
  scale_x_continuous(breaks = seq(0, 160, 20)) +
  scale_y_discrete(labels = new_labels) +
  scale_color_manual(values = colors, name = "Wartość w grupie\n(mechanizm i metoda)") +
  theme(text = element_text(size = 20, color = "#000000"),
        axis.text = element_text(size = 25, color = "#000000"),
        axis.title = element_text(size = 25, color = "#000000"))                