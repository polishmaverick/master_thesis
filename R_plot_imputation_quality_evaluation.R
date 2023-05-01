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

create_dataframe_mape()

######Function for extracting min and max values - accuracy######
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

create_dataframe_acc

######Plot example######
mechanism_technique <-
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
value_dumbbell <- data.frame(mechanism_technique, value_size, value, paired)
value_dumbbell

#Adding variable value_max
value_dumbbell <- value_dumbbell %>%
  group_by(mechanism_technique, paired) %>%
  mutate(value_max = max(value))

#Adding variable interval
value_dumbbell <- value_dumbbell %>%
  group_by(mechanism_technique, paired) %>%
  mutate(interval = max(value) - min(value))

#Colors
colors <- c("#203864", #max
            "#C40404") #min
                     
#Plot
value_dumbbell %>%
  ggplot(aes(x = value, y = reorder(mechanism_technique, interval))) +
  geom_line(aes(group = paired), color = "#000000") +
  geom_point(aes(color = value_size), size = 7, shape = 18) +
  labs(x = "Value", y = "Mechanizm powstania braków danych i technika imputacji") +
  theme_minimal() +
  scale_color_manual(values = colors, name = "MAPE size")+
  theme(text = element_text(size = 20, color = "#000000"))