######Bootstrap median######
#Read .rds files
readRDS("median_bootstrap_median_MCAR.rds") -> median_bootstrap_median_MCAR
readRDS("median_bootstrap_median_MAR.rds") -> median_bootstrap_median_MAR
readRDS("median_bootstrap_median_MNAR.rds") -> median_bootstrap_median_MNAR
readRDS("median_bootstrap_hotdeck_MCAR.rds") -> median_bootstrap_hotdeck_MCAR
readRDS("median_bootstrap_hotdeck_MAR.rds") -> median_bootstrap_hotdeck_MAR
readRDS("median_bootstrap_hotdeck_MNAR.rds") -> median_bootstrap_hotdeck_MNAR
readRDS("median_bootstrap_kNN_MCAR.rds") -> median_bootstrap_kNN_MCAR
readRDS("median_bootstrap_kNN_MAR.rds") -> median_bootstrap_kNN_MAR
readRDS("median_bootstrap_kNN_MNAR.rds") -> median_bootstrap_kNN_MNAR
readRDS("median_bootstrap_reg_MCAR.rds") -> median_bootstrap_reg_MCAR
readRDS("median_bootstrap_reg_MAR.rds") -> median_bootstrap_reg_MAR
readRDS("median_bootstrap_reg_MNAR.rds") -> median_bootstrap_reg_MNAR
readRDS("median_bootstrap_rf_MCAR.rds") -> median_bootstrap_rf_MCAR
readRDS("median_bootstrap_rf_MAR.rds") -> median_bootstrap_rf_MAR
readRDS("median_bootstrap_rf_MNAR.rds") -> median_bootstrap_rf_MNAR
readRDS("median_bootstrap_mul_MCAR.rds") -> median_bootstrap_mul_MCAR
readRDS("median_bootstrap_mul_MAR.rds") -> median_bootstrap_mul_MAR
readRDS("median_bootstrap_mul_MNAR.rds") -> median_bootstrap_mul_MNAR

#Combine the datasets into one
all_data_b <- rbind(
  transform(median_bootstrap_median_MCAR, method = "MCAR mediana"),
  transform(median_bootstrap_median_MAR, method = "MAR mediana"),
  transform(median_bootstrap_median_MNAR, method = "MNAR mediana"),
  transform(median_bootstrap_hotdeck_MCAR, method = "MCAR hot-deck"),
  transform(median_bootstrap_hotdeck_MAR, method = "MAR hot-deck"),
  transform(median_bootstrap_hotdeck_MNAR, method = "MNAR hot-deck"),
  transform(median_bootstrap_kNN_MCAR, method = "MCAR kNN"),
  transform(median_bootstrap_kNN_MAR, method = "MAR kNN"),
  transform(median_bootstrap_kNN_MNAR, method = "MNAR kNN"),
  transform(median_bootstrap_reg_MCAR, method = "MCAR regresyjna"),
  transform(median_bootstrap_reg_MAR, method = "MAR regresyjna"),
  transform(median_bootstrap_reg_MNAR, method = "MNAR regresyjna"),
  transform(median_bootstrap_rf_MCAR, method = "MCAR random forest"),
  transform(median_bootstrap_rf_MAR, method = "MAR random forest"),
  transform(median_bootstrap_rf_MNAR, method = "MNAR random forest"),
  transform(median_bootstrap_mul_MCAR, method = "MCAR wielokrotna"),
  transform(median_bootstrap_mul_MAR, method = "MAR wielokrotna"),
  transform(median_bootstrap_mul_MNAR, method = "MNAR wielokrotna")
)


######Jackknife median######
#Read .rds files
readRDS("median_jackknife_median_MCAR.rds") -> median_jackknife_median_MCAR
readRDS("median_jackknife_median_MAR.rds") -> median_jackknife_median_MAR
readRDS("median_jackknife_median_MNAR.rds") -> median_jackknife_median_MNAR
readRDS("median_jackknife_hotdeck_MCAR.rds") -> median_jackknife_hotdeck_MCAR
readRDS("median_jackknife_hotdeck_MAR.rds") -> median_jackknife_hotdeck_MAR
readRDS("median_jackknife_hotdeck_MNAR.rds") -> median_jackknife_hotdeck_MNAR
readRDS("median_jackknife_kNN_MCAR.rds") -> median_jackknife_kNN_MCAR
readRDS("median_jackknife_kNN_MAR.rds") -> median_jackknife_kNN_MAR
readRDS("median_jackknife_kNN_MNAR.rds") -> median_jackknife_kNN_MNAR
readRDS("median_jackknife_reg_MCAR.rds") -> median_jackknife_reg_MCAR
readRDS("median_jackknife_reg_MAR.rds") -> median_jackknife_reg_MAR
readRDS("median_jackknife_reg_MNAR.rds") -> median_jackknife_reg_MNAR
readRDS("median_jackknife_rf_MCAR.rds") -> median_jackknife_rf_MCAR
readRDS("median_jackknife_rf_MAR.rds") -> median_jackknife_rf_MAR
readRDS("median_jackknife_rf_MNAR.rds") -> median_jackknife_rf_MNAR
readRDS("median_jackknife_mul_MCAR.rds") -> median_jackknife_mul_MCAR
readRDS("median_jackknife_mul_MAR.rds") -> median_jackknife_mul_MAR
readRDS("median_jackknife_mul_MNAR.rds") -> median_jackknife_mul_MNAR

#Combine the datasets into one
all_data_j <- rbind(
  transform(median_jackknife_median_MCAR, method = "MCAR mediana"),
  transform(median_jackknife_median_MAR, method = "MAR mediana"),
  transform(median_jackknife_median_MNAR, method = "MNAR mediana"),
  transform(median_jackknife_hotdeck_MCAR, method = "MCAR hot-deck"),
  transform(median_jackknife_hotdeck_MAR, method = "MAR hot-deck"),
  transform(median_jackknife_hotdeck_MNAR, method = "MNAR hot-deck"),
  transform(median_jackknife_kNN_MCAR, method = "MCAR kNN"),
  transform(median_jackknife_kNN_MAR, method = "MAR kNN"),
  transform(median_jackknife_kNN_MNAR, method = "MNAR kNN"),
  transform(median_jackknife_reg_MCAR, method = "MCAR regresyjna"),
  transform(median_jackknife_reg_MAR, method = "MAR regresyjna"),
  transform(median_jackknife_reg_MNAR, method = "MNAR regresyjna"),
  transform(median_jackknife_rf_MCAR, method = "MCAR random forest"),
  transform(median_jackknife_rf_MAR, method = "MAR random forest"),
  transform(median_jackknife_rf_MNAR, method = "MNAR random forest"),
  transform(median_jackknife_mul_MCAR, method = "MCAR wielokrotna"),
  transform(median_jackknife_mul_MAR, method = "MAR wielokrotna"),
  transform(median_jackknife_mul_MNAR, method = "MNAR wielokrotna")
)

######Results######
#Median bootstrap error < 5%
all_data_b %>%
  mutate(x = abs((mean_bootstrap_median / 164079.5) - 1) * 100) %>%
  summarise(percentage = sum(x < 5) / n() * 100)

#Median jackknife error < 5%
all_data_j %>%
  mutate(x = abs((mean_jackknife_median / 164079.5) - 1) * 100) %>%
  summarise(percentage = sum(x < 5) / n() * 100)

#Median bootstrap error < 1%
all_data_b %>%
  mutate(x = abs((mean_bootstrap_median / 164079.5) - 1) * 100) %>%
  summarise(percentage = sum(x < 5) / n() * 100)

#Median jackknife error < 1%
all_data_j %>%
  mutate(x = abs((mean_jackknife_median / 164079.5) - 1) * 100) %>%
  summarise(percentage = sum(x < 5) / n() * 100)


#Median in population
p <- 164079.5

#Max
round(((max(all_data_b$mean_bootstrap_median) / p) - 1) * 100, 2)
round(((max(all_data_j$mean_jackknife_median) / p) - 1) * 100, 2)

#Min
round(min(abs(((all_data_b$mean_bootstrap_median) / p) - 1) * 100), 4)
round(min(abs(((all_data_j$mean_jackknife_median) / p) - 1) * 100), 4)

#Min
round(median(abs(((all_data_b$mean_bootstrap_median) / p) - 1) * 100), 2)
round(median(abs(((all_data_j$mean_jackknife_median) / p) - 1) * 100), 2)

all_data_b$mean_bootstrap_median
all_data_j$mean_jackknife_median
p

# Dodaj nową kolumnę 'key' do ramki danych all_data_b
all_data_b$key <- paste(all_data_b$na_frac, all_data_b$method, sep = " ")

# Dodaj nową kolumnę 'key' do ramki danych all_data_j
all_data_j$key <- paste(all_data_j$na_frac, all_data_j$method, sep = " ")

# Połącz ramki danych na podstawie nowej kolumny 'key'
merged_data <- merge(all_data_b, all_data_j, by = "key", all = TRUE)

merged_data <- merged_data %>%
  mutate(closest_to_p = ifelse(abs(mean_bootstrap_median - p) < abs(mean_jackknife_median - p), "b", "j"))

MCAR <- merged_data %>% 
  dplyr::select(na_frac.x, method.x, mean_bootstrap_median, mean_jackknife_median, closest_to_p) %>%
  filter(str_detect(method.x, "^MCAR"))

MAR <- merged_data %>% 
  dplyr::select(na_frac.x, method.x, mean_bootstrap_median, mean_jackknife_median, closest_to_p) %>%
  filter(str_detect(method.x, "^MAR"))

MNAR <- merged_data %>% 
  dplyr::select(na_frac.x, method.x, mean_bootstrap_median, mean_jackknife_median, closest_to_p) %>%
  filter(str_detect(method.x, "^MNAR"))

#Table
table(MCAR$closest_to_p)
table(MAR$closest_to_p)
table(MNAR$closest_to_p)

######Density plot######
plot1 <- ggplot(all_data_b, aes(x = mean_bootstrap_median)) +
  geom_density(aes(fill = "Bootstrap")) +
  geom_vline(
    aes(xintercept = p),
    linetype = "dashed",
    color = "black",
    size = 1
  ) +
  xlab("Dochód gospodarstwa domowego") +
  ylab("Gęstość") +
  scale_fill_manual(values = c("Bootstrap" = "#413d7b")) +
  labs(fill = "Metoda") +
  theme_minimal(base_size = 25) +
  theme(text = element_text(color = "black"))+
  theme(axis.text = element_text(color = "black"), 
        axis.title = element_text(color = "black"))

plot2 <- ggplot(all_data_j, aes(x = mean_jackknife_median)) +
  geom_density(aes(fill = "Jackknife")) +
  geom_vline(
    aes(xintercept = p),
    linetype = "dashed",
    color = "black",
    size = 1
  ) +
  xlab("Dochód gospodarstwa domowego") +
  ylab("Gęstość") +
  scale_fill_manual(values = c("Jackknife" = "#348fa7")) +
  labs(fill = "Metoda") +
  theme_minimal(base_size = 25) +
  theme(text = element_text(color = "black"))+
  theme(axis.text = element_text(color = "black"), 
        axis.title = element_text(color = "black"))

grid.arrange(plot1, plot2, ncol = 1)