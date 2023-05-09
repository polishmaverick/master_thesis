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
all_data <- rbind(
  transform(median_bootstrap_median_MCAR, method = "MCAR median"),
  transform(median_bootstrap_median_MAR, method = "MAR median"),
  transform(median_bootstrap_median_MNAR, method = "MNAR median"),
  transform(median_bootstrap_hotdeck_MCAR, method = "MCAR hot-deck"),
  transform(median_bootstrap_hotdeck_MAR, method = "MAR hot-deck"),
  transform(median_bootstrap_hotdeck_MNAR, method = "MNAR hot-deck"),
  transform(median_bootstrap_kNN_MCAR, method = "MCAR kNN"),
  transform(median_bootstrap_kNN_MAR, method = "MAR kNN"),
  transform(median_bootstrap_kNN_MNAR, method = "MNAR kNN"),
  transform(median_bootstrap_reg_MCAR, method = "MCAR regression"),
  transform(median_bootstrap_reg_MAR, method = "MAR regression"),
  transform(median_bootstrap_reg_MNAR, method = "MNAR regression"),
  transform(median_bootstrap_rf_MCAR, method = "MCAR random forest"),
  transform(median_bootstrap_rf_MAR, method = "MAR random forest"),
  transform(median_bootstrap_rf_MNAR, method = "MNAR random forest"),
  transform(median_bootstrap_mul_MCAR, method = "MCAR multiple"),
  transform(median_bootstrap_mul_MAR, method = "MAR multiple"),
  transform(median_bootstrap_mul_MNAR, method = "MNAR multiple")
)

#Rename columns
colnames(all_data) <- c("na_frac", "Z", "method")
median(data$tot_household_income)
#Convert 'method' column to factor
all_data$method <- factor(all_data$method)

#Przygotowanie danych do heatmapy - obliczenie średniej wartości Z dla każdej kombinacji na_frac i method
heatmap_data <- all_data %>%
  group_by(na_frac, method) %>%
  summarise(mean_Z = mean(Z, na.rm = TRUE))

#Obliczenie wartości bezwzględnych różnic między wartościami mean_Z a medianą mean_Z
heatmap_data <- heatmap_data %>%
  mutate(abs_diff = abs(mean_Z - median(mean_Z, na.rm = TRUE)))

#Wybór 10 kafelków z wartościami najbliższymi medianie
top_10_tiles <- heatmap_data %>%
  arrange(abs_diff) %>%
  head(10)

heatmap_plot <- ggplot(heatmap_data, aes(x = method, y = na_frac, fill = mean_Z)) +
  geom_tile() +
  scale_fill_viridis(name = "Mediana dochodu\ngospodarstwa domowego", option = "mako") +
  labs(x = "Mechanizm braków i metoda imputacji", y = "Frakcja braków danych", fill = "Mean Z") +
  theme_minimal() +
  theme(axis.title = element_text(size = 22, color = "black"),
        axis.text = element_text(size = 20, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 22, color = "black"),
        legend.text = element_text(size = 18, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

heatmap_plot <- heatmap_plot +
  scale_x_discrete(labels = function(x) {
    x <- gsub("median", "mediana", x)
    x <- gsub("regression", "regresyjna", x)
    x <- gsub("multiple", "wielokrotna", x)
    return(x)
  }) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

heatmap_plot <- heatmap_plot + geom_text(data = top_10_tiles, aes(label = round(mean_Z, 0)), size = 7, color = "white")

print(heatmap_plot)

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
all_data <- rbind(
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

#Rename columns
colnames(all_data) <- c("na_frac", "Z", "method")
median(data$tot_household_income)
#Convert 'method' column to factor
all_data$method <- factor(all_data$method)

#Przygotowanie danych do heatmapy - obliczenie średniej wartości Z dla każdej kombinacji na_frac i method
heatmap_data <- all_data %>%
  group_by(na_frac, method) %>%
  summarise(mean_Z = mean(Z, na.rm = TRUE))

#Obliczenie wartości bezwzględnych różnic między wartościami mean_Z a medianą mean_Z
heatmap_data <- heatmap_data %>%
  mutate(abs_diff = abs(mean_Z - median(mean_Z, na.rm = TRUE)))

#Wybór 10 kafelków z wartościami najbliższymi medianie
top_10_tiles <- heatmap_data %>%
  arrange(abs_diff) %>%
  head(10)

heatmap_plot <- ggplot(heatmap_data, aes(x = method, y = na_frac, fill = mean_Z)) +
  geom_tile() +
  scale_fill_viridis(name = "Mediana dochodu\ngospodarstwa domowego", option = "mako") +
  labs(x = "Mechanizm braków i metoda imputacji", y = "Frakcja braków danych", fill = "Mean Z") +
  theme_minimal() +
  theme(axis.title = element_text(size = 22, color = "black"),
        axis.text = element_text(size = 20, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_text(size = 22, color = "black"),
        legend.text = element_text(size = 18, color = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

heatmap_plot <- heatmap_plot +
  scale_x_discrete(labels = function(x) {
    x <- gsub("median", "mediana", x)
    x <- gsub("regression", "regresyjna", x)
    x <- gsub("multiple", "wielokrotna", x)
    return(x)
  }) +
  scale_y_continuous(labels = function(x) paste0(x, "%"))

heatmap_plot <- heatmap_plot + geom_text(data = top_10_tiles, aes(label = round(mean_Z, 0)), size = 7, color = "white")

print(heatmap_plot)