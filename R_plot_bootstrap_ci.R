#####Plot - bootstrap confidence intervals#####
######Preparing data######
readRDS("median_ci_bootstrap_hotdeck_MCAR.rds") -> median_ci_bootstrap_hotdeck_MCAR
readRDS("median_ci_bootstrap_hotdeck_MAR.rds") -> median_ci_bootstrap_hotdeck_MAR
readRDS("median_ci_bootstrap_hotdeck_MNAR.rds") -> median_ci_bootstrap_hotdeck_MNAR
readRDS("median_ci_bootstrap_kNN_MCAR.rds") -> median_ci_bootstrap_kNN_MCAR
readRDS("median_ci_bootstrap_kNN_MAR.rds") -> median_ci_bootstrap_kNN_MAR
readRDS("median_ci_bootstrap_kNN_MNAR.rds") -> median_ci_bootstrap_kNN_MNAR
readRDS("median_ci_bootstrap_reg_MCAR.rds") -> median_ci_bootstrap_reg_MCAR
readRDS("median_ci_bootstrap_reg_MAR.rds") -> median_ci_bootstrap_reg_MAR
readRDS("median_ci_bootstrap_reg_MNAR.rds") -> median_ci_bootstrap_reg_MNAR
readRDS("median_ci_bootstrap_rf_MCAR.rds") -> median_ci_bootstrap_rf_MCAR
readRDS("median_ci_bootstrap_rf_MAR.rds") -> median_ci_bootstrap_rf_MAR
readRDS("median_ci_bootstrap_rf_MNAR.rds") -> median_ci_bootstrap_rf_MNAR
readRDS("median_ci_bootstrap_mul_MCAR.rds") -> median_ci_bootstrap_mul_MCAR
readRDS("median_ci_bootstrap_mul_MAR.rds") -> median_ci_bootstrap_mul_MAR
readRDS("median_ci_bootstrap_mul_MNAR.rds") -> median_ci_bootstrap_mul_MNAR

all <- rbind(
  transform(median_ci_bootstrap_hotdeck_MCAR, method = "MCAR hot-deck"),
  transform(median_ci_bootstrap_hotdeck_MAR, method = "MAR hot-deck"),
  transform(median_ci_bootstrap_hotdeck_MNAR, method = "MNAR hot-deck"),
  transform(median_ci_bootstrap_kNN_MCAR, method = "MCAR kNN"),
  transform(median_ci_bootstrap_kNN_MAR, method = "MAR kNN"),
  transform(median_ci_bootstrap_kNN_MNAR, method = "MNAR kNN"),
  transform(median_ci_bootstrap_reg_MCAR, method = "MCAR regression"),
  transform(median_ci_bootstrap_reg_MAR, method = "MAR regression"),
  transform(median_ci_bootstrap_reg_MNAR, method = "MNAR regression"),
  transform(median_ci_bootstrap_rf_MCAR, method = "MCAR random forest"),
  transform(median_ci_bootstrap_rf_MAR, method = "MAR random forest"),
  transform(median_ci_bootstrap_rf_MNAR, method = "MNAR random forest"),
  transform(median_ci_bootstrap_mul_MCAR, method = "MCAR multiple"),
  transform(median_ci_bootstrap_mul_MAR, method = "MAR multiple"),
  transform(median_ci_bootstrap_mul_MNAR, method = "MNAR multiple")
)

colnames(all) <- c("na_frac", "lower", "upper", "coverage", "method")

all_modified <- all %>%
  dplyr::select(method, na_frac, coverage)

all_wide <- all %>%
  dplyr::select(method, na_frac, coverage) %>%
  spread(key = na_frac, value = coverage)

write_xlsx(all_wide, "all_wide.xlsx")

######Plot 3.7######
m <- matrix(c(93, 92, 92, 92, 84, 88, 85, 80, 79, 75, 68, 62, 68, 61,
              95, 94, 92, 86, 84, 83, 72, 65, 65, 61, 57, 53, 40, 41,
              95, 92, 90, 83, 79, 71, 68, 70, 65, 58, 45, 49, 39, 34,
              97, 96, 93, 96, 92, 93, 93, 89, 86, 91, 85, 83, 78, 81,
              97, 96, 96, 96, 92, 93, 92, 88, 87, 92, 88, 81, 83, 82), 5, 14, byrow = TRUE)

t(m) -> m

colnames(m) <- c("MCAR hot-deck",
                 "MCAR kNN",
                 "MCAR regresyjna",
                 "MCAR random forest",
                 "MCAR wielokrotna")
rownames(m) <- paste(seq(5, 70, by = 5), "%")

df <- melt(m)
colnames(df) <- c("x", "y", "value")

#Plot the heatmap using the viridis color palette
ggplot(df, 
       aes(x = x, 
           y = y, 
           fill = value)) +
  geom_tile(color = "#203864") +
  geom_text(aes(label = value), 
            color = "white", 
            size = 10) +
  coord_fixed() +
  scale_fill_viridis(limits = c(0, 100),
                     direction = -1,
                     option = "mako", 
                     name = "Częstość względna objęcia\nprzez przedział ufności\nrzeczywistej wartości parametru") +
  scale_x_discrete(labels = rownames(m)) +
  labs(x = "Frakcja braków danych", 
       y = "Mechanizm powstania\nbraków i metoda imputacji") +
  theme(text = element_text(color = "black", 
                            size = 20),
        axis.text.x = element_text(color = "black", 
                                   angle = 45, 
                                   hjust = 1, 
                                   size = 20),
        axis.text.y = element_text(color = "black", 
                                   size = 20))

######Plot 3.8######
m <- matrix(c(95, 95, 93, 86, 89, 85, 84, 83, 84, 79, 60, 63, 51, 58,
              95, 94, 92, 83, 88, 78, 74, 66, 55, 57, 56, 45, 39, 31,
              96, 95, 95, 91, 87, 92, 83, 83, 80, 74, 74, 66, 75, 76,
              96, 96, 97, 95, 93, 87, 91, 91, 92, 92, 85, 82, 75, 76,
              95, 96, 95, 95, 97, 92, 91, 91, 90, 96, 88, 83, 79, 81),
            5, 14, byrow = TRUE)

t(m) -> m

colnames(m) <- c("MAR hot-deck",
                 "MAR kNN",
                 "MAR regresyjna",
                 "MAR random forest",
                 "MAR wielokrotna")
rownames(m) <- paste(seq(5, 70, by = 5), "%")

df <- melt(m)
colnames(df) <- c("x", "y", "value")

#Plot the heatmap using the viridis color palette
ggplot(df, 
       aes(x = x, 
           y = y, 
           fill = value)) +
  geom_tile(color = "#203864") +
  geom_text(aes(label = value), 
            color = "white", 
            size = 10) +
  coord_fixed() +
  scale_fill_viridis(limits = c(0, 100),
                     direction = -1,
                     option = "mako", 
                     name = "Częstość względna objęcia\nprzez przedział ufności\nrzeczywistej wartości parametru") +
  scale_x_discrete(labels = rownames(m)) +
  labs(x = "Frakcja braków danych", 
       y = "Mechanizm powstania\nbraków i metoda imputacji") +
  theme(text = element_text(color = "black", 
                            size = 20),
        axis.text.x = element_text(color = "black", 
                                   angle = 45, 
                                   hjust = 1, 
                                   size = 20),
        axis.text.y = element_text(color = "black", 
                                   size = 20))

######Plot 3.9######
m <- matrix(c(93, 83, 70, 56, 24, 12, 3, 0, 0, 0, 0, 0, 0, 0,
              95, 95, 91, 92, 86, 80, 80, 63, 44, 28, 16, 2, 0, 0,
              94, 87, 75, 68, 54, 39, 25, 19, 13, 3, 3, 4, 0, 0,
              94, 91, 88, 81, 73, 65, 54, 29, 18, 5, 2, 0, 0, 0,
              95, 95, 90, 87, 79, 72, 58, 40, 34, 12, 10, 1, 1, 0),
            5, 14, byrow = TRUE)

t(m) -> m

colnames(m) <- c("MNAR hot-deck",
                 "MNAR kNN",
                 "MNAR regresyjna",
                 "MNAR random forest",
                 "MNAR wielokrotna")
rownames(m) <- paste(seq(5, 70, by = 5), "%")

#Transform the matrix in long format
df <- melt(m)
colnames(df) <- c("x", "y", "value")

#Plot the heatmap using the viridis color palette
ggplot(df, 
       aes(x = x, 
           y = y, 
           fill = value)) +
  geom_tile(color = "#203864") +
  geom_text(aes(label = value), 
            color = "white", 
            size = 10) +
  coord_fixed() +
  scale_fill_viridis(limits = c(0, 100),
                     direction = -1,
                     option = "mako", 
                     name = "Częstość względna objęcia\nprzez przedział ufności\nrzeczywistej wartości parametru") +
  scale_x_discrete(labels = rownames(m)) +
  labs(x = "Frakcja braków danych", 
       y = "Mechanizm powstania\nbraków i metoda imputacji") +
  theme(text = element_text(color = "black", 
                            size = 20),
        axis.text.x = element_text(color = "black", 
                                   angle = 45, 
                                   hjust = 1, 
                                   size = 20),
        axis.text.y = element_text(color = "black", 
                                   size = 20))