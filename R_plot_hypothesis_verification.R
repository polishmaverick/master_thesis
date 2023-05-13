#####Plot example#####
#X axis labels
labels <- c(
            "MCAR hot-deck",
            "MAR hot-deck",
            "MNAR hot-deck",
            "MCAR kNN",
            "MAR kNN",
            "MNAR kNN",
            "MCAR regresyjna",
            "MAR regresyjna",
            "MNAR regresyjna",
            "MCAR random forest",
            "MAR random forest",
            "MNAR random forest",
            "MCAR wielokrotna",
            "MAR wielokrotna",
            "MNAR wielokrotna")

hypothesis <- expand.grid(X = labels, Y = seq(5, 70, 5))
hypothesis <- transform(hypothesis, X = factor(X))
hypothesis$Z <- runif(nrow(hypothesis)) * 100

my_palette <- colorRampPalette(brewer.pal(n = 9, "Blues")[4:9])(100)

hypothesis <- hypothesis %>%
  left_join(all_data, by = c("X", "Y")) %>%
  mutate(Z = ifelse(is.na(Z.y), Z.x, Z.y)) %>%
  dplyr::select(X, Y, Z = Z.y)

hypothesis$X <- as.factor(hypothesis$X)

my_palette <- viridis(100, option = "mako", begin = 0.5, end = 0.0) # Tworzenie palety kolorów z palety mako

levelplot(Z ~ X * Y,
          data = hypothesis,
          xlab = list(label = "Mechanizm powstania braków danych i metoda imputacji", fontsize = 20),
          ylab = list(label = "Frakcja braków danych (w %)", fontsize = 20),
          scales = list(x = list(rot = 45, cex = 1.4), y = list(cex = 1.4)),
          col.regions = my_palette,
          colorkey = list(labels = list(cex = 1.4),
                          at = seq(0, 100, length.out = 100), 
                          title = "% przypadków odrzucenia hipotezy H0"))

######Jackknife median######
#Read .rds files
readRDS("ow_hotdeck_MCAR.rds") -> ow_hotdeck_MCAR
readRDS("ow_hotdeck_MAR.rds") -> ow_hotdeck_MAR
readRDS("ow_hotdeck_MNAR.rds") -> ow_hotdeck_MNAR
readRDS("ow_kNN_MCAR.rds") -> ow_kNN_MCAR
readRDS("ow_kNN_MAR.rds") -> ow_kNN_MAR
readRDS("ow_kNN_MNAR.rds") -> ow_kNN_MNAR
readRDS("ow_reg_MCAR.rds") -> ow_reg_MCAR
readRDS("ow_reg_MAR.rds") -> ow_reg_MAR
readRDS("ow_reg_MNAR.rds") -> ow_reg_MNAR
readRDS("ow_rf_MCAR.rds") -> ow_rf_MCAR
readRDS("ow_rf_MAR.rds") -> ow_rf_MAR
readRDS("ow_rf_MNAR.rds") -> ow_rf_MNAR
readRDS("ow_mul_MCAR.rds") -> ow_mul_MCAR
readRDS("ow_mul_MAR.rds") -> ow_mul_MAR
readRDS("ow_mul_MNAR.rds") -> ow_mul_MNAR

#Combine the datasets into one
all_data <- rbind(
  transform(ow_hotdeck_MCAR, method = "MCAR hot-deck"),
  transform(ow_hotdeck_MAR, method = "MAR hot-deck"),
  transform(ow_hotdeck_MNAR, method = "MNAR hot-deck"),
  transform(ow_kNN_MCAR, method = "MCAR kNN"),
  transform(ow_kNN_MAR, method = "MAR kNN"),
  transform(ow_kNN_MNAR, method = "MNAR kNN"),
  transform(ow_reg_MCAR, method = "MCAR regresyjna"),
  transform(ow_reg_MAR, method = "MAR regresyjna"),
  transform(ow_reg_MNAR, method = "MNAR regresyjna"),
  transform(ow_rf_MCAR, method = "MCAR random forest"),
  transform(ow_rf_MAR, method = "MAR random forest"),
  transform(ow_rf_MNAR, method = "MNAR random forest"),
  transform(ow_mul_MCAR, method = "MCAR wielokrotna"),
  transform(ow_mul_MAR, method = "MAR wielokrotna"),
  transform(ow_mul_MNAR, method = "MNAR wielokrotna")
)

colnames(all_data) <- c("Y", "Z", "X")


######Fisher test######
#Read .rds files
readRDS("fisher_hotdeck_MCAR.rds") -> fisher_hotdeck_MCAR
readRDS("fisher_hotdeck_MAR.rds") -> fisher_hotdeck_MAR
readRDS("fisher_hotdeck_MNAR.rds") -> fisher_hotdeck_MNAR
readRDS("fisher_kNN_MCAR.rds") -> fisher_kNN_MCAR
readRDS("fisher_kNN_MAR.rds") -> fisher_kNN_MAR
readRDS("fisher_kNN_MNAR.rds") -> fisher_kNN_MNAR
readRDS("fisher_reg_MCAR.rds") -> fisher_reg_MCAR
readRDS("fisher_reg_MAR.rds") -> fisher_reg_MAR
readRDS("fisher_reg_MNAR.rds") -> fisher_reg_MNAR
readRDS("fisher_rf_MCAR.rds") -> fisher_rf_MCAR
readRDS("fisher_rf_MAR.rds") -> fisher_rf_MAR
readRDS("fisher_rf_MNAR.rds") -> fisher_rf_MNAR
readRDS("fisher_mul_MCAR.rds") -> fisher_mul_MCAR
readRDS("fisher_mul_MAR.rds") -> fisher_mul_MAR
readRDS("fisher_mul_MNAR.rds") -> fisher_mul_MNAR

#Combine the datasets into one
all_data <- rbind(
  transform(fisher_hotdeck_MCAR, method = "MCAR hot-deck"),
  transform(fisher_hotdeck_MAR, method = "MAR hot-deck"),
  transform(fisher_hotdeck_MNAR, method = "MNAR hot-deck"),
  transform(fisher_kNN_MCAR, method = "MCAR kNN"),
  transform(fisher_kNN_MAR, method = "MAR kNN"),
  transform(fisher_kNN_MNAR, method = "MNAR kNN"),
  transform(fisher_reg_MCAR, method = "MCAR regresyjna"),
  transform(fisher_reg_MAR, method = "MAR regresyjna"),
  transform(fisher_reg_MNAR, method = "MNAR regresyjna"),
  transform(fisher_rf_MCAR, method = "MCAR random forest"),
  transform(fisher_rf_MAR, method = "MAR random forest"),
  transform(fisher_rf_MNAR, method = "MNAR random forest"),
  transform(fisher_mul_MCAR, method = "MCAR wielokrotna"),
  transform(fisher_mul_MAR, method = "MAR wielokrotna"),
  transform(fisher_mul_MNAR, method = "MNAR wielokrotna")
)

colnames(all_data) <- c("Y", "Z", "X")


