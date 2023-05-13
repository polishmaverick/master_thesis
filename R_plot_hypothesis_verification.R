######Oneway test######
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
readRDS("ow_regresyjna_MCAR.rds") -> ow_regresyjna_MCAR
readRDS("ow_regresyjna_MAR.rds") -> ow_regresyjna_MAR
readRDS("ow_regresyjna_MNAR.rds") -> ow_regresyjna_MNAR
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
  transform(ow_regresyjna_MCAR, method = "MCAR random forest"),
  transform(ow_regresyjna_MAR, method = "MAR random forest"),
  transform(ow_regresyjna_MNAR, method = "MNAR random forest"),
  transform(ow_mul_MCAR, method = "MCAR wielokrotna"),
  transform(ow_mul_MAR, method = "MAR wielokrotna"),
  transform(ow_mul_MNAR, method = "MNAR wielokrotna")
)

colnames(all_data) <- c("Y", "Z", "X")

#Setting colors
color_mapping <- c("MCAR hot-deck" = "#28192FFF",
                   "MAR hot-deck" = "#28192FFF",
                   "MNAR hot-deck" = "#28192FFF",
                   "MCAR kNN" = "#40498EFF",
                   "MAR kNN" = "#40498EFF",
                   "MNAR kNN" = "#40498EFF",
                   "MCAR regresyjna" = "#366A9FFF",
                   "MAR regresyjna" = "#366A9FFF",
                   "MNAR regresyjna" = "#366A9FFF",
                   "MCAR random forest" = "#348AA6FF",
                   "MAR random forest" = "#348AA6FF",
                   "MNAR random forest" = "#348AA6FF",
                   "MCAR wielokrotna" = "#38AAACFF",
                   "MAR wielokrotna" = "#38AAACFF",
                   "MNAR wielokrotna" = "#38AAACFF")

#Plot
ggplot(all_data, aes(x = Y, y = Z)) +
  geom_point(aes(color = X), size = 3) +
  labs(x = "Frakcja braków danych", y = "% przypadków odrzucenia hipotezy H0") +
  facet_wrap(~ X, scales = "fixed", nrow = 5) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 20, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        strip.text.x = element_text(size = 15),
        axis.title = element_text(size = 25)) +
  scale_color_manual(values = color_mapping) +
  scale_x_continuous(breaks = seq(0, 70, 10),
                     labels = function(x) paste0(x, "%"))

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
readRDS("fisher_regresyjna_MCAR.rds") -> fisher_regresyjna_MCAR
readRDS("fisher_regresyjna_MAR.rds") -> fisher_regresyjna_MAR
readRDS("fisher_regresyjna_MNAR.rds") -> fisher_regresyjna_MNAR
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
  transform(fisher_regresyjna_MCAR, method = "MCAR random forest"),
  transform(fisher_regresyjna_MAR, method = "MAR random forest"),
  transform(fisher_regresyjna_MNAR, method = "MNAR random forest"),
  transform(fisher_mul_MCAR, method = "MCAR wielokrotna"),
  transform(fisher_mul_MAR, method = "MAR wielokrotna"),
  transform(fisher_mul_MNAR, method = "MNAR wielokrotna")
)

colnames(all_data) <- c("Y", "Z", "X")

#Setting colors
color_mapping <- c("MCAR hot-deck" = "#28192FFF",
                   "MAR hot-deck" = "#28192FFF",
                   "MNAR hot-deck" = "#28192FFF",
                   "MCAR kNN" = "#40498EFF",
                   "MAR kNN" = "#40498EFF",
                   "MNAR kNN" = "#40498EFF",
                   "MCAR regresyjna" = "#366A9FFF",
                   "MAR regresyjna" = "#366A9FFF",
                   "MNAR regresyjna" = "#366A9FFF",
                   "MCAR random forest" = "#348AA6FF",
                   "MAR random forest" = "#348AA6FF",
                   "MNAR random forest" = "#348AA6FF",
                   "MCAR wielokrotna" = "#38AAACFF",
                   "MAR wielokrotna" = "#38AAACFF",
                   "MNAR wielokrotna" = "#38AAACFF")

#Plot
ggplot(all_data, aes(x = Y, y = Z)) +
  geom_point(aes(color = X), size = 3) +
  labs(x = "Frakcja braków danych", y = "% przypadków odrzucenia hipotezy H0") +
  facet_wrap(~ X, scales = "fixed", nrow = 5) +
  theme_bw() +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 20),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, size = 20, color = "black"),
        axis.text.y = element_text(size = 20, color = "black"),
        strip.text.x = element_text(size = 15),
        axis.title = element_text(size = 25)) +
  scale_color_manual(values = color_mapping) +
  scale_x_continuous(breaks = seq(0, 70, 10),
                     labels = function(x) paste0(x, "%"))