#####Median imputation#####

######MCAR######


######MAR######


######MNAR######


#####Hot-deck imputation#####

######MCAR######


######MAR######


######MNAR######

#####kNN imputation#####

######MCAR######


######MAR######


######MNAR######

#####Regression imputation#####

######MCAR######


######MAR######


######MNAR######


#####Random forest imputation#####

######MCAR######


######MAR######


######MNAR######


#####Multiple imputation#####

######MCAR######


######MAR######


######MNAR######





readRDS("imp_median_data_MCAR.rds") -> imp_median_data_MCAR
readRDS("imp_median_data_MAR.rds") -> imp_median_data_MAR
readRDS("imp_median_data_MNAR.rds") -> imp_median_data_MNAR
readRDS("imp_hot_deck_data_MCAR.rds") -> imp_hotdeck_data_MCAR
readRDS("imp_hot_deck_data_MAR.rds") -> imp_hotdeck_data_MAR
readRDS("imp_hot_deck_data_MNAR.rds") -> imp_hotdeck_data_MNAR
readRDS("imp_kNN_data_MCAR.rds") -> imp_kNN_data_MCAR
readRDS("imp_kNN_data_MAR.rds") -> imp_kNN_data_MAR
readRDS("imp_kNN_data_MNAR.rds") -> imp_kNN_data_MNAR
readRDS("imp_reg_data_MCAR.rds") -> imp_reg_data_MCAR
readRDS("imp_reg_data_MAR.rds") -> imp_reg_data_MAR
readRDS("imp_reg_data_MNAR.rds") -> imp_reg_data_MNAR
readRDS("imp_rf_data_MCAR.rds") -> imp_rf_data_MCAR
readRDS("imp_rf_data_MAR.rds") -> imp_rf_data_MAR
readRDS("imp_rf_data_MNAR.rds") -> imp_rf_data_MNAR
readRDS("imp_mul_data_MCAR.rds") -> imp_mul_data_MCAR
readRDS("imp_mul_data_MAR.rds") -> imp_mul_data_MAR
readRDS("imp_mul_data_MNAR.rds") -> imp_mul_data_MNAR

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

# Rename columns
colnames(all) <- c("na_frac", "lower", "upper", "coverage", "method")

# Wczytaj pakiet dplyr
library(dplyr)

# Wybierz kolumny i zmień nazwy
all_modified <- all %>%
  dplyr::select(method, na_frac, coverage)

# Wyświetl zmodyfikowaną ramkę danych
print(all_modified)

# Wczytaj pakiet tidyverse
library(tidyverse)

# Wczytaj pakiet tidyverse
library(tidyverse)

# Przekształć ramkę danych
all_wide <- all %>%
  dplyr::select(method, na_frac, coverage) %>%
  spread(key = na_frac, value = coverage)

# Wyświetl zmodyfikowaną ramkę danych
print(all_wide)

# Zainstaluj i wczytaj pakiet writexl, jeśli jeszcze tego nie zrobiłeś
if (!requireNamespace("writexl", quietly = TRUE)) {
  install.packages("writexl")
}
library(writexl)

# Zapisz ramkę danych all_wide do pliku Excel
write_xlsx(all_wide, "all_wide.xlsx")