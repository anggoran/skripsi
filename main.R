# oleh Muhammad Anggoran Iwari (1906366425) - Fakultas Ekonomi dan Bisnis, Universitas Indonesia

# Load seminr package
library(seminr)

# Memuat CSV yang berisikan data untuk Main Test.
goran_main_test_data <- read.csv(file = "maintest.csv", header = TRUE, sep = ",")

# Membuat model pengukuran (reflektif atau Mode_A), yakni variabel indikator dikelompokkan kepada masing-masing variabel laten.
measurement_model <- constructs(
  composite("PTI", multi_items("PTI", 1:3), weights = mode_A),
  composite("PBAT", multi_items("PBAT", 1:3), weights = mode_A),
  composite("PBD", multi_items("PBD", 1:2), weights = mode_A),
  composite("IPK", multi_items("IPK", 1:3), weights = mode_A),
  composite("IPS", multi_items("IPS", 1:3), weights = mode_A),
  composite("KF", multi_items("KF", 1:3), weights = mode_A),
  composite("KRS", multi_items("KRS", 1:3), weights = mode_A)
)

# Membuat model struktural, yakni hubungan antara variabel laten.
structural_model <- relationships(
  paths(from = c("PTI", "PBAT", "PBD"), to = c("IPK", "IPS", "KF", "KRS")),
  paths(from = c("IPK", "IPS"), to = c("KF", "KRS"))
)

# Membuat estimasi model PLS-SEM.
pls_model <- estimate_pls(
  data = goran_main_test_data,
  measurement_model = measurement_model,
  structural_model = structural_model
)

# Membuat rangkuman hasil estimasi model.
model_summary <- summary(pls_model)

# Melakukan bootstrap terhadap model.
boot_model <- bootstrap_model(pls_model, nboot = 10000)
boot_summary <- summary(boot_model, alpha = 0.05)


# -- [ Analisis Model Pengukuran ] --

## 1. Indicator reliability: model_summary$loadings pada matriks indicator loadings (≥ 0.708).
model_summary$loadings

## 2. Internal consistency reliability: model_summary$reliability pada kolom rhoC (≥ 0.70).
## 3. Convergent validity: model_summary$reliability pada kolom AVE (≥ 0.50).
model_summary$reliability

## 4. Discriminant validity: model_summary$validity$htmt pada matriks HTMT (< 0.90).
model_summary$validity$htmt


# -- [ Analisis Model Struktural ] --

## 1. Collinearity: model_summary$vif_antecendents pada barisan VIF (< 3 (rendah), 3 - 5 (sedang), ≥ 5 (tinggi)).
model_summary$vif_antecedents

## 2. Significance and relevance
### Signifikansi: boot_summary$bootstrapped_paths pada kolom T Stat (alpha = 0.05, t-values > 1.960 agar signifikan).
### Relevansi: boot_summary$bootstrapped_paths pada kolom Original Est (> 0 untuk positif, < 0 untuk negatif).
boot_summary$bootstrapped_paths

## 3. Explanatory power: boot_summary$paths pada baris R^2 (0.25 ≤ x < 0.5 (rendah), 0.5 ≤ x < 0.75 (sedang), x ≥ 0.75 (tinggi)).
model_summary$paths

## 4. Predictive power: predict_summary pada baris RMSE, di mana PLS out-of-sample < LM out-of-sample (all: high, majority: medium, minority: low, none: lacks).
predict_model <- predict_pls(model = pls_model, noFolds = 10, reps = 10)
predict_summary <- summary(predict_model)
predict_summary


# -- [ Analisis Pengaruh Mediasi ] --
# Signifikan adalah tidak ada nilai 0 pada confidence interval.

## 1. Tes indirect effect
model_summary$total_indirect_effects
specific_effect_significance(boot_model, from = "PTI", through = "IPK", to = "KF")
specific_effect_significance(boot_model, from = "PBD", through = "IPK", to = "KF")
specific_effect_significance(boot_model, from = "PBAT", through = "IPK", to = "KF")
specific_effect_significance(boot_model, from = "PTI", through = "IPK", to = "KRS")
specific_effect_significance(boot_model, from = "PBD", through = "IPK", to = "KRS")
specific_effect_significance(boot_model, from = "PBAT", through = "IPK", to = "KRS")
specific_effect_significance(boot_model, from = "PTI", through = "IPS", to = "KF")
specific_effect_significance(boot_model, from = "PBD", through = "IPS", to = "KF")
specific_effect_significance(boot_model, from = "PBAT", through = "IPS", to = "KF")
specific_effect_significance(boot_model, from = "PTI", through = "IPS", to = "KRS")
specific_effect_significance(boot_model, from = "PBD", through = "IPS", to = "KRS")
specific_effect_significance(boot_model, from = "PBAT", through = "IPS", to = "KRS")

## 2. Tes direct effect
model_summary$paths
boot_summary$bootstrapped_paths

## 3. Tes total effect (variabel dengan indirect effect & direct effect signifikan).
model_summary$paths["PBAT", "KF"] * model_summary$paths["PBAT", "IPS"] * model_summary$paths["IPS", "KF"]


# Membuat diagram SEM.
plot(pls_model)