# oleh Muhammad Anggoran Iwari (1906366425) - Fakultas Ekonomi dan Bisnis, Universitas Indonesia
# merujuk pada https://doi.org/10.1007/978-3-030-80519-7

# Memuat package seminr.
library(seminr)

# Memuat data untuk main test.
data <- load()

# Membuat model pengukuran (reflektif atau Mode_A), yakni variabel indikator dikelompokkan kepada masing-masing variabel laten.
measurement_model <- constructs(
  composite("PTI", multi_items("PTI", 1:3), weights = mode_A),
  composite("BDA", multi_items("BDA", 1:5), weights = mode_A),
  composite("IPK", multi_items("IPK", 1:3), weights = mode_A),
  composite("IPS", multi_items("IPS", 1:3), weights = mode_A),
  composite("KRP", multi_items("KRP", 1:3), weights = mode_A),
  higher_composite("INO", c("IPK", "IPS"))
)

# Membuat model struktural, yakni hubungan antara variabel laten.
structural_model <- relationships(
  paths(from = c("PTI", "BDA"), to = c("INO", "KRP")),
  paths(from = c("INO"), to = c("KRP"))
)

# Membuat estimasi model PLS-SEM.
pls_model <- estimate_pls(
  data = main_test_data,
  measurement_model = measurement_model,
  structural_model = structural_model
)
measurement_for_predict <- measurement_model[-length(measurement_model)]
structural_for_predict <- relationships(
  paths(from = c("PTI", "BDA"), to = c("IPK", "IPS", "KRP")),
  paths(from = c("IPK", "IPS"), to = c("KRP"))
)
model_for_predict <- estimate_pls(
  data = main_test_data,
  measurement_model = measurement_for_predict,
  structural_model = structural_for_predict
)

# Membuat rangkuman hasil estimasi model.
model_summary <- summary(pls_model)

# Melakukan bootstrap terhadap model.
boot_model <- bootstrap_model(pls_model, nboot = 10000)
boot_summary <- summary(boot_model, alpha = 0.10)


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
### Signifikansi: boot_summary$bootstrapped_paths pada kolom T Stat (alpha = 0.05 one-tailed, t-values > 1.645 agar signifikan).
### Relevansi: boot_summary$bootstrapped_paths pada kolom Original Est (> 0 untuk positif, < 0 untuk negatif).
boot_summary$bootstrapped_paths

## 3. Explanatory power: boot_summary$paths pada baris R^2 (0.25 ≤ x < 0.5 (rendah), 0.5 ≤ x < 0.75 (sedang), x ≥ 0.75 (tinggi)).
model_summary$paths

## 4. Predictive power: predict_summary pada baris RMSE, di mana PLS out-of-sample < LM out-of-sample (all: high, majority: medium, minority: low, none: lacks).
predict_model <- predict_pls(model = model_for_predict, noFolds = 10, reps = 10)
predict_summary <- summary(predict_model, alpha = 0.10)
predict_summary


# -- [ Analisis Pengaruh Mediasi ] --
# Signifikan adalah tidak ada nilai 0 pada confidence interval.

## 1. Tes indirect effect
specific_effect_significance(boot_model, from = "PTI", through = "INO", to = "KRP", alpha = 0.10)
specific_effect_significance(boot_model, from = "BDA", through = "INO", to = "KRP", alpha = 0.10)

## 2. Tes direct effect
boot_summary$bootstrapped_paths

## 3. Tes total effect (variabel dengan indirect effect & direct effect signifikan).
model_summary$paths["PTI", "KRP"] * model_summary$paths["PTI", "INO"] * model_summary$paths["INO", "KRP"]


# Membuat diagram SEM.
plot(pls_model)