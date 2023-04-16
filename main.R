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

# Membuat estimasi model.
pls_model <- estimate_pls(
  data = goran_main_test_data,
  measurement_model = measurement_model,
  structural_model = structural_model
)

# Membuat rangkuman hasil estimasi model.
model_summary <- summary(pls_model)

# Membuat diagram SEM
plot(pls_model)