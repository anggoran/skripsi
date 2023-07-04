# oleh Muhammad Anggoran Iwari (1906366425) - Fakultas Ekonomi dan Bisnis, Universitas Indonesia

load <- function() {
  # Memuat data dari CSV.
  data <- read.csv(file = "data.csv", header = TRUE, sep = ",")
  
  # Merapikan data untuk analisis (apabila ada perubahan dengan kuesioner).
  data <- subset(data, select = -c(KF1, KF2, KF3))
  names(data) <- gsub("KRS", "KRP", names(data))
  old_bda_names <- c("PBAT1", "PBAT2", "PBAT3", "PBD1", "PBD2")
  new_bda_names <- paste0("BDA", 1:5)
  changed_columns <- match(old_bda_names, names(data))
  names(data)[changed_columns] <- new_bda_names
  
  return(data)
}