# oleh Muhammad Anggoran Iwari (1906366425) - Fakultas Ekonomi dan Bisnis, Universitas Indonesia
# merujuk pada https://doi.org/10.3390/jtaer17040068

# Memuat package psych.
library(psych)

# Memuat data untuk pilot test.
n_pilot <- 50
data <- load()
data <- data[1:n_pilot, ]

# Membuat correlation matrix
corr_matrix <- cor(data)

# Mencari Cronbach's alpha untuk analisis reliabilitas (> 0.700)
alpha(data[, c("PTI1", "PTI2", "PTI3")])$total$raw_alpha
alpha(data[, c("BDA1", "BDA2", "BDA3", "BDA4", "BDA5")])$total$raw_alpha
alpha(data[, c("IPK1", "IPK2", "IPK3")])$total$raw_alpha
alpha(data[, c("IPS1", "IPS2", "IPS3")])$total$raw_alpha
alpha(data[, c("KRP1", "KRP2", "KRP3")])$total$raw_alpha

# Mencari KMO-MSA dan Bartlett's sphericity untuk kecukupan data dalam analisis (MSA ≥ 0.600 dan p value < 0.050)
KMO(corr_matrix)$MSA
cortest.bartlett(corr_matrix, n = n_pilot)$p.value

# Mencari factor loadings untuk analisis validitas (≥ 0.600)
principal(r, nfactors = 1)$loadings