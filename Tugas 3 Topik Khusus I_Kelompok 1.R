# Data untuk membuat peta
library(sf)
# https://geosai.my.id/download-shp-kabupaten-kota-indonesia/
maps <- st_read("D:/Materi Kuliah UI/Topik Khusus I - Analisis Data Spasial/Tugas Topik Khusus I - Analisis Data Spasial/Sumatera_Utara_ADMIN_BPS.shp")
maps$Kabupaten <- gsub("Kota ", "", maps$Kabupaten)
maps <- maps[order(maps$Kabupaten), ]
class(maps)
library(ggplot2)
ggplot(maps) + geom_sf()

# Data Tingkat Pengangguran Terbuka
library(readxl)
df <- read_excel("D:/Materi Kuliah UI/Topik Khusus I - Analisis Data Spasial/Tugas Topik Khusus I - Analisis Data Spasial/data_sumut.xlsx")
df <- data.frame(df)
colnames(df)[1] <- "kab_kota"
df <- df[-1, ] # Menghilangkan Sumatera Utara
df <- df[order(df$kab_kota), ]
df[, -1] <- lapply(df[, -1], as.numeric)
View(df)

# Preprocessing
# Periksa nama kabupaten/kota
maps <- maps[!maps$Kabupaten=="Danau Toba", ]
df$kab_kota==maps$Kabupaten
df[df$kab_kota=="Labuhan Batu", ]$kab_kota <- "Labuhanbatu"
df[df$kab_kota=="Labuanbatu Utara", ]$kab_kota <- "Labuhanbatu Utara"
maps[maps$Kabupaten=="Labuhan Batu", ]$Kabupaten <- "Labuhanbatu"
maps[maps$Kabupaten=="Labuhan Batu Selatan", ]$Kabupaten <- "Labuhanbatu Selatan"
maps[maps$Kabupaten=="Labuhan Batu Utara", ]$Kabupaten <- "Labuhanbatu Utara"
maps[maps$Kabupaten=="Tanjung Balai", ]$Kabupaten <- "Tanjungbalai"
maps[maps$Kabupaten=="Pematang Siantar", ]$Kabupaten <- "Pematangsiantar"
maps[maps$Kabupaten=="Toba Samosir", ]$Kabupaten <- "Toba"
df <- df[order(df$kab_kota), ]
maps <- maps[order(maps$Kabupaten), ]
colnames(maps)[5] <- "kab_kota"
df$kab_kota==maps$kab_kota

# Merge data
library(dplyr)
df_merge_sp <- inner_join(maps, df, by = "kab_kota")
df_merge_sp <- as(df_merge_sp, "Spatial")

# Cari centroid koordinat 
centroid <- st_centroid(maps)
coords <- st_coordinates(centroid)
# Cek centroid pada peta
ggplot(maps) + geom_sf(aes(fill = df$kab_kota), show.legend = FALSE) + 
  geom_sf(data = centroid, col = "black") 
# Save koordinat
library(sp)
df_sp <- SpatialPointsDataFrame(coords, df)

# Label plot
label <- c()
i <- 1
for(lab in df$kab_kota){
  if (length(strsplit(lab, " ")[[1]]) > 1 || nchar(lab)>4){
    # Extract first three-letter string
    first_three <- gsub("\\b(\\w{3})\\w*\\b", "\\1", lab)
    first_three <- gsub("\\s", "", first_three)
    label[i] <- first_three
  } else{
    label[i] <- df$kab_kota[i]
  }
  i <- i + 1
}
label
labels <- data.frame(label = label, long = coords[, 1], lat = coords[, 2])

# Statistika Deskriptif
summary(df[, c("TPT_2022", "PPM_2022", "DR_2022", "GR_2022", "RRLS_2022")])
df[df$TPT_2022==min(df$TPT_2022), ]
df[df$TPT_2022==max(df$TPT_2022), ]

# Tingkat Pengangguran Terbuka (TPT) Tahun 2022
# https://maczokni.github.io/crime_mapping_textbook/spatial-regression-models.html#fitting-an-interpreting-a-spatial-error-model
library(tmap)
tm_shape(df_merge_sp) + 
  tm_fill("TPT_2022", title = "TPT", style = "quantile", palette = "Reds") +
  tm_borders(alpha = 0.1) +
  tm_layout(main.title = "Tingkat Pengangguran Terbuka (TPT) \ndi Provinsi Sumatera Utara Tahun 2022", 
            main.title.size = 0.7 , legend.position = c("left", "top"),
            legend.outside = TRUE, legend.title.size = 0.8)

library(ggspatial)
df[order(df$TPT_2022), ]
p1 <- ggplot(maps) + geom_sf(aes(fill = df$TPT_2022)) +
  geom_text(data = labels, aes(x = long, y = lat, label = label), size = 4, color = "black") + 
  scale_fill_gradient(name = "TPT", low = "lightblue", high = "darkblue") + 
  ggtitle("Tingkat Pengangguran Terbuka (TPT) \ndi Provinsi Sumatera Utara Tahun 2022") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 14, face = "bold"))
# https://bookdown.org/brianwood1/QDASS/simple-static-maps.html
p1

# Persentase Penduduk Miskin (PPM) Tahun 2022
p2 <- ggplot(maps) + geom_sf(aes(fill = df$PPM_2022)) + 
  scale_fill_gradient(name = "PPM", low = "lightgreen", high = "darkgreen") + 
  ggtitle("Persentase Penduduk Miskin (PPM) \ndi Provinsi Sumatera Utara Tahun 2022") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 14, face = "bold"))
p2

# Dependency Ratio (DR) Tahun 2022
p3 <- ggplot(maps) + geom_sf(aes(fill = df$DR_2022)) + 
  scale_fill_gradient(name = "DR", low = "yellow", high = "red") + 
  ggtitle("Dependency Ratio (DR) \ndi Provinsi Sumatera Utara Tahun 2022") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 14, face = "bold"))
p3

# Gini Rasio (GR) Tahun 2022
library(viridis)
p4 <- ggplot(maps) + geom_sf(aes(fill = df$GR_2022)) + 
  scale_fill_viridis_c(option = "cividis",  name = "GR", direction = -1) + 
  ggtitle("Gini Rasio (GR) \ndi Provinsi Sumatera Utara Tahun 2022") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 14, face = "bold"))
p4

# Rata-Rata Lama Sekolah (RRLS) Tahun 2022
p5 <- ggplot(maps) + geom_sf(aes(fill = df$RRLS_2022)) + 
  scale_fill_viridis_c(option = "rocket",  name = "RRLS", direction = -1) + 
  ggtitle("Rata-Rata Lama Sekolah (RRLS) \ndi Provinsi Sumatera Utara Tahun 2022") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 14, face = "bold"))
p5

# Gabungan Plot
library(ggpubr)
ggarrange(p2, p3, p4, p5, ncol = 2, nrow = 2)

# Plot Transformasi Variabel
df$log_TPT <- log(df$TPT_2022)
density_data1 <- data.frame(x = df$TPT_2022, group = "Sebelum")
density_data2 <- data.frame(x = log(df$TPT_2022), group = "Sesudah")
# Combine the datasets
density_data <- rbind(density_data1, density_data2)
# Create the plot
ggplot(density_data, aes(x = x, fill = group)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Perbandingan Densitas Sebelum dan Sesudah \nTransformasi Logaritma Natural",
       x = "Values", y = "Density") +
  theme_minimal()

# Regresi Linier
reg <- lm(log(TPT_2022) ~ PPM_2022 + DR_2022 + GR_2022 + RRLS_2022, data = df)
summary(reg)
# Log-Likelihood
logLik(reg)
# AIC
AIC(reg)

# Residual
df_merge_sp$res_reg <- residuals(reg)
df_merge_sp$fitted_reg <- fitted(reg)
df_merge_sp$sd_breaks <- scale(df_merge_sp$res_reg)[, 1] 
# Because scale is made for matrices, we just need to get the first column using [, 1]
# this is equal to (df_merge_sp$res_reg - mean(df_merge_sp$res_reg))/sd(df_merge_sp$res_reg)
summary(df_merge_sp$sd_breaks)
my_breaks <- c(-14, -3, -2, -1, 1, 2, 3, 14)
library(tmap)
tm_shape(df_merge_sp) + 
  tm_fill("sd_breaks", title = "Residual", style = "fixed", breaks = my_breaks, palette = "-RdBu") +
  tm_borders(alpha = 0.1) +
  tm_layout(main.title = "Residual", main.title.size = 0.7 ,
            legend.position = c("right", "bottom"), legend.title.size = 0.8,
            legend.outside = TRUE)

# Uji Asumsi
# Uji Multikolinearitas
library(car)
vif(reg)
cor(df[, -1])

# Uji Normalitas
qqnorm(reg$residuals, ylab = "Residuals", xlab = "Normal Scores")
qqline(reg$residuals)
shapiro.test(reg$residuals)
ks.test(reg$residuals, "pnorm")

# Uji Durbin Watson (Autocorrelation)
library(lmtest)
dwtest(log(TPT_2022) ~ PPM_2022 + DR_2022 + GR_2022 + RRLS_2022, data = df)

# Uji Heteroskedastisitas
bptest(reg)
spreadLevelPlot(reg)
par(mfrow = c(2, 2)) 
plot(reg)
par(mfrow = c(1, 1)) 
plot(reg, 1)

# Autokorelasi Spasial/Dependensi Spasial
library(spdep)
# Create a list of neighbours using the Queen criteria
w <- poly2nb(df_merge_sp, row.names = df_merge_sp$kab_kota) # sp object
summary(w)
# Generates a weights matrix for a neighbours list with spatial weights
wm <- nb2mat(w, style = 'B')
# Convert a square spatial weights matrix to a weights list object
rwm <- mat2listw(wm, style = 'W')

# Uji Indeks Moran
# Variabel Dependen
moran.test(log(df_merge_sp$TPT_2022), rwm, alternative = "two.sided")
# Variabel Independen
moran.test(df_merge_sp$PPM_2022, rwm, alternative = "two.sided")
moran.test(df_merge_sp$DR_2022, rwm, alternative = "two.sided")
moran.test(df_merge_sp$GR_2022, rwm, alternative = "two.sided")
moran.test(df_merge_sp$RRLS_2022, rwm, alternative = "two.sided")
# Residual
lm.morantest(reg, rwm, alternative = "two.sided")

# Lagrange Multipliers
lm.LMtests(reg, rwm, test = c("LMerr", "LMlag", "RLMerr", "RLMlag", "SARMA"))

# Spatial Regression
# https://rpubs.com/r_anisa/Spatial-Durbin
# https://r-spatial.org/book/17-Econometrics.html
# https://chrismgentry.github.io/Spatial-Regression/

# Spatial Lag Model (SLM)/Spatial Autoregressive (SAR) Model
library(spatialreg)
model <- "log(TPT_2022) ~ PPM_2022 + DR_2022 + GR_2022 + RRLS_2022"
sar <- lagsarlm(model, data = df_merge_sp, rwm)
summary(sar)
# Impact measures
W <- as(rwm, "CsparseMatrix")
trMC <- trW(W, type = "MC")
im <- impacts(sar, tr = trMC, R = 100)
sums <- summary(im,  zstats = TRUE)
data.frame(sums$res)
# To print the p-values
data.frame(sums$pzmat)
# Asumsi
ks.test(sar$residuals, "pnorm")
bptest.Sarlm(sar)
# Log-Likelihood
sar$LL
# AIC
AIC(sar)

# Spatial Error Model (SEM)
sem <- errorsarlm(model, data = df_merge_sp, rwm)
summary(sem)
# Asumsi
ks.test(sem$residuals, "pnorm")
bptest.Sarlm(sem)
# Log-Likelihood
sem$LL
# AIC
AIC(sem)

# Spatial Lag Exogeneous (SLX) Model
slx <- lmSLX(model, data = df_merge_sp, rwm, Durbin = TRUE)
summary(slx)
# Asumsi
ks.test(slx$residuals, "pnorm")
bptest(slx)
# Log-Likelihood
logLik(slx)
# AIC
AIC(slx)

# Spatial Durbin Model (SDM)
sdm <- lagsarlm(model, data = df_merge_sp, rwm, type = "mixed")
summary(sdm)
sdm <- lagsarlm(model, data = df_merge_sp, rwm, Durbin = TRUE)
summary(sdm)
# Asumsi
ks.test(sdm$residuals, "pnorm")
bptest.Sarlm(sdm)
# Log-Likelihood
sdm$LL
# AIC
AIC(sdm)

# Spatial Durbin Error Model (SDEM)
sdem <- errorsarlm(model, data = df_merge_sp, rwm, etype = "mixed")
summary(sdem)
sdem <- errorsarlm(model, data = df_merge_sp, rwm, Durbin = TRUE)
summary(sdem)
# Asumsi
ks.test(sdem$residuals, "pnorm")
bptest.Sarlm(sdem)
# Log-Likelihood
sdem$LL
# AIC
AIC(sdem)

# Kelejian-Purcha Model/Spatial Autoregressive Combined (SAC) Model
sac <- sacsarlm(model, data = df_merge_sp, rwm, Durbin = FALSE)
summary(sac)
# Asumsi
ks.test(sac$residuals, "pnorm")
bptest.Sarlm(sac)
# Log-Likelihood
sac$LL
# AIC
AIC(sac)

# General Nested Spatial Model (GSNM)
gnsm <- sacsarlm(model, data = df_merge_sp, rwm, Durbin = TRUE)
summary(gnsm)
# Asumsi
ks.test(gnsm$residuals, "pnorm")
bptest.Sarlm(gnsm)
# Log-Likelihood
gnsm$LL
# AIC
AIC(gnsm)

# Visualisasi Residual
# Regresi Linier (OLS)
r1 <- ggplot(maps) + geom_sf(aes(fill = reg$residuals)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white", 
                       high = "blue", midpoint = 0) + 
  ggtitle("Residual Model Regresi Linier (OLS)") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
# SAR
r2 <- ggplot(maps) + geom_sf(aes(fill = sar$residuals)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white", 
                       high = "blue", midpoint = 0) + 
  ggtitle("Residual Model Spatial Autoregressive (SAR)") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
# SEM
r3 <- ggplot(maps) + geom_sf(aes(fill = sem$residuals)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white", 
                       high = "blue", midpoint = 0) + 
  ggtitle("Residual Spatial Error Model (SEM)") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
# SLX
r4 <- ggplot(maps) + geom_sf(aes(fill = slx$residuals)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white", 
                       high = "blue", midpoint = 0) + 
  ggtitle("Residual Model Spatial Lag Exogenous (SLX)") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
# SDM
r5 <- ggplot(maps) + geom_sf(aes(fill = sdm$residuals)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white", 
                       high = "blue", midpoint = 0) + 
  ggtitle("Residual Spatial Durbin Model (SDM)") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
# SDEM
r6 <- ggplot(maps) + geom_sf(aes(fill = sdem$residuals)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white", 
                       high = "blue", midpoint = 0) + 
  ggtitle("Residual Spatial Durbin Error Model (SDEM)") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
# SAC
r7 <- ggplot(maps) + geom_sf(aes(fill = sac$residuals)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white", 
                       high = "blue", midpoint = 0) + 
  ggtitle("Residual Model Spatial Autoregressive Combined (SAC)") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
# GNSM
r8 <- ggplot(maps) + geom_sf(aes(fill = gnsm$residuals)) + 
  scale_fill_gradient2(name = "Residual", low = "red", mid = "white", 
                       high = "blue", midpoint = 0) + 
  ggtitle("Residual General Nested Spatial Model (GSNM)") +
  xlab("Longitude") + ylab("Latitude") +
  annotation_scale(location = "bl", width_hint = 0.4) +
  annotation_north_arrow(location = "bl", which_north = "true",
                         pad_x = unit(0.0, "in"), pad_y = unit(0.2, "in"),
                         style = north_arrow_fancy_orienteering) +
  theme(plot.title = element_text(size = 10, face = "bold"))
ggarrange(r1, r2, r3, r4, ncol = 2, nrow = 2)
ggarrange(r5, r6, r7, r8, ncol = 2, nrow = 2)

# Save Hasil
df$longitude <- coords[, 1]
df$latitude <- coords[, 2]
library("writexl")
tabel_data <- df[, c("kab_kota", "TPT_2022", "PPM_2022", "DR_2022", "GR_2022", "RRLS_2022", 
                     "longitude", "latitude")]
write_xlsx(tabel_data, "D:/Materi Kuliah UI/Topik Khusus I - Analisis Data Spasial/Tugas Topik Khusus I - Analisis Data Spasial/tabel_data3.xlsx")
