# ==============================================================================
# PROJEKT: MAPA RODZINNA V10 - FORMAT 20:30 (STRUKTURA POZIOMA) + KLASYCZNE PUNKTY
# ==============================================================================

if (!require("geodata")) install.packages("geodata")
if (!require("terra")) install.packages("terra")
if (!require("maptiles")) install.packages("maptiles")

library(geodata)
library(terra)
library(maptiles)

# 1. POBIERANIE DANYCH GADM
message("Pobieranie bazy geograficznej...")
poland_gminy <- gadm(country = "POL", level = 3, path = ".")

# 2. SZTYWNE KADROWANIE REGIONU
okno_widoku <- ext(20.35, 21.95, 51.95, 52.42)
mapa_regionu <- crop(poland_gminy, okno_widoku)

# 3. IDENTYFIKACJA WARSZAWY I LATOWICZA
indeks_warszawa <- mapa_regionu$NAME_2 == "Warszawa"
indeks_latowicz <- grep("Latowicz", mapa_regionu$NAME_3, ignore.case = TRUE)

# 4. POBIERANIE PODKŁADU SATELITARNEGO
message("Pobieranie podkładu satelitarnego...")
podklad_sat <- get_tiles(okno_widoku, provider = "Esri.WorldImagery", zoom = 11, crop = TRUE)

# 5. PARAMETRY GRAFIKI - PROPORCJA VIA 20:30 (3600 x 2400 px, 300 DPI)
plik_wyjsciowy <- "mapa_przodkow_czytelna.jpg"
jpeg(filename = plik_wyjsciowy, width = 3600, height = 2400, res = 300, quality = 100)

# Marginesy i kremowe tło ramki retro
par(mar = c(2, 2, 5, 2), bg = "#FDFBF7", family = "serif")

# ==============================================================================
# RYSOWANIE WARSTW
# ==============================================================================

# KROK A: Inicjalizacja kadru
plot(okno_widoku, col = NA, border = NA, axes = FALSE, mar = NA)

# KROK B: Podkład satelitarny
plotRGB(podklad_sat, add = TRUE)

# KROK C: Uwypuklona siatka granic gmin
plot(mapa_regionu, col = NA, border = "#FFFFFF65", lwd = 0.9, add = TRUE)

# 6. UWYRAŹNIONE PODŚWIETLENIE WARSZAWY (Obszar miejski)
if(sum(indeks_warszawa) > 0) {
  plot(mapa_regionu[indeks_warszawa, ], col = "#B84D4755", border = "#9E3A34C0", lwd = 2.0, add = TRUE)
}

# 7. UWYRAŹNIONE PODŚWIETLENIE GMINY LATOWICZ (Obszar wiejski)
if(length(indeks_latowicz) > 0) {
  plot(mapa_regionu[indeks_latowicz, ], col = "#801C1577", border = "#60100B", lwd = 3.5, add = TRUE)
}

# 8. PUNKT I OPIS - JELONKI
jelonki_x <- 20.916327
jelonki_y <- 52.231735

points(jelonki_x, jelonki_y, col = "#A8221844", pch = 16, cex = 5) 
points(jelonki_x, jelonki_y, col = "#A82218", pch = 16, cex = 2.0)  
points(jelonki_x, jelonki_y, col = "#FFFFFF", pch = 1, cex = 2.0, lwd = 1.2) 

text(jelonki_x, jelonki_y + 0.09, labels = "JELONKI (WARSZAWA)\nStąd pochodzi współczesna\nrodzina Bontruk od XX do XXI w.", 
     cex = 0.85, font = 2, col = "#FFFFFF", pos = 3)

arrows(jelonki_x, jelonki_y + 0.08, jelonki_x, jelonki_y + 0.01, length = 0.09, col = "#000000", lwd = 4)
arrows(jelonki_x, jelonki_y + 0.08, jelonki_x, jelonki_y + 0.01, length = 0.09, col = "#FFFFFF", lwd = 2)

# 9. OPIS DLA GMINY LATOWICZ
if(length(indeks_latowicz) > 0) {
  coords_l <- geom(centroids(mapa_regionu[indeks_latowicz, ]))[, c("x", "y")]
  
  text(coords_l[1], coords_l[2] + 0.10, 
       labels = "GMINA LATOWICZ\nZ tego regionu pochodzi większość\nprzodków od XVIII do XX w.", 
       cex = 0.85, font = 2, col = "#FFFFFF", pos = 3)
  
  arrows(coords_l[1], coords_l[2] + 0.09, coords_l[1], coords_l[2] + 0.01, length = 0.09, col = "#000000", lwd = 4)
  arrows(coords_l[1], coords_l[2] + 0.09, coords_l[1], coords_l[2] + 0.01, length = 0.09, col = "#FFFFFF", lwd = 2)
}

# 10. PUNKTY MIEJSCOWOŚCI I ETYKIETA WARSZAWY (Czyste białe napisy)

# Funkcja do rysowania standardowego punktu z podpisem
rysuj_punkt_czysty_bialy <- function(x, y, label, pos_napis) {
  points(x, y, pch = 18, col = "#000000", cex = 1.4) 
  points(x, y, pch = 18, col = "#FFFFFF", cex = 1.0) 
  text(x, y, labels = label, cex = 0.85, font = 2, col = "#FFFFFF", pos = pos_napis)
}

# Warszawa - czysty napis idealnie wycentrowany na środku obszaru
text(21.05, 52.23, labels = "Warszawa", cex = 0.95, font = 2, col = "#FFFFFF")

# Wszystkie miejscowości rysowane jednolitym, klasycznym stylem (białe romby)
rysuj_punkt_czysty_bialy(20.543739, 52.274804, "Szymanówek", 3)
rysuj_punkt_czysty_bialy(21.818898, 52.060379, "Wężyczyn", 4)
rysuj_punkt_czysty_bialy(21.809306, 52.044896, "Dąbrówka", 4)
rysuj_punkt_czysty_bialy(21.806137, 52.026330, "Latowicz", 4)
rysuj_punkt_czysty_bialy(21.698766, 52.037496, "Budy Wielgoleskie", 2)
rysuj_punkt_czysty_bialy(21.727557, 52.018085, "Chyżyny", 2)

# 11. BLOK TYTUŁOWY I RAMKI PASSE-PARTOUT
title(main = "ARCHIWUM RODZINY BONTRUK", cex.main = 1.6, font.main = 2, col = "#2C2A29", line = 1.5)
mtext("Obszary pochodzenia przodków", side = 3, line = 0.2, cex = 1.1, col = "#65615E", font = 3)

dev.off()
message("Plik w proporcjach 20:30 ze zwykłymi punktami został wygenerowany.")