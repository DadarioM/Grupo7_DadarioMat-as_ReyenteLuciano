# 01_download_raw_argentina.R
# Descarga y guardado de datos para el análisis de Argentina (2000–2024)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# --- 1. LIMPIEZA Y CARGA DE LIBRERÍAS ----------------------------------------
rm(list = ls()); gc()

packages <- c("WDI", "readxl", "dplyr", "tidyr", "zoo", "writexl", "readr")
installed_packages <- packages %in% rownames(installed.packages())
if (any(!installed_packages)) install.packages(packages[!installed_packages])
lapply(packages, library, character.only = TRUE)

options(scipen = 999)

cat("\n=== INICIAMOS DESCARGA DE DATOS PARA ARGENTINA ===\n")

# --- 2. DESCARGA DE DATOS WDI ----------------------------------------------
indicators <- c(
  "NE.EXP.GNFS.KD",    # Exportaciones reales (X)
  "NE.IMP.GNFS.KD",    # Importaciones reales (M)
  "NY.GDP.MKTP.KD"     # PIB real (Y)
)

# ARGENTINA
wdi_arg <- WDI(country = "AR", indicator = indicators, start = 2000, end = 2024) %>%
  rename(
    X = NE.EXP.GNFS.KD,
    M = NE.IMP.GNFS.KD,
    Y = NY.GDP.MKTP.KD
  ) %>%
  mutate(country = "Argentina")

cat("✅ WDI Argentina descargado.\n")

# MUNDO
wdi_world <- WDI(country = "WLD", indicator = "NY.GDP.MKTP.KD",
                 start = 2000, end = 2024) %>%
  rename(Y_world = NY.GDP.MKTP.KD) %>%
  mutate(country = "World")

cat("✅ WDI Mundo descargado.\n")

# --- 3. GUARDAR RAW DATA ----------------------------------------------------
write_csv(wdi_arg,   "../data/raw/wdi_argentina_raw.csv")
write_csv(wdi_world, "../data/raw/wdi_world_raw.csv")

cat("📁 Archivos guardados en /data/raw\n")
cat("=== FIN SCRIPT 01 ===\n")
