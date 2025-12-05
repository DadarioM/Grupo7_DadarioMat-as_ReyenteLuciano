# 02_download_raw_comparativo.R
# Descarga y guardado de datos para análisis comparativo internacional (2000-2024)
# Incluye 10 países y REER; ITCRM ajustado para Argentina en el script 04

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# --- 1. LIMPIEZA Y CARGA DE LIBRERÍAS ----------------------------------------
rm(list = ls()); gc()

packages <- c("WDI", "readxl", "dplyr", "tidyr", "zoo", "writexl", "readr")
installed_packages <- packages %in% rownames(installed.packages())
if (any(!installed_packages)) install.packages(packages[!installed_packages])
lapply(packages, library, character.only = TRUE)

options(scipen = 999)

cat("\n=== INICIAMOS DESCARGA DE DATOS COMPARATIVOS INTERNACIONALES ===\n")

# --- 2. PARÁMETROS -----------------------------------------------------------
start_year <- 2000
end_year   <- 2024

indicators <- c(
  "NE.EXP.GNFS.KD",   # X
  "NE.IMP.GNFS.KD",   # M
  "NY.GDP.MKTP.KD",   # Y
  "PX.REX.REER"       # REER (no existe para Argentina)
)

paises  <- c("AR","UY","CL","BR","MX","KR","US","DE","SG","AU")
nombres <- c("Argentina","Uruguay","Chile","Brasil","México","Corea del Sur",
             "Estados Unidos","Alemania","Singapur","Australia")

# --- 3. DESCARGA WDI POR PAÍS ------------------------------------------------

lista_paises <- list()

for(i in seq_along(paises)){
  code <- paises[i]
  name <- nombres[i]
  cat("\nDescargando WDI para", name, "(", code, ") ...\n")
  
  df <- WDI(country = code, indicator = indicators,
            start = start_year, end = end_year)
  
  df <- df %>%
    rename(
      X = NE.EXP.GNFS.KD,
      M = NE.IMP.GNFS.KD,
      Y = NY.GDP.MKTP.KD,
      R = PX.REX.REER
    ) %>%
    mutate(
      country = name,
      iso2c = code
    ) %>%
    arrange(year)
  
  # Guardado individual
  write_csv(df, paste0("../data/raw/WDI_", code, "_2000_2024.csv"))
  cat("   ✓ Guardado WDI_", code, "_2000_2024.csv\n")
  
  lista_paises[[name]] <- df
}

cat("\n=== DESCARGA COMPLETA ===\n")

# --- 4. GUARDAR BASE COMPARATIVA UNIFICADA ----------------------------------

comparativo_raw <- bind_rows(lista_paises)

write_csv(comparativo_raw, "../data/raw/comparativo_raw.csv")

cat("📁 Archivo unificado guardado en /data/raw/comparativo_raw.csv\n")
cat("⚠️ Nota: Argentina NO posee REER en WDI; en el Script 04 se reemplazará por ITCRM.\n")
cat("=== FIN SCRIPT 02 ===\n")
