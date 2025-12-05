# ======================================================================
# 03_process_argentina.R  (versión simple y 100% compatible con tus datos)
# ======================================================================

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

rm(list = ls()); gc()
packages <- c("dplyr","tidyr","readxl","zoo","readr")
installed <- packages %in% rownames(installed.packages())
if(any(!installed)) install.packages(packages[!installed])
lapply(packages, library, character.only = TRUE)
options(scipen = 999)

cat("\n=== INICIO: PROCESS ARGENTINA ===\n\n")

# --------------------------
# 1) CARGAR WDI RAW
# --------------------------
cat("Leyendo WDI Argentina y Mundo...\n")

wdi_arg   <- read_csv("../data/raw/wdi_argentina_raw.csv", show_col_types = FALSE)
wdi_world <- read_csv("../data/raw/wdi_world_raw.csv", show_col_types = FALSE)

cat("Columnas wdi_arg: "); print(names(wdi_arg))
cat("\nColumnas wdi_world: "); print(names(wdi_world)); cat("\n")

# wdi_arg YA TIENE:
# year, X, M, Y  → NO RENOMBRAMOS NADA


# --------------------------
# 2) PROCESAR ITCRM BCRA
# --------------------------
cat("Leyendo ITCRM desde Excel...\n")

itcrm_raw <- read_excel("../data/external/ITCRMSerie.xlsx", skip = 1)

# Detectamos columnas correctas
col_fecha <- names(itcrm_raw)[1]
col_valor <- names(itcrm_raw)[2]

cat("Usando columnas: Fecha =", col_fecha, " | ITCRM =", col_valor, "\n")

# Convertir fecha
itcrm_raw <- itcrm_raw %>%
  mutate(
    Fecha = as.Date(!!sym(col_fecha)),
    year  = as.numeric(format(Fecha, "%Y"))
  ) %>%
  rename(ITCRM = !!sym(col_valor)) %>%
  filter(year >= 2000 & year <= 2024)

# Anualizar (promedio)
itcrm_yearly <- itcrm_raw %>%
  group_by(year) %>%
  summarise(ITCRM = mean(ITCRM, na.rm = TRUE), .groups = "drop")

cat("ITCRM anualizado:\n")
print(itcrm_yearly %>% head())


# --------------------------
# 3) PREPARAR PIB MUNDIAL
# --------------------------
cat("Procesando PIB Mundial...\n")

# En tu CSV, la columna del PIB ya se llama Y_world
col_year_world <- "year"
col_gdp_world  <- "Y_world"

cat("Columna PIB mundial detectada:", col_gdp_world, "\n")

# Seleccionamos solo year y Y_world
wdi_world <- wdi_world %>%
  rename(Y_world = !!sym(col_gdp_world)) %>%
  select(year, Y_world)

# Revisar primeras filas
cat("Primeras filas PIB mundial:\n")
print(head(wdi_world))

# --------------------------
# 4) UNIR TODO
# --------------------------
cat("Uniendo datasets...\n")

df_arg <- wdi_arg %>%
  left_join(itcrm_yearly, by = "year") %>%
  left_join(wdi_world, by = "year") %>%
  arrange(year)

cat("Columnas resultantes:\n")
print(names(df_arg))


# --------------------------
# 5) TRANSFORMACIONES
# --------------------------
cat("Calculando logs y diferencias...\n")

df_arg <- df_arg %>%
  mutate(
    lnX       = log(X),
    lnM       = log(M),
    lnY       = log(Y),
    lnR       = log(ITCRM),        # <<-- PARA ARGENTINA: R = ITCRM (REAL MÁS ALTO = DEPRECIADO)
    lnYworld  = log(Y_world),
    
    d_lnX = c(NA, diff(lnX)),
    d_lnM = c(NA, diff(lnM)),
    d_lnY = c(NA, diff(lnY)),
    d_lnR = c(NA, diff(lnR))
  ) %>%
  mutate(dummy_covid = ifelse(year %in% c(2020, 2021), 1, 0))


# --------------------------
# 6) GUARDAR
# --------------------------
dir.create("../data/processed", showWarnings = FALSE, recursive = TRUE)

write_csv(df_arg, "../data/processed/argentina_processed.csv")

cat("\n============================================\n")
cat("✔ Guardado: ../data/processed/argentina_processed.csv\n")
cat("Filas:", nrow(df_arg), " | Columnas:", ncol(df_arg), "\n")
cat("============================================\n\n")
