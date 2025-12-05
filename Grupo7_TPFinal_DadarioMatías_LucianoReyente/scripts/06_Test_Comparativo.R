# 06_Process_Comparativo_Processed.R
# ANALISIS COMPARATIVO INTERNACIONAL USANDO DATOS PROCESADOS (2000–2024)
# Compatible con outputs de Script 04 (comparativo_internacional.csv)

rm(list = ls()); gc()
packages <- c("dplyr","tidyr","ggplot2","zoo","broom","purrr","writexl","readr")
installed_packages <- packages %in% rownames(installed.packages())
if(any(!installed_packages)) install.packages(packages[!installed_packages])
lapply(packages, library, character.only = TRUE)
options(scipen = 999)

cat("\n=== INICIO SCRIPT 06: ANALISIS COMPARATIVO INTERNACIONAL ===\n")

# --- 1. CARGAR DATOS PROCESADOS --------------------------------------
ruta_data <- "../data/processed/comparativo_internacional.csv"
if(!file.exists(ruta_data)) stop("No se encontró comparativo_internacional.csv")
df_all <- read_csv(ruta_data, show_col_types = FALSE)

cat("Datos cargados. Países disponibles:\n")
print(unique(df_all$country))

# --- 2. CREAR PIB MUNDIAL Y LOGARITMOS --------------------------------
# Si no existe columna Y_world, creamos sumando Y de todos los países por año
if(!"Y_world" %in% names(df_all)){
  cat("Generando PIB mundial (Y_world) sumando Y de todos los países...\n")
  Y_world <- df_all %>%
    group_by(year) %>%
    summarise(Y_world = sum(Y, na.rm=TRUE))
  df_all <- df_all %>% left_join(Y_world, by="year")
}

# Log del PIB mundial
df_all <- df_all %>%
  mutate(
    lnYworld = log(Y_world),
    lnR = -log(R)
  )

# --- 3. LISTA DE PAÍSES ----------------------------------------------
paises_code <- unique(df_all$iso2c)
paises_name <- unique(df_all$country)

# --- 4. FUNCION ANALIZAR CADA PAÍS ----------------------------------
analizar_pais <- function(df, code_name){
  df_pais <- df %>% filter(iso2c == code_name) %>% arrange(year)
  
  if(nrow(df_pais)<10){
    warning("Datos insuficientes para ", code_name)
    return(list(
      resultados = tibble(Pais=code_name, Codigo=code_name, n_obs=nrow(df_pais),
                          delta=NA, eta=NA, g_thirlwall=NA, R2_X=NA, R2_M=NA),
      modelo_X=NULL,
      modelo_M=NULL
    ))
  }
  
  modelo_X <- lm(lnX ~ lnYworld + lnR, data=df_pais)
  modelo_M <- lm(lnM ~ lnY + lnR, data=df_pais)
  
  delta <- coef(modelo_X)["lnYworld"]
  eta   <- coef(modelo_M)["lnY"]
  R2_X  <- summary(modelo_X)$r.squared
  R2_M  <- summary(modelo_M)$r.squared
  
  g_world <- mean(diff(log(na.omit(df_pais$Y_world))), na.rm=TRUE)
  g_T <- if(!is.na(delta) & !is.na(eta) & eta != 0) abs(g_world*(delta/eta)) else NA
  
  resultados <- tibble(
    Pais=unique(df_pais$country)[1],
    Codigo=code_name,
    n_obs=nrow(df_pais),
    delta=round(delta,3),
    eta=round(eta,3),
    g_thirlwall=round(g_T,4),
    R2_X=round(R2_X,3),
    R2_M=round(R2_M,3)
  )
  
  return(list(resultados=resultados, modelo_X=modelo_X, modelo_M=modelo_M))
}

# --- 5. EJECUCIÓN PARA TODOS LOS PAÍSES -----------------------------
lista_resultados <- purrr::map(paises_code, ~analizar_pais(df_all, .x))

resultados_df <- purrr::map_dfr(lista_resultados, "resultados")
modelos_X <- purrr::map(lista_resultados, "modelo_X")
modelos_M <- purrr::map(lista_resultados, "modelo_M")

cat("\n✅ Resultados resumidos:\n")
print(resultados_df)

# --- 6. GUARDAR RESULTADOS Y MODELOS ---------------------------------
dir.create("../outputs", showWarnings=FALSE, recursive=TRUE)
write_csv(resultados_df, "../outputs/Resultados_Comparativa_Thirlwall_Processed.csv")
writexl::write_xlsx(resultados_df, "../outputs/Resultados_Comparativa_Thirlwall_Processed.xlsx")

# Guardar modelos por país
dir.create("../data/models", showWarnings=FALSE, recursive=TRUE)
purrr::walk2(modelos_X, paises_code, ~if(!is.null(.x)) saveRDS(.x, paste0("../data/models/modelo_X_", .y, ".rds")))
purrr::walk2(modelos_M, paises_code, ~if(!is.null(.x)) saveRDS(.x, paste0("../data/models/modelo_M_", .y, ".rds")))

cat("\n✅ Script 06 completado. Resultados y modelos guardados.\n")
