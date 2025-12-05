# 04_process_comparativo.R (versión extendida)
# Procesamiento final de los datos WDI por país para análisis comparativo
# - Normalizamos R: REER cuando esté disponible; si no (Argentina) usamos ITCRM
# - Interpolamos R cuando tenga huecos
# - Calculamos lnX, lnM, lnY, lnR
# - Guardamos archivos por país y dataset comparativo final
# - Estimamos elasticidades y tasa de Thirlwall
# - Guardamos modelos y resultados para tests y gráficos posteriores

# --- 0. Directorio de trabajo --------------------------------------------
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# --- 1. Librerías y limpieza inicial -------------------------------------
rm(list = ls()); gc()
packages <- c("dplyr","tidyr","readr","zoo","writexl","purrr","WDI","broom")
installed <- packages %in% rownames(installed.packages())
if(any(!installed)) install.packages(packages[!installed])
lapply(packages, library, character.only = TRUE)
options(scipen = 999)

cat("\n=== INICIAMOS SCRIPT 04: PROCESS COMPARATIVO INTERNACIONAL ===\n\n")

# --- 2. Lectura de ITCRM anualizado --------------------------------------
itcrm_path_processed <- "../data/processed/argentina_processed.csv"
itcrm_yearly <- NULL

if(file.exists(itcrm_path_processed)){
  cat("Leyendo argentina_processed.csv para extraer ITCRM anual.\n")
  arg_proc <- read_csv(itcrm_path_processed, show_col_types = FALSE)
  if("ITCRM" %in% names(arg_proc)){
    itcrm_yearly <- arg_proc %>% select(year, ITCRM) %>% distinct()
  }
}

# --- 3. Localizamos los raw WDI por país ----------------------------------
raw_files <- list.files("../data/raw", pattern = "WDI_.*_2000_2024.csv", full.names = TRUE)
if(length(raw_files) == 0) stop("No se encontraron archivos raw tipo WDI_..._2000_2024.csv en ../data/raw")

cat("Archivos raw detectados:\n")
print(raw_files)

# --- 4. Función para procesar cada raw -----------------------------------
procesar_raw <- function(path_raw, itcrm_yearly = NULL, save_processed = TRUE){
  cat("\nProcesando archivo:", path_raw, "\n")
  
  df_raw <- read_csv(path_raw, show_col_types = FALSE)
  
  # Renombramos columnas si es necesario
  if(all(c("NE.EXP.GNFS.KD","NE.IMP.GNFS.KD","NY.GDP.MKTP.KD") %in% names(df_raw))){
    df <- df_raw %>%
      rename(X = NE.EXP.GNFS.KD,
             M = NE.IMP.GNFS.KD,
             Y = NY.GDP.MKTP.KD) %>%
      mutate(R = ifelse("PX.REX.REER" %in% names(df_raw), df_raw$PX.REX.REER, NA_real_))
  } else if(all(c("X","M","Y") %in% names(df_raw))){
    df <- df_raw
    if(!("R" %in% names(df)) && "PX.REX.REER" %in% names(df_raw)){
      df <- df %>% mutate(R = df_raw$PX.REX.REER)
    }
  } else {
    df <- df_raw
    if("NE.EXP.GNFS.KD" %in% names(df_raw)) df <- df %>% rename(X = NE.EXP.GNFS.KD)
    if("NE.IMP.GNFS.KD" %in% names(df_raw)) df <- df %>% rename(M = NE.IMP.GNFS.KD)
    if("NY.GDP.MKTP.KD" %in% names(df_raw)) df <- df %>% rename(Y = NY.GDP.MKTP.KD)
    if(!("R" %in% names(df)) && "PX.REX.REER" %in% names(df_raw)) df <- df %>% mutate(R = df_raw$PX.REX.REER)
  }
  
  iso2 <- if("iso2c" %in% names(df)) df$iso2c[1] else NA_character_
  pais_nombre <- if("country" %in% names(df)) df$country[1] else NA_character_
  
  # Argentina: reemplazamos R por ITCRM si R es NA
  if(!is.na(iso2) && iso2=="AR" && all(is.na(df$R)) && !is.null(itcrm_yearly)){
    cat("  -> Argentina: reemplazo R por ITCRM anual.\n")
    df <- df %>%
      left_join(itcrm_yearly, by="year") %>%
      mutate(R = (ITCRM/mean(ITCRM, na.rm=TRUE))*100) %>%
      select(-ITCRM)
  }
  
  # Interpolación spline
  if("R" %in% names(df) && sum(!is.na(df$R))>=3){
    df$R <- zoo::na.spline(df$R)
  }
  
  # Valores no positivos a NA
  df <- df %>% mutate(across(c(X,M,Y,R), ~ifelse(.<=0, NA, .)))
  
  # Logs
  df <- df %>% arrange(year) %>%
    mutate(
      lnX = ifelse(!is.na(X), log(X), NA_real_),
      lnM = ifelse(!is.na(M), log(M), NA_real_),
      lnY = ifelse(!is.na(Y), log(Y), NA_real_),
      lnR = ifelse(!is.na(R), -log(R), NA_real_)
    )
  
  # Eliminamos filas incompletas
  df_complete <- df %>% filter(complete.cases(lnX,lnM,lnY,lnR)) %>%
    mutate(d_lnX = c(NA,diff(lnX)),
           d_lnM = c(NA,diff(lnM)),
           d_lnY = c(NA,diff(lnY)),
           d_lnR = c(NA,diff(lnR)),
           iso2c = iso2,
           country = pais_nombre)
  
  # Guardado procesado
  if(save_processed){
    out_path <- paste0("../data/processed/WDI_", iso2 %||% tools::file_path_sans_ext(basename(path_raw)), "_processed.csv")
    readr::write_csv(df_complete, out_path)
    cat("  -> Guardado procesado:", out_path, "\n")
  }
  
  return(df_complete)
}

# --- 5. Procesamos todos los archivos -------------------------------------
lista_proc <- lapply(raw_files, procesar_raw, itcrm_yearly=itcrm_yearly)

names(lista_proc) <- gsub("../data/raw/WDI_|_2000_2024.csv","",raw_files)

# --- 6. Guardamos dataframe comparativo final -----------------------------
df_comparativo <- bind_rows(lista_proc, .id="pais_code") %>%
  mutate(pais = ifelse(is.na(country), pais_code, country))

out_comparativo <- "../data/processed/comparativo_internacional.csv"
readr::write_csv(df_comparativo, out_comparativo)
cat("\n✔ Dataset comparativo guardado en:", out_comparativo, "\n")

# --- 7. Estimación de modelos por país ------------------------------------
modelos_list <- list()
elasticidades_list <- list()

analizar_df <- function(df_in, code_name){
  if(nrow(df_in)<10){
    warning("Datos insuficientes para ", code_name)
    return(tibble(Pais=code_name,Codigo=code_name,n_obs=nrow(df_in),
                  delta=NA,eta=NA,g_thirlwall=NA,R2_X=NA,R2_M=NA))
  }
  
  mX <- tryCatch(lm(lnX ~ lnY + lnR, data=df_in), error=function(e) NULL)
  mM <- tryCatch(lm(lnM ~ lnY + lnR, data=df_in), error=function(e) NULL)
  
  if(is.null(mX) || is.null(mM)) return(tibble(Pais=code_name,Codigo=code_name,n_obs=nrow(df_in),
                                               delta=NA,eta=NA,g_thirlwall=NA,R2_X=NA,R2_M=NA))
  
  # Guardamos modelos
  modelos_list[[code_name]] <<- list(mX=mX,mM=mM)
  
  delta <- coef(mX)["lnY"]
  eta   <- coef(mM)["lnY"]
  R2_X  <- summary(mX)$r.squared
  R2_M  <- summary(mM)$r.squared
  
  g_world <- mean(diff(log(df_in$Y)), na.rm=TRUE) # aproximación local
  g_T <- if(!is.na(delta) && !is.na(eta) && eta!=0) abs(g_world*(delta/eta)) else NA_real_
  
  elasticidades_list[[code_name]] <<- tibble(Pais=unique(df_in$country)[1] %||% code_name,
                                             delta=delta, eta=eta, g_thirlwall=g_T)
  
  tibble(Pais=unique(df_in$country)[1] %||% code_name,
         Codigo=code_name,
         n_obs=nrow(df_in),
         delta=round(delta,3),
         eta=round(eta,3),
         g_thirlwall=round(g_T,4),
         R2_X=round(R2_X,3),
         R2_M=round(R2_M,3))
}

`%||%` <- function(x,y) if(is.null(x) || length(x)==0) y else x

resultados_list <- map2_dfr(lista_proc, names(lista_proc), analizar_df)

# --- 8. Guardamos modelos y elasticidades para scripts posteriores ---------
dir.create("../data/models", showWarnings = FALSE, recursive = TRUE)
saveRDS(modelos_list, "../data/models/modelos_WDI_comparativo.rds")
saveRDS(elasticidades_list, "../data/models/elasticidades_WDI_comparativo.rds")

# --- 9. Guardamos resultados finales --------------------------------------
dir.create("../outputs", showWarnings = FALSE, recursive = TRUE)
out_results_xlsx <- "../outputs/Resultados_Comparativa_Thirlwall_REER_ITCRM.xlsx"
writexl::write_xlsx(resultados_list, out_results_xlsx)
readr::write_csv(resultados_list, "../outputs/Resultados_Comparativa_Thirlwall_ITCRM_REER.csv")

cat("\n✔ Resultados guardados en outputs y modelos en data/models\n")
cat("=== SCRIPT 04 COMPLETADO ===\n")
