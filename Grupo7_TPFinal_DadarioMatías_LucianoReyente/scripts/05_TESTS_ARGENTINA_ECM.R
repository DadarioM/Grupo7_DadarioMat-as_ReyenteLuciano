# SCRIPT 05_TESTS_ARGENTINA_ECM.R
# Validaciones, tests y estimación de ECM para Argentina
# Guardado de resultados listos para análisis posterior y gráficos

# 1. LIMPIEZA Y CARGA =========================================================
rm(list = ls()); gc()

packages <- c("dplyr","tidyr","ggplot2","readxl","writexl",
              "urca","lmtest","sandwich","GGally","zoo","stargazer")
installed_packages <- packages %in% rownames(installed.packages())
if(any(!installed_packages)) install.packages(packages[!installed_packages])
lapply(packages, library, character.only = TRUE)
options(scipen = 999)

# Crear carpeta de outputs si no existe
dir.create("../outputs", showWarnings = FALSE, recursive = TRUE)

# 2. CARGAR DATA PROCESADA ====================================================
ruta_data <- "../data/processed/argentina_processed.csv"
df <- read.csv(ruta_data)
cat("\n✅ Dataset cargado correctamente. Primeras filas:\n")
print(head(df))

# 3. CREAR LOGARITMOS Y DIFERENCIAS ==========================================
df <- df %>%
  arrange(year) %>%
  mutate(
    lnYw = log(Y_world),
    lnR  = log(ITCRM),
    dlnX = lnX - lag(lnX),
    dlnM = lnM - lag(lnM),
    dlnY = lnY - lag(lnY),
    dlnYw = lnYw - lag(lnYw),
    dlnR = lnR - lag(lnR),
    D_covid = ifelse(year %in% c(2020,2021),1,0)
  )

# 4. TEST DE RAÍZ UNITARIA ADF =================================================
cat("\n=== Test de raíz unitaria (ADF) ===\n")
adf_test_series <- function(x, nombre, tipo="drift", lags=1) {
  x <- na.omit(x)
  if(length(x)<10) {
    cat(nombre, ": insuficiente número de observaciones para ADF\n")
    return(NULL)
  }
  test <- ur.df(x, type=tipo, lags=lags)
  stat <- test@teststat[1]
  crit <- test@cval[1,"5pct"]
  cat(sprintf("%s: τ = %.3f | crítico 5%% = %.3f --> %s\n",
              nombre, stat, crit,
              ifelse(stat < crit, "ESTACIONARIA", "NO estacionaria")))
  return(test)
}

adf_lnX  <- adf_test_series(df$lnX,  "lnX")
adf_lnM  <- adf_test_series(df$lnM,  "lnM")
adf_lnY  <- adf_test_series(df$lnY,  "lnY")
adf_lnYw <- adf_test_series(df$lnYw, "lnYw")
adf_lnR  <- adf_test_series(df$lnR,  "lnR")

# 5. MODELOS DE LARGO PLAZO ===================================================
cat("\n=== Estimación de modelos de largo plazo ===\n")
modelo_X_lvl <- lm(lnX ~ lnYw + lnR, data=df)
modelo_M_lvl <- lm(lnM ~ lnY + lnR, data=df)

cat("\n--- Exportaciones (lnX ~ lnYw + lnR) ---\n")
print(summary(modelo_X_lvl))
cat("\n--- Importaciones (lnM ~ lnY + lnR) ---\n")
print(summary(modelo_M_lvl))

# 6. RESIDUOS Y LAG PARA ECM ===================================================
df <- df %>%
  mutate(
    ecm_X_resid = resid(modelo_X_lvl),
    ecm_M_resid = resid(modelo_M_lvl),
    lag_ecm_X = lag(ecm_X_resid),
    lag_ecm_M = lag(ecm_M_resid)
  )

# 7. ESTIMACIÓN DE ECM =========================================================
cat("\n=== Estimación de modelos ECM de corto plazo ===\n")
ecm_X <- lm(dlnX ~ dlnYw + dlnR + lag_ecm_X + D_covid, data=df)
ecm_M <- lm(dlnM ~ dlnY + dlnR + lag_ecm_M + D_covid, data=df)

cat("\n--- ECM Exportaciones ---\n")
print(summary(ecm_X))
cat("\n--- ECM Importaciones ---\n")
print(summary(ecm_M))

# 8. DIAGNÓSTICOS =============================================================
cat("\n=== Test de autocorrelación y heterocedasticidad ===\n")
cat("\nBreusch-Godfrey:\n"); print(bgtest(ecm_X)); print(bgtest(ecm_M))
cat("\nBreusch-Pagan:\n"); print(bptest(ecm_X)); print(bptest(ecm_M))
cat("\nErrores robustos (White/HC1):\n")
print(coeftest(ecm_X, vcov=vcovHC(ecm_X, type="HC1")))
print(coeftest(ecm_M, vcov=vcovHC(ecm_M, type="HC1")))

# 9. CALCULAR ELASTICIDADES Y TASA THIRLWALL ================================
delta <- coef(modelo_X_lvl)["lnYw"]
eta   <- coef(modelo_M_lvl)["lnY"]
g_world <- mean(diff(log(na.omit(df$Y_world))), na.rm = TRUE)
g_thirlwall <- g_world * (delta / eta)

resumen_elas <- tibble(
  Variable = c("Exportaciones","Importaciones","Crecimiento Mundial","Tasa Thirlwall"),
  Valor    = c(delta, eta, g_world, g_thirlwall)
)

cat(sprintf("\nElasticidad ingreso exportaciones (δ): %.3f", delta))
cat(sprintf("\nElasticidad ingreso importaciones (η): %.3f", eta))
cat(sprintf("\nCrecimiento mundial promedio (g*): %.3f", g_world))
cat(sprintf("\nTasa de Thirlwall (g_T): %.3f\n", g_thirlwall))

# 10. GUARDAR RESULTADOS ======================================================
# Dataset con ECM listo
write.csv(df, "../outputs/argentina_ECM_ready.csv", row.names=FALSE)
cat("\n✅ Dataset con ECM listo guardado en 'argentina_ECM_ready.csv'\n")

# Guardar modelos para uso posterior
saveRDS(modelo_X_lvl, "../outputs/modelo_X_lvl.rds")
saveRDS(modelo_M_lvl, "../outputs/modelo_M_lvl.rds")
saveRDS(ecm_X, "../outputs/ecm_X.rds")
saveRDS(ecm_M, "../outputs/ecm_M.rds")
cat("✅ Modelos guardados como RDS en /outputs\n")

# Guardar tabla de elasticidades para gráficos posteriores
write_xlsx(resumen_elas, "../outputs/elasticidades_argentina.xlsx")
cat("✅ Tabla de elasticidades guardada en /outputs/elasticidades_argentina.xlsx\n")

# 11. REPORTAR MODELOS EN CONSOLA ============================================
stargazer::stargazer(modelo_X_lvl, modelo_M_lvl, ecm_X, ecm_M,
                     type="text",
                     title="Modelos de Largo y Corto Plazo - Restricción Externa y Elasticidades",
                     digits=3)
# 12. CREAR DATASET “TIDY” PARA GRAFICOS ECM ==================================
df_ecm_tidy <- df %>%
  select(year, lnX, lnM, lnY, lnYw, lnR,
         dlnX, dlnM, dlnY, dlnYw, dlnR,
         ecm_X_resid, ecm_M_resid,
         lag_ecm_X, lag_ecm_M,
         D_covid)

# Guardamos como CSV para uso posterior en gráficos
write.csv(df_ecm_tidy, "../outputs/argentina_ECM_tidy.csv", row.names = FALSE)
cat("\n✅ Dataset 'tidy' para gráficos de ECM guardado en '../outputs/argentina_ECM_tidy.csv'\n")

