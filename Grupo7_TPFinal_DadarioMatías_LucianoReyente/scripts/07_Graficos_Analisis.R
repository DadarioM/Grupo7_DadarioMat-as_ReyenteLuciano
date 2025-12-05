# ==============================================
# 07_Graficos_Analisis.R
# Genera 12 gráficos profesionales para Argentina y comparación internacional
# ==============================================

rm(list = ls())
gc()

# --- 0. Librerías necesarias ---
library(ggplot2)
library(ggrepel)
library(patchwork)
library(dplyr)
library(readr)
library(viridis)
library(writexl)
library(zoo)

# --- 1. Cargar datasets ---
df_arg  <- read_csv("../outputs/argentina_ECM_ready.csv", show_col_types = FALSE)
df_comp <- read_csv("../outputs/Resultados_Comparativa_Thirlwall_Processed.csv", show_col_types = FALSE)

# --- 2. Preparar columna R original para gráfico ---
# R = REER / ITCRM, lnR = -log(R)
df_arg <- df_arg %>% mutate(R_orig = exp(-lnR))

# --- 3. Theme profesional estilo FT / Economist ---
theme_ft <- function(base_size=14, base_family="Arial"){
  theme_minimal(base_size=base_size, base_family=base_family) %+replace%
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill="white", color=NA),
      axis.line = element_line(color="black", linewidth=0.7),
      axis.ticks = element_line(color="black"),
      axis.text = element_text(color="black", size=base_size-2),
      axis.title = element_text(face="bold", color="black", size=base_size),
      plot.title = element_text(face="bold", size=base_size+2, hjust=0),
      plot.subtitle = element_text(size=base_size, hjust=0),
      plot.caption = element_text(size=base_size-2, color="grey50"),
      legend.position = "bottom",
      legend.background = element_blank(),
      legend.key = element_blank(),
      legend.text = element_text(size=base_size-2),
      legend.title = element_text(face="bold", size=base_size-1)
    )
}

# --- 4. Paleta de colores sobria y consistente ---
pal_ft <- c("#1F77B4","#D62728","#2CA02C","#9467BD","#FF7F0E",
            "#8C564B","#E377C2","#7F7F7F","#BCBD22","#17BECF")

# --- 5. Crear carpeta para figuras ---
dir.create("../outputs/figures", showWarnings = FALSE, recursive = TRUE)

# --- 6. Gráficos individuales ---

# 1. Exportaciones lnX
g1 <- ggplot(df_arg, aes(x=year, y=lnX)) +
  geom_line(color=pal_ft[1], linewidth=1.2) + geom_point(color=pal_ft[1], size=2) +
  labs(title="Exportaciones de Argentina (lnX)", x="Año", y="lnX") + theme_ft()

# 2. Importaciones lnM
g2 <- ggplot(df_arg, aes(x=year, y=lnM)) +
  geom_line(color=pal_ft[2], linewidth=1.2) + geom_point(color=pal_ft[2], size=2) +
  labs(title="Importaciones de Argentina (lnM)", x="Año", y="lnM") + theme_ft()

# 3. Producto Interno lnY
g3 <- ggplot(df_arg, aes(x=year, y=lnY)) +
  geom_line(color=pal_ft[3], linewidth=1.2) + geom_point(color=pal_ft[3], size=2) +
  labs(title="Producto Interno Bruto Argentina (lnY)", x="Año", y="lnY") + theme_ft()

# 4. REER / ITCRM (original)
g4 <- ggplot(df_arg, aes(x=year, y=R_orig)) +
  geom_line(color=pal_ft[4], linewidth=1.2) +
  labs(title="REER / ITCRM Argentina", x="Año", y="R") + theme_ft()

# 5. Diferenciales lnX (crecimiento)
g5 <- ggplot(df_arg, aes(x=year, y=dlnX)) +
  geom_bar(stat="identity", fill=pal_ft[1]) +
  labs(title="Crecimiento exportaciones Argentina", x="Año", y="Δ lnX") + theme_ft()

# 6. Diferenciales lnM (crecimiento)
g6 <- ggplot(df_arg, aes(x=year, y=dlnM)) +
  geom_bar(stat="identity", fill=pal_ft[2]) +
  labs(title="Crecimiento importaciones Argentina", x="Año", y="Δ lnM") + theme_ft()

# 7. Residuos ECM X
g7 <- ggplot(df_arg, aes(x=year, y=ecm_X_resid)) +
  geom_line(color=pal_ft[1], linewidth=1.2) +
  geom_hline(yintercept=0, linetype="dashed", color="grey50") +
  labs(title="Residuos ECM Exportaciones", x="Año", y="Residual") + theme_ft()

# 8. Residuos ECM M
g8 <- ggplot(df_arg, aes(x=year, y=ecm_M_resid)) +
  geom_line(color=pal_ft[2], linewidth=1.2) +
  geom_hline(yintercept=0, linetype="dashed", color="grey50") +
  labs(title="Residuos ECM Importaciones", x="Año", y="Residual") + theme_ft()

# 9. Elasticidades delta vs eta por país
g9 <- ggplot(df_comp, aes(x=delta, y=eta, label=Pais, color=Pais)) +
  geom_point(size=3) +
  geom_text_repel(size=4) +
  scale_color_manual(values=pal_ft) +
  labs(title="Elasticidades: Exportaciones vs Importaciones",
       x="δ (lnX ~ lnY*)", y="η (lnM ~ lnY)") +
  theme_ft()


# --- 7. Guardar todos los gráficos ---
plots <- list(g1,g2,g3,g4,g5,g6,g7,g8,g9)

for(i in seq_along(plots)){
  ggsave(filename=paste0("../outputs/figures/grafico_",i,".png"),
         plot=plots[[i]], width=10, height=6, dpi=300)
}

cat("\n✅ Todos los gráficos guardados en ../outputs/figures\n")
