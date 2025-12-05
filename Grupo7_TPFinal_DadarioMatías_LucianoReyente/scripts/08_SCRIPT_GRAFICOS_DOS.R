# ===========================================
# SCRIPT GRAFICOS 2
# ===========================================
rm(list=ls()); gc()

# --- Librerías ---
library(ggplot2)
library(ggrepel)
library(patchwork)
library(dplyr)
library(readr)
library(tidyr)

# --- Cargar datasets ---
df_arg <- read_csv("../outputs/argentina_ECM_ready.csv", show_col_types = FALSE)
df_comp <- read_csv("../outputs/Resultados_Comparativa_Thirlwall_Processed.csv", show_col_types = FALSE)
df_all <- read_csv("../data/processed/comparativo_internacional.csv", show_col_types = FALSE)

# --- Theme profesional ---
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

# --- Paleta de colores profesional ---
pal_trade <- c("#2C3E50","#16A085") # azul grisáceo y verde grisáceo
pal_elastic <- c("#34495E","#27AE60") # tonos sobrios para elasticidades

# --- Crear carpeta figuras ---
dir.create("../outputs/figures", showWarnings=FALSE, recursive=TRUE)

# ===========================================
# GRAFICO 1: Exportaciones e Importaciones
# ===========================================
g_trade <- ggplot(df_arg, aes(x=year)) +
  geom_line(aes(y=lnX, color="Exportaciones"), linewidth=1.5) +
  geom_line(aes(y=lnM, color="Importaciones"), linewidth=1.5) +
  scale_color_manual(values=c("Exportaciones"=pal_trade[1],"Importaciones"=pal_trade[2])) +
  labs(title="Argentina: Exportaciones e Importaciones (2000-2024)",
       x="Año", y="ln Valores", color="Serie") +
  theme_ft()

# ===========================================
# GRAFICO 2: ITCRM Argentina
# ===========================================
g_ITCRM <- ggplot(df_arg, aes(x=year, y=ITCRM)) +
  geom_line(color=pal_trade[1], linewidth=1.5) +
  labs(title="ITCRM Argentina", x="Año", y="ITCRM") +
  theme_ft()

# ===========================================
# GRAFICO 3: REER comparativo (rearmado con patchwork)
# ===========================================
df_reer_all <- df_all %>% filter(!is.na(R) & iso2c != "AR")

# Crear lista de gráficos por país
lista_paises <- unique(df_reer_all$country)
graficos_reer <- lapply(lista_paises, function(p){
  df_sub <- df_reer_all %>% filter(country==p)
  ggplot(df_sub, aes(x=year, y=R)) +
    geom_line(color=pal_trade[2], linewidth=1.2) +
    labs(title=p, x="Año", y="REER") +
    theme_ft() +
    theme(legend.position="none")
})

# Combinar con patchwork
g_reer_patch <- wrap_plots(graficos_reer, ncol=2)

# ===========================================
# GRAFICO 4: Elasticidades X e M
# ===========================================
df_comp_long <- df_comp %>%
  select(Pais, delta, eta) %>%
  pivot_longer(cols=c(delta, eta), names_to="Tipo", values_to="Elasticidad") %>%
  mutate(Tipo = recode(Tipo, delta="Exportaciones", eta="Importaciones"))

g_elasticidades <- ggplot(df_comp_long, aes(x=reorder(Pais, Elasticidad), y=Elasticidad, fill=Tipo)) +
  geom_bar(stat="identity", position=position_dodge(width=0.8)) +
  coord_flip() +
  scale_fill_manual(values=c("Exportaciones"=pal_elastic[1], "Importaciones"=pal_elastic[2])) +
  labs(title="Elasticidades de Exportaciones e Importaciones por país",
       x="País", y="Elasticidad", fill="Tipo") +
  theme_ft()

# ===========================================
# GRAFICO 6: dejar como estaba (ejemplo)
# ===========================================
g_graf6 <- df_arg %>%
  filter(!is.na(dlnX)) %>%   # eliminar filas con NA en dlnX
  ggplot(aes(x=year, y=dlnX)) +
  geom_line(color=pal_trade[1], linewidth=1.5) +
  labs(title="Crecimiento de ΔlnX Argentina", x="Año", y="ΔlnX") +
  theme_ft()

# ===========================================
# Normalizar Exportaciones e Importaciones
# ===========================================
# Tomamos los residuos para definir la escala
resid_min <- min(c(df_arg$ecm_X_resid, df_arg$ecm_M_resid), na.rm=TRUE)
resid_max <- max(c(df_arg$ecm_X_resid, df_arg$ecm_M_resid), na.rm=TRUE)

# Normalización min-max de lnX y lnM para que queden en escala comparable con los residuos
df_arg <- df_arg %>%
  mutate(
    lnX_scaled = (lnX - min(lnX, na.rm=TRUE)) / (max(lnX, na.rm=TRUE) - min(lnX, na.rm=TRUE)) * (resid_max - resid_min) + resid_min,
    lnM_scaled = (lnM - min(lnM, na.rm=TRUE)) / (max(lnM, na.rm=TRUE) - min(lnM, na.rm=TRUE)) * (resid_max - resid_min) + resid_min
  )

# ===========================================
# Paleta profesional para Exportaciones e Importaciones
# ===========================================
pal_trade <- c("Exportaciones"="#1B4F72",        # azul oscuro
               "Importaciones"="#6E2C00",       # marrón oscuro sobrio
               "Residuo Exportaciones"="#1B4F72",
               "Residuo Importaciones"="#6E2C00")

# ===========================================
# GRAFICO 7: Exportaciones, Importaciones y sus Residuos ECM (normalizado)
# ===========================================
g_trade_resid <- ggplot(df_arg, aes(x=year)) +
  # Líneas de series originales (normalizadas)
  geom_line(aes(y=lnX_scaled, color="Exportaciones"), linewidth=1.8) +
  geom_line(aes(y=lnM_scaled, color="Importaciones"), linewidth=1.8) +
  # Líneas de residuos ECM (punteadas)
  geom_line(aes(y=ecm_X_resid, color="Residuo Exportaciones"), linewidth=1.2, linetype="dashed") +
  geom_line(aes(y=ecm_M_resid, color="Residuo Importaciones"), linewidth=1.2, linetype="dashed") +
  scale_color_manual(values=pal_trade) +
  labs(title="Argentina: Exportaciones, Importaciones y Residuos ECM (2000-2024)",
       x="Año", y="Valores normalizados / Residual", color="") +
  theme_ft() +
  theme(legend.position="bottom", legend.box="vertical")

# ===========================================
# GUARDAR GRAFICOS
# ===========================================
ggsave("../outputs/figures/grafico_1_trade.png", g_trade, width=10, height=6, dpi=300)
ggsave("../outputs/figures/grafico_2_ITCRM.png", g_ITCRM, width=10, height=6, dpi=300)
ggsave("../outputs/figures/grafico_3_REER.png", g_reer_patch, width=12, height=10, dpi=300)
ggsave("../outputs/figures/grafico_4_elasticidades.png", g_elasticidades, width=10, height=6, dpi=300)
ggsave("../outputs/figures/grafico_6.png", g_graf6, width=10, height=6, dpi=300)
ggsave("../outputs/figures/grafico_7_trade_resid.png",
       plot=g_trade_resid,
       width=10, height=6, dpi=300)
