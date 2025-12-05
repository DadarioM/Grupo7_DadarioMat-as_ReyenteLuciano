# ===========================================
# GRAFICOS REER INTERNACIONALES + Argentina ITCRM
# ===========================================

rm(list=ls()); gc()

# --- Librerías ---
library(ggplot2)
library(dplyr)
library(patchwork)
library(readr)

# --- Cargar datasets ---
df_all <- read_csv("../data/processed/comparativo_internacional.csv", show_col_types = FALSE)
df_arg <- read_csv("../outputs/argentina_ECM_ready.csv", show_col_types = FALSE)

# --- Filtrar países distintos a Argentina y con REER disponible ---
df_reer_all <- df_all %>% filter(!is.na(R) & iso2c != "AR")

# --- Paleta sobria ---
pal_ft <- c(
  "#2C7BB6", "#D7191C", "#4DAF4A", "#984EA3", "#FF7F00",
  "#A65628", "#F781BF", "#999999", "#E6AB02", "#377EB8"
)

# --- Theme profesional ---
theme_ft <- function(base_size=12, base_family="Arial"){
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
      legend.position = "none"
    )
}

# --- Crear lista de gráficos individuales ---
countries <- unique(df_reer_all$country)
plots_list <- list()

for(i in seq_along(countries)){
  df_tmp <- df_reer_all %>% filter(country == countries[i])
  
  g <- ggplot(df_tmp, aes(x=year, y=R)) +
    geom_line(color=pal_ft[i], linewidth=1.1) +
    labs(title=countries[i], x="Año", y="REER") +
    theme_ft()
  
  plots_list[[i]] <- g
}

# --- Agregar panel de Argentina (ITCRM) ---
g_arg <- ggplot(df_arg, aes(x=year, y=ITCRM)) +
  geom_line(color="#1F78B4", linewidth=1.2) +
  labs(title="Argentina (ITCRM)", x="Año", y="ITCRM") +
  theme_ft()

plots_list[[length(plots_list)+1]] <- g_arg

# --- Combinar con patchwork ---
n_cols <- ceiling(length(plots_list)/2)
combined_plot <- wrap_plots(plots_list, nrow=2, ncol=n_cols)

# --- Guardar gráfico combinado ---
dir.create("../outputs/figures", showWarnings=FALSE, recursive=TRUE)
ggsave("../outputs/figures/grafico_REER_comparativo_ARG_ITCRM.png",
       combined_plot, width=20, height=10, dpi=300)
