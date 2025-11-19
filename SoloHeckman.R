# ==========================================================
# ARCHIVO: 02_analisis_modelo.R (SOLO HECKMAN)
# ==========================================================
library(dplyr)
library(sampleSelection)

# ==========================================================
# 0) CONFIGURACIÓN Y CARGA
# ==========================================================

# 1. Cargar la base guardada por el Script 01
# IMPORTANTE: Se usará la ruta absoluta como primer intento para asegurar la carga.
RUTA_BASE_ABSOLUTA <- "C:/Users/Joaking/Desktop/No denuncia/nodenuncia_paper/data/base_datos_final.rds"

RUTA_ARCHIVO_A_CARGAR <- RUTA_BASE_ABSOLUTA

if (!file.exists(RUTA_ARCHIVO_A_CARGAR)) {
  # Intento alternativo (ruta relativa, si el usuario está en la carpeta del repo)
  RUTA_ARCHIVO_A_CARGAR <- "data/base_datos_final.rds"
}

if (file.exists(RUTA_ARCHIVO_A_CARGAR)) {
  cat("ℹ️ Cargando base desde:", RUTA_ARCHIVO_A_CARGAR, "\n")
  enusc_combinada_completa <- readRDS(RUTA_ARCHIVO_A_CARGAR)
  cat("✅ Base cargada: ", nrow(enusc_combinada_completa), " filas.\n")
} else {
  stop("❌ ERROR: No encuentro el archivo 'base_datos_final.rds'. Ejecuta el Script 1 primero.")
}

# ==========================================================
# 1) PREPARACIÓN DE VARIABLES
# ==========================================================
cat("ℹ️ Recodificando variables para el modelo...\n")

data_for_model <- enusc_combinada_completa |>
  mutate(
    # --- Sexo ---
    sexo_f = factor(sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    
    # --- Edad (Recodificación optimizada, asumiendo edades exactas y tramos) ---
    edad_f = case_when(
      edad_cat %in% c(1, 2) | (edad_cat >= 15 & edad_cat <= 29) ~ "Joven (15-29)",
      edad_cat %in% c(3, 4) | (edad_cat >= 30 & edad_cat <= 49) ~ "Adulto (30-49)",
      edad_cat %in% c(5, 6) | (edad_cat >= 50 & edad_cat <= 64) ~ "Adulto Mayor (50-64)",
      edad_cat >= 7 | edad_cat >= 65 ~ "Senior (65+)",
      TRUE ~ NA_character_
    ),
    
    # --- Educación (Recodificación optimizada) ---
    educ_f = case_when(
      educ %in% c(0, 1, 3, 4) ~ "Basica",
      educ %in% c(2, 5, 6, 7, 8) ~ "Media",
      educ %in% c(9, 10, 11, 12, 13) ~ "Superior",
      TRUE ~ NA_character_
    ),
    
    # --- Confianza (Optimizada: 1,2=Alta vs 3,4=Baja) ---
    conf_carab_f = if_else(conf_carab %in% c(1, 2), "Alta", "Baja"),
    conf_pdi_f   = if_else(conf_pdi %in% c(1, 2),   "Alta", "Baja"),
    
    # --- Región ---
    region_f = factor(region)
  )

# ==========================================================
# 2) FILTRADO DE DATOS (Estrategia de Maximización)
# ==========================================================

# Se quitan variables con NAs masivos (conf_fisc, nse, estado_civil) 
# para retener la muestra grande (~223,000 obs).
data_model_clean <- data_for_model |>
  filter(complete.cases(
    hogar_victima_dmcs, region_f,  
    sexo_f, edad_f, educ_f,        
    conf_carab_f, conf_pdi_f       
  )) |>
  # Filtro lógico: O no es víctima, o es víctima con dato de denuncia
  filter((hogar_victima_dmcs == 0) | (hogar_victima_dmcs == 1 & !is.na(cifra_oscura_dmcs)))

cat("✅ Muestra final para el modelo:", nrow(data_model_clean), "observaciones.\n")

# ==========================================================
# 3) MODELO HECKMAN CON CONTROL DE DELITOS
# ==========================================================

if(nrow(data_model_clean) > 2000) {
  cat("ℹ️ Ejecutando Heckman con control de tipo de delito...\n")
  
  # 1. SELECCIÓN (¿Quién es víctima? - Usa variables estructurales básicas)
  selection_eq <- hogar_victima_dmcs ~ sexo_f + edad_f + educ_f + region_f
  
  # 2. RESULTADO (¿Quién NO denuncia?)
  # Se agregan los 7 indicadores de delito como variables de control
  outcome_eq <- cifra_oscura_dmcs ~ sexo_f + educ_f + conf_carab_f + conf_pdi_f +
    victima_rvi + victima_rps + victima_rfv + victima_hur + victima_les_agr + victima_rdv + victima_rddv
  
  heckit_model <- heckit(
    selection = selection_eq,
    outcome = outcome_eq,
    data = data_model_clean,
    weights = data_model_clean$fact_hog, # Ponderador de hogar
    method = "ml" 
  )
  
  print(summary(heckit_model))
  
  cat("\n--- INTERPRETACIÓN CON TIPO DE DELITO ---\n")
  cat("• Rho (ρ): Indica si la corrección por sesgo era necesaria.\n")
  cat("• Coeficientes Outcome (victima_*): Muestran si ese delito en específico influye en la no denuncia, una vez controlado por todo lo demás.\n")
  
} else {
  cat("⚠️ Alerta: Tienes muy pocos datos. Revisa los filtros.\n")
}