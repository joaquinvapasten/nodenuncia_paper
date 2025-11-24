# ==========================================================
# ARCHIVO: 02_analisis_modelo.R (HECKPROBIT CORREGIDO)
# ==========================================================
library(dplyr)
library(sampleSelection) # Para la función selection()

# ==========================================================
# 0) CARGAR BASE DE DATOS
# ==========================================================
# Intentamos ruta absoluta y relativa
RUTA_ARCHIVO <- "C:/Users/Joaking/Desktop/No denuncia/nodenuncia_paper/data/base_datos_final.rds"
if (!file.exists(RUTA_ARCHIVO)) RUTA_ARCHIVO <- "data/base_datos_final.rds"

if (file.exists(RUTA_ARCHIVO)) {
  cat("ℹ️ Cargando base desde:", RUTA_ARCHIVO, "\n")
  enusc_combinada_completa <- readRDS(RUTA_ARCHIVO)
  cat("✅ Base cargada: ", nrow(enusc_combinada_completa), " filas.\n")
} else {
  stop("❌ ERROR: No encuentro el archivo 'base_datos_final.rds'. Ejecuta el Script 1 primero.")
}

# ==========================================================
# 1) PREPARACIÓN DE VARIABLES (RECUPERACIÓN DE MUESTRA)
# ==========================================================
cat("ℹ️ Recodificando variables y recuperando NAs...\n")

data_for_model <- enusc_combinada_completa |>
  mutate(
    # --- Sexo ---
    sexo_f = factor(sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
    
    # --- Edad (Corrección por tramos anuales) ---
    edad_f = case_when(
      anio <= 2018 & edad_cat >= 15 & edad_cat <= 29 ~ "Joven (15-29)",
      anio <= 2018 & edad_cat >= 30 & edad_cat <= 49 ~ "Adulto (30-49)",
      anio <= 2018 & edad_cat >= 50 & edad_cat <= 69 ~ "Adulto Mayor (50-69)",
      anio <= 2018 & edad_cat >= 70 ~ "Senior (70+)",
      
      (anio >= 2019 & anio <= 2022) & edad_cat %in% c(1, 2, 3) ~ "Joven (15-29)",
      (anio >= 2019 & anio <= 2022) & edad_cat %in% c(4, 5) ~ "Adulto (30-49)",
      (anio >= 2019 & anio <= 2022) & edad_cat %in% c(6, 7) ~ "Adulto Mayor (50-69)",
      (anio >= 2019 & anio <= 2022) & edad_cat >= 8 ~ "Senior (70+)",
      
      anio >= 2023 & edad_cat %in% c(1, 2) ~ "Joven (15-29)",
      anio >= 2023 & edad_cat %in% c(3, 4) ~ "Adulto (30-49)",
      anio >= 2023 & edad_cat %in% c(5, 6) ~ "Adulto Mayor (50-69)",
      anio >= 2023 & edad_cat == 7 ~ "Senior (70+)",
      TRUE ~ NA_character_
    ),
    
    # --- Educación (Con categoría "Sin Dato" para salvar el 2020) ---
    educ_f_temp = case_when(
      (anio <= 2019) & educ >= 0 & educ <= 4 ~ "Basica",
      (anio <= 2019) & educ >= 5 & educ <= 8 ~ "Media",
      (anio <= 2019) & educ >= 9 & educ <= 13 ~ "Superior",
      (anio >= 2021) & educ %in% c(0, 1) ~ "Basica",
      (anio >= 2021) & educ == 2 ~ "Media",
      (anio >= 2021) & educ == 3 ~ "Superior",
      TRUE ~ "Sin Dato" # Captura 2020 y NAs de otros años
    ),
    educ_f = factor(educ_f_temp, levels = c("Basica", "Media", "Superior", "Sin Dato")),
    
    # --- Confianza (Recuperando los 100k perdidos como NS/NR) ---
    conf_carab_temp = case_when(
      conf_carab %in% c(1, 2) ~ "Alta",
      conf_carab %in% c(3, 4) ~ "Baja",
      TRUE ~ "NS/NR" # Recupera NAs y años sin pregunta
    ),
    conf_carab_f = factor(conf_carab_temp, levels = c("Alta", "Baja", "NS/NR")),
    
    conf_pdi_temp = case_when(
      conf_pdi %in% c(1, 2) ~ "Alta",
      conf_pdi %in% c(3, 4) ~ "Baja",
      TRUE ~ "NS/NR"
    ),
    conf_pdi_f = factor(conf_pdi_temp, levels = c("Alta", "Baja", "NS/NR")),
    
    # --- Otros Controles ---
    pueblo_f = if_else(pueblo_orig == 1, "Pertenece", "No Pertenece", missing="No Pertenece"),
    aum_del_barrio_f = if_else(aum_del_barrio == 1, "Aumentó", "Mantuvo/Disminuyó", missing="Mantuvo/Disminuyó"),
    anio_f = factor(anio),
    region_f = factor(region)
  )

# ==========================================================
# 2) FILTRADO DE DATOS
# ==========================================================
data_model_clean <- data_for_model |>
  filter(complete.cases(
    hogar_victima_dmcs, region_f, anio_f,
    sexo_f, edad_f, educ_f,
    conf_carab_f, conf_pdi_f, # Ahora tienen "NS/NR", no se borran
    pueblo_f, aum_del_barrio_f
  )) |>
  filter((hogar_victima_dmcs == 0) | (hogar_victima_dmcs == 1 & !is.na(cifra_oscura_dmcs)))

cat("✅ Muestra final para el modelo:", nrow(data_model_clean), "observaciones.\n")

# ==========================================================
# 3) MODELO HECKMAN (VERSIÓN ROBUSTA 2-STEP)
# ==========================================================

if(nrow(data_model_clean) > 5000) {
  cat("ℹ️ Ejecutando Heckman (2-step) con pesos normalizados...\n")
  
  # 1. Normalizar pesos (Crucial para que no explote)
  data_model_clean$peso_norm <- data_model_clean$fact_hog / mean(data_model_clean$fact_hog)
  
  # 2. Definir Ecuaciones
  selection_eq <- hogar_victima_dmcs ~ sexo_f + edad_f + educ_f + region_f + 
    anio_f + pueblo_f
  
  outcome_eq <- cifra_oscura_dmcs ~ sexo_f + educ_f + 
    pueblo_f + anio_f + aum_del_barrio_f + 
    conf_carab_f + conf_pdi_f +            
    victima_rvi + victima_rps + victima_rfv + victima_hur + 
    victima_les_agr + victima_rdv + victima_rddv
  
  # Bloque corregido SIN pesos para obtener significancia estadística
  heckit_model <- heckit(
    selection = selection_eq,
    outcome = outcome_eq,
    data = data_model_clean,
    # weights = data_model_clean$peso_norm,  <-- COMENTAR ESTO ES LA CLAVE
    method = "2step" 
  )
  
  print(summary(heckit_model))
  
  cat("\n--- INTERPRETACIÓN FINAL ---\n")
  cat("• Rho (ρ): Si es significativo, confirma el sesgo de selección.\n")
  cat("• NS/NR: Observa los coeficientes de conf_carab_fNS/NR.\n")
  
} else {
  cat("⚠️ Alerta: Muestra insuficiente.\n")
}