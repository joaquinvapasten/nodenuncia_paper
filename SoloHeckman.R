
# ==========================================================
# 6) ANÁLISIS ESTADÍSTICO (MODELO HECKMAN - CORREGIDO CON RECODIFICACIÓN)
# ==========================================================
# Este bloque replica la metodología del paper de Colombia
# USANDO LA BASE COMPLETA ('enusc_combinada_completa')

if (!is.null(enusc_combinada_completa) && nrow(enusc_combinada_completa) > 0) {
  
  cat("\n---\n")
  cat("ℹ️ Iniciando Bloque 6: Análisis Modelo Heckman (con Recodificación).\n")
  
  # Cargar la librería necesaria
  if (!require(sampleSelection)) {
    install.packages("sampleSelection")
    library(sampleSelection)
  }
  
  # --- 6.1) RECODIFICACIÓN DE VARIABLES ---
  # Aquí agrupamos las variables numéricas en factores manejables.
  
  cat("ℹ️ Recodificando variables sociodemográficas...\n")
  
  data_for_model <- enusc_combinada_completa |>
    mutate(
      # --- Sexo (Factor) ---
      sexo_f = factor(sexo, levels = c(1, 2), labels = c("Hombre", "Mujer")),
      
      # --- Edad (Factor) ---
      # Basado en tramos de manuales 2023/2024 [cite: 2814, 5505]
      edad_f = case_when(
        edad_cat %in% c(1, 2) ~ "15-29", # 15-19 y 20-29
        edad_cat %in% c(3, 4) ~ "30-49", # 30-39 y 40-49
        edad_cat %in% c(5, 6) ~ "50-69", # 50-59 y 60-69
        edad_cat == 7 ~ "70+",
        TRUE ~ NA_character_ # Agrupa otros códigos o NAs
      ),
      
      # --- Educación (Factor) ---
      # Basado en tramos de manuales 2023/2024 [cite: 2688, 2819] (y similar a 2016-2019)
      educ_f = case_when(
        educ %in% c(0, 1, 3, 4) ~ "Basica o menos", # 0=Sin, 1=Básica (2023), 3=Básica, 4=Primaria (2016-19)
        educ %in% c(2, 5, 6, 7, 8) ~ "Media", # 2=Secundaria (2023), 5-8 (Media C-H, T-P, etc. 2016-19)
        educ %in% c(3, 9, 10, 11, 12, 13) ~ "Superior", # 3=Superior (2023), 9-13 (Técnica, Prof, etc. 2016-19)
        TRUE ~ NA_character_ # Agrupa 90, 96, 99 y NAs
      ),
      
      # --- NSE (Factor) ---
      # Basado en 2023/2024[cite: 2822], Quintil (2016-19) es similar
      nse_f = case_when(
        nse %in% c(1, 2, 1) ~ "Bajo",     # Quintil 1 y 2 (2016-19) o NSE 1 (2023-24)
        nse %in% c(3, 2) ~ "Medio",    # Quintil 3 (2016-19) o NSE 2 (2023-24)
        nse %in% c(4, 5, 3) ~ "Alto",     # Quintil 4 y 5 (2016-19) o NSE 3 (2023-24)
        TRUE ~ NA_character_
      ),
      
      # --- Estado Civil (Factor) ---
      # Basado en 2023/2024 [cite: 2819]
      estado_f = case_when(
        estado_civil %in% c(1, 2) ~ "Casado/Conviviente",
        estado_civil == 5 ~ "Soltero",
        estado_civil %in% c(3, 4) ~ "Separado/Viudo/Anulado",
        TRUE ~ NA_character_ # Agrupa 8, 96, etc.
      ),
      
      # --- Pueblo Originario (Factor) ---
      # Basado en 2023/2024 [cite: 2817]
      pueblo_f = case_when(
        pueblo_orig == 1 ~ "Pertenece",
        pueblo_orig == 2 | pueblo_orig == 10 | pueblo_orig == 11 ~ "No Pertenece", # 10/11 son "No pertenece" en manuales antiguos
        TRUE ~ NA_character_ # Agrupa 88, 99, 96 y NAs
      ),
      
      # --- Confianza (Factor) ---
      # Agrupamos 1=Mucha/Bastante y 2=Poca/Nada
      # O 1=Muy Mal/Mal y 2=Bien/Muy Bien para 2019
      conf_carab_f = case_when(
        conf_carab %in% c(1, 2, 4) ~ "Alta/Bien", # 1=Mucha, 2=Bastante (2016-17, 23-24) | 4=Muy Bien (2019)
        conf_carab %in% c(3, 4, 1) ~ "Baja/Mal",  # 3=Poca, 4=Nada (2016-17, 23-24) | 1=Muy Mal (2019)
        TRUE ~ NA_character_
      ),
      conf_pdi_f = case_when(
        conf_pdi %in% c(1, 2, 4) ~ "Alta/Bien",
        conf_pdi %in% c(3, 4, 1) ~ "Baja/Mal",
        TRUE ~ NA_character_
      ),
      conf_fisc_f = case_when(
        conf_fisc %in% c(1, 2, 4) ~ "Alta/Bien",
        conf_fisc %in% c(3, 4, 1) ~ "Baja/Mal",
        TRUE ~ NA_character_
      ),
      
      # --- Aumento Delincuencia Barrio (Factor) ---
      aum_del_barrio_f = case_when(
        aum_del_barrio == 1 ~ "Aumento",
        aum_del_barrio %in% c(2, 3) ~ "Mantuvo/Disminuyo",
        TRUE ~ NA_character_
      ),
      
      # --- Tiempo Residencia (Factor) ---
      tiempo_res_f = case_when(
        tiempo_res %in% c(1, 2, 3) ~ "Menos de 10 anos",
        tiempo_res %in% c(4, 5, 6) ~ "10 anos o mas",
        TRUE ~ NA_character_
      ),
      
      # --- Región (Factor) ---
      region_f = factor(region)
    )
  
  # --- 6.2) Filtrar Casos Completos (usando nuevas variables) ---
  data_for_model_clean <- data_for_model |>
    filter(complete.cases(
      hogar_victima_dmcs, fact_hog, region_f, # Variables mínimas
      sexo_f, edad_f, educ_f, nse_f, estado_f, pueblo_f, # Sociodemo
      conf_carab_f, conf_pdi_f, conf_fisc_f, aum_del_barrio_f, tiempo_res_f # Percepción
    )) |>
    filter(
      (hogar_victima_dmcs == 0) | 
        (hogar_victima_dmcs == 1 & !is.na(cifra_oscura_dmcs))
    ) |>
    filter(fact_hog > 0)
  
  cat("ℹ️ Filas válidas para el modelo Heckman (recodificadas y sin NAs):", nrow(data_for_model_clean), "\n")
  
  if(nrow(data_for_model_clean) > 0) {
    
    # --- 6.3) Definición de las Fórmulas (con variables _f) ---
    
    # Ecuación de Selección: Probabilidad de ser víctima
    selection_formula <- hogar_victima_dmcs ~ sexo_f + edad_f + educ_f + 
      nse_f + estado_f + pueblo_f + region_f # <-- Instrumento
    
    # Ecuación de Resultado: Probabilidad de NO denunciar
    # Quitamos edad_f y estado_f para identificación
    outcome_formula <- cifra_oscura_dmcs ~ sexo_f + educ_f + nse_f + pueblo_f + 
      conf_carab_f + conf_pdi_f + conf_fisc_f + 
      aum_del_barrio_f + tiempo_res_f
    
    # --- 6.4) Ejecución del Modelo Heckman (Probit con ponderadores) ---
    cat("ℹ️ Ejecutando modelo heckit() con variables recodificadas... (Esto puede tardar)\n")
    
    tryCatch({
      heckit_model <- heckit(
        selection = selection_formula,
        outcome = outcome_formula,
        data = data_for_model_clean,
        weights = data_for_model_clean$fact_hog, # Ponderador de hogar
        method = "ml"
      )
      
      # --- 6.5) Mostrar Resultados ---
      cat("\n✅ Modelo Heckman completado. Resultados:\n")
      print(summary(heckit_model))
      
      # --- 6.6) Interpretación ---
      cat("\n--- Interpretación de Resultados ---\n")
      cat("1. Revisa el coeficiente 'rho' (ρ) al final de la tabla:\n")
      cat("   - Si 'rho' es estadísticamente significativo (Pr(>|t|) < 0.05), significa que SÍ había sesgo de selección.\n")
      cat("\n2. Revisa la tabla 'Outcome equation' (Ecuación de Resultado):\n")
      cat("   - Estos son los coeficientes que predicen 'cifra_oscura_dmcs' (No Denunciar), ya corregidos.\n")
      cat("   - Coeficiente positivo: AUMENTA la probabilidad de NO denunciar.\n")
      cat("   - Coeficiente negativo: DISMINUYE la probabilidad de NO denunciar (fomenta la denuncia).\n")
      
    }, error = function(e) {
      cat("❌ ERROR al ejecutar el modelo Heckman. El error fue:\n")
      print(e$message)
      cat("\nEl modelo AÚN falla. Revisa la recodificación o simplifica MÁS las fórmulas (quitar más variables).\n")
    })
    
  } else {
    cat("\n❌ No quedaron datos válidos (sin NAs) para ejecutar el modelo Heckman.\n")
    cat("   Esto puede pasar si una de las variables seleccionadas tiene muchos NAs (ej. 'nse' o 'educ' en 2020-2022).\n")
    cat("   Intenta quitando 'educ', 'nse' y 'estado_civil' de las fórmulas y de 'complete.cases()'.\n")
  }
  
} else {
  cat("\n--- No se generó la base combinada. No se puede proceder con el análisis. ---\n")
}