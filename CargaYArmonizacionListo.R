# ==========================================================
# 0) LIBRERÍAS Y OPCIONES
# ==========================================================
# Asegúrate de tener estas librerías instaladas:
install.packages(c("readr", "dplyr", "janitor", "purrr", "stringr", "tibble", "tidyr", "survey", "sampleSelection"))

library(readr)
library(dplyr)
library(janitor)
library(purrr)
library(stringr)
library(tibble)
library(tidyr) # Para unnest y replace_na

options(scipen = 999, dplyr.summarise.inform = FALSE)

# ==========================================================
# 1) CONFIGURACIÓN INICIAL Y FUNCIONES AUXILIARES
# ==========================================================
ANIOS <- 2016:2024 # Años a procesar
CARPETA_DATOS <- "." # RUTA ACTUALIZADA

# --- Funciones auxiliares (robustecidas) ---
guess_delim <- function(file){
  first <- tryCatch({
    readLines(file, n = 1, warn = FALSE, encoding = "UTF-8")
  }, error = function(e_utf8) {
    tryCatch({
      readLines(file, n = 1, warn = FALSE, encoding = "Latin1")
    }, error = function(e_latin1) {
      warning("No se pudo leer la primera línea de ", basename(file))
      return(NULL)
    })
  })
  if (is.null(first) || length(first) == 0 || nchar(first) == 0) return(",")
  if (grepl(";", first)) ";" else ","
}

leer_enusc <- function(path_csv){
  if (!file.exists(path_csv)) {
    warning("Archivo no encontrado: ", path_csv)
    return(NULL)
  }
  delim <- guess_delim(path_csv)
  if(is.null(delim)) return(NULL)
  tryCatch({
    readr::read_delim(path_csv, delim = delim, show_col_types = FALSE, na = c("", "NA", " "), locale = locale(encoding = "UTF-8")) |>
      janitor::clean_names()
  }, error = function(e_utf8) {
    tryCatch({
      warning("Fallo al leer ", basename(path_csv), " con UTF-8, intentando Latin1...")
      readr::read_delim(path_csv, delim = delim, show_col_types = FALSE, na = c("", "NA", " "), locale = locale(encoding = "Latin1")) |>
        janitor::clean_names()
    }, error = function(e_latin1) {
      warning("Fallo al leer ", basename(path_csv), " con UTF-8 y Latin1.")
      return(NULL)
    })
  })
}

getv <- function(df, aliases) {
  nm <- names(df)
  aliases_vec <- unlist(aliases)
  hit <- aliases_vec[aliases_vec %in% nm]
  if (length(hit) == 0) {
    return(rep(NA_character_, nrow(df)))
  }
  return(as.character(df[[hit[1]]]))
}

num <- function(x) suppressWarnings(as.numeric(x))

na_codes <- function(x, codes = c(88, 99, 96)){
  x_char <- as.character(x)
  x_char[x_char %in% as.character(codes)] <- NA
  num(x_char)
}

# ==========================================================
# 2) LISTA MAESTRA DE ALIAS (CON 'region' AÑADIDA)
# ==========================================================
NOMBRES <- list(
  # --- Diseño Muestral, Filtro e Instrumento ---
  kish = c("kish"),
  fact_hog = c("fact_hog", "fact_hog_reg", "fact_hog_regional_102"),
  varstrat = c("varstrat"),
  conglomerado = c("conglomerado"),
  region = c("enc_region", "enc_region16"), # <-- AÑADIDO
  
  # --- Victimización DMCS (Hogar) ---
  scr_rvi     = c("a1_1_1", "screen_rob_rvi"),
  scr_rps     = c("b1_1_1", "screen_rob_rps"),
  scr_rfv     = c("c1_1_1", "screen_rob_rfv"),
  scr_hur     = c("d1_1_1", "screen_rob_hur"),
  scr_les_agr = c("e1_1_1", "screen_rob_agr"),
  scr_rdv     = c("g1_1_1", "screen_rob_rdv"),
  scr_rddv    = c("h1_1_1", "screen_rob_rddv"),
  
  # --- Denuncia DMCS (Hogar) ---
  den_rvi     = c("a2_1_1", "rvi_denuncias"),
  den_rps     = c("b2_1_1", "rps_denuncias"),
  den_rfv     = c("c2_1_1", "rfv_denuncias"),
  den_hur     = c("d2_1_1", "hur_denuncias"),
  den_les_agr = c("e2_1_1", "agr_denuncias"),
  den_rdv     = c("g2_1_1", "rdv_denuncias"),
  den_rddv    = c("h2_1_1", "rddv_denuncias"),
  
  # --- Re-victimización ---
  re_vict_agreg = c("rva_dc", "rvh_dc"),
  
  # --- Sociodemográficas (Kish) ---
  sexo          = c("rph_sexo"),
  edad_cat      = c("rph_edad"),
  educ          = c("rph_nivel"),
  nse           = c("quintil", "rph_nse"),
  estado_civil  = c("rph_estado", "rph_estadocivil"),
  pueblo_orig   = c("rph_pertenencia_indigena"),
  
  # --- Percepción/Contexto (Kish) ---
  conf_carab      = c("p21b_5_1", "p21_1_3", "ev_confia_cch"),
  conf_pdi        = c("p21b_6_1", "p21_2_2", "ev_confia_pdi"),
  conf_fisc       = c("p21b_8_1", "p21_4_1", "ev_confia_fmp"),
  aum_del_barrio  = c("p3_3_1", "p1_3_1", "p_aumento_barrio"),
  tiempo_res      = c("p22_1_1", "antig_sector")
)

# ==========================================================
# 3) FUNCIÓN DE PROCESAMIENTO ANUAL (CON 'region' AÑADIDA)
# ==========================================================
procesar_anio <- function(df_raw, yr) {
  
  # --- 3.1) Filtrar por Kish ---
  kish_var_raw <- getv(df_raw, NOMBRES$kish)
  if (all(is.na(kish_var_raw)) || !"1" %in% kish_var_raw) {
    warning("No se encontró Kish válido para año ", yr, ". Omitiendo este año.")
    return(NULL)
  }
  df_kish <- df_raw |> filter(kish_var_raw == "1")
  cat("Dimensiones después de Kish=1:", nrow(df_kish), "x", ncol(df_kish), "\n")
  if(nrow(df_kish) == 0) {
    warning("Filtro Kish=1 sin resultados para año ", yr, ". Omitiendo.")
    return(NULL)
  }
  
  # --- 3.2) Extracción Inicial (Variables "Raw") ---
  df_tmp <- tibble(
    anio = yr,
    # Diseño Muestral e Instrumento
    fact_hog = num(getv(df_kish, NOMBRES$fact_hog)),
    varstrat = as.character(getv(df_kish, NOMBRES$varstrat)),
    conglomerado = as.character(getv(df_kish, NOMBRES$conglomerado)),
    region = num(getv(df_kish, NOMBRES$region)), # <-- AÑADIDO
    
    # Victimización (Raw)
    scr_rvi  = num(getv(df_kish, NOMBRES$scr_rvi)),
    scr_rps  = num(getv(df_kish, NOMBRES$scr_rps)),
    scr_rfv  = num(getv(df_kish, NOMBRES$scr_rfv)),
    scr_hur  = num(getv(df_kish, NOMBRES$scr_hur)),
    scr_les_agr = num(getv(df_kish, NOMBRES$scr_les_agr)),
    scr_rdv  = num(getv(df_kish, NOMBRES$scr_rdv)),
    scr_rddv = num(getv(df_kish, NOMBRES$scr_rddv)),
    
    # Denuncia (Raw)
    den_rvi_raw  = num(getv(df_kish, NOMBRES$den_rvi)),
    den_rps_raw  = num(getv(df_kish, NOMBRES$den_rps)),
    den_rfv_raw  = num(getv(df_kish, NOMBRES$den_rfv)),
    den_hur_raw  = num(getv(df_kish, NOMBRES$den_hur)),
    den_les_agr_raw = num(getv(df_kish, NOMBRES$den_les_agr)),
    den_rdv_raw  = num(getv(df_kish, NOMBRES$den_rdv)),
    den_rddv_raw = num(getv(df_kish, NOMBRES$den_rddv)),
    
    # Re-victimización (Raw)
    re_vict_agreg_raw = num(getv(df_kish, NOMBRES$re_vict_agreg)),
    
    # Sociodemográficas (Kish)
    sexo          = num(getv(df_kish, NOMBRES$sexo)),
    edad_cat      = num(getv(df_kish, NOMBRES$edad_cat)),
    educ          = num(getv(df_kish, NOMBRES$educ)),
    nse           = num(getv(df_kish, NOMBRES$nse)),
    estado_civil  = num(getv(df_kish, NOMBRES$estado_civil)),
    pueblo_orig   = num(getv(df_kish, NOMBRES$pueblo_orig)),
    
    # Percepción/Contexto (Kish)
    conf_carab      = na_codes(getv(df_kish, NOMBRES$conf_carab)),
    conf_pdi        = na_codes(getv(df_kish, NOMBRES$conf_pdi)),
    conf_fisc       = na_codes(getv(df_kish, NOMBRES$conf_fisc)),
    aum_del_barrio  = na_codes(getv(df_kish, NOMBRES$aum_del_barrio)),
    tiempo_res      = na_codes(getv(df_kish, NOMBRES$tiempo_res))
  )
  
  # --- 3.3) Cálculo de Variables Limpias y Agregadas ---
  df_final <- df_tmp |>
    mutate(
      # Victimización Desagregada (0/1)
      victima_rvi = if_else(is.na(scr_rvi), 0, if_else(scr_rvi == 1, 1, 0)),
      victima_rps = if_else(is.na(scr_rps), 0, if_else(scr_rps == 1, 1, 0)),
      victima_rfv = if_else(is.na(scr_rfv), 0, if_else(scr_rfv == 1, 1, 0)),
      victima_hur = if_else(is.na(scr_hur), 0, if_else(scr_hur == 1, 1, 0)),
      victima_les_agr = if_else(is.na(scr_les_agr), 0, if_else(scr_les_agr == 1, 1, 0)),
      victima_rdv = if_else(is.na(scr_rdv), 0, if_else(scr_rdv == 1, 1, 0)),
      victima_rddv = if_else(is.na(scr_rddv), 0, if_else(scr_rddv == 1, 1, 0)),
      
      # Victimización Agregada (0/1)
      n_delitos_dmcs = victima_rvi + victima_rps + victima_rfv + victima_hur + victima_les_agr + victima_rdv + victima_rddv,
      hogar_victima_dmcs = if_else(n_delitos_dmcs > 0, 1, 0),
      
      # Denuncia Desagregada (1=Sí, 0=No, NA=No víctima)
      denuncio_rvi  = if_else(victima_rvi == 1, if_else(den_rvi_raw == 1, 1, 0, missing = 0), NA_real_),
      denuncio_rps  = if_else(victima_rps == 1, if_else(den_rps_raw == 1, 1, 0, missing = 0), NA_real_),
      denuncio_rfv  = if_else(victima_rfv == 1, if_else(den_rfv_raw == 1, 1, 0, missing = 0), NA_real_),
      denuncio_hur  = if_else(victima_hur == 1, if_else(den_hur_raw == 1, 1, 0, missing = 0), NA_real_),
      denuncio_les_agr = if_else(victima_les_agr == 1, if_else(den_les_agr_raw == 1, 1, 0, missing = 0), NA_real_),
      denuncio_rdv  = if_else(victima_rdv == 1, if_else(den_rdv_raw == 1, 1, 0, missing = 0), NA_real_),
      denuncio_rddv = if_else(victima_rddv == 1, if_else(den_rddv_raw == 1, 1, 0, missing = 0), NA_real_),
      
      # Denuncia Agregada y Cifra Oscura (para víctimas)
      n_denuncias_dmcs = rowSums(across(starts_with("denuncio_"), ~replace_na(., 0)), na.rm = TRUE),
      hogar_denuncio_dmcs = if_else(
        hogar_victima_dmcs == 1,
        if_else(n_denuncias_dmcs > 0, 1, 0),
        NA_real_
      ),
      cifra_oscura_dmcs = if_else(
        hogar_victima_dmcs == 1,
        1 - hogar_denuncio_dmcs,
        NA_real_
      ),
      
      # Re-victimización Limpia
      re_victima = if_else(
        hogar_victima_dmcs == 1,
        if_else(re_vict_agreg_raw == 1, 1, 0, missing = 0),
        NA_real_
      )
    )
  
  return(df_final)
}

# ==========================================================
# 4) BUCLE DE CARGA Y PROCESAMIENTO
# ==========================================================
lista_enusc_procesada <- list()

for (yr in ANIOS) {
  cat("\n================ Procesando ENUSC", yr, "================\n")
  
  archivo_csv_base <- paste0("enusc", yr, ".csv")
  archivo_csv <- file.path(CARPETA_DATOS, archivo_csv_base)
  
  df_raw <- leer_enusc(archivo_csv)
  if (is.null(df_raw)) {
    archivo_alternativo1 <- file.path(CARPETA_DATOS, paste0("base_enusc_", yr, ".csv"))
    cat("Intentando con nombre alternativo:", basename(archivo_alternativo1), "\n")
    df_raw <- leer_enusc(archivo_alternativo1)
  }
  # ... (añadir más intentos si es necesario) ...
  
  if (is.null(df_raw)) {
    cat("Omitiendo año", yr, "- No se pudo leer el archivo CSV.\n")
    next
  }
  
  cat("Dimensiones originales:", nrow(df_raw), "x", ncol(df_raw), "\n")
  
  # Procesar el dataframe
  df_procesado_anio <- tryCatch({
    procesar_anio(df_raw, yr)
  }, error = function(e) {
    cat("⚠️ Error procesando año", yr, ": ", e$message, "\n")
    return(NULL)
  })
  
  if (!is.null(df_procesado_anio)) {
    cat("Dimensiones finales procesadas para", yr, ":", nrow(df_procesado_anio), "x", ncol(df_procesado_anio), "\n")
    lista_enusc_procesada[[as.character(yr)]] <- df_procesado_anio
  }
  
  rm(df_raw, df_procesado_anio); gc()
}

# ==========================================================
# 5) BLOQUE FLEXIBLE: CONSOLIDACIÓN Y SELECCIÓN FINAL
# ==========================================================
if (length(lista_enusc_procesada) > 0) {
  
  # --- 5.1) Unir todas las bases anuales ---
  enusc_bruta_combinada <- dplyr::bind_rows(lista_enusc_procesada)
  
  cat("\n✅ Bases combinadas. Dimensiones totales (brutas):", nrow(enusc_bruta_combinada), "x", ncol(enusc_bruta_combinada), "\n")
  cat("Años incluidos:", paste(sort(unique(enusc_bruta_combinada$anio)), collapse=", "), "\n")
  
  # --- 5.2) BLOQUE FLEXIBLE DE SELECCIÓN ---
  # Define aquí las variables que quieres mantener en la base COMPLETA
  columnas_seleccionadas <- c(
    # --- Clave (Mantener) ---
    "anio",
    "hogar_victima_dmcs",   # 1=Víctima, 0=No Víctima
    "cifra_oscura_dmcs",    # 1=No denunció, 0=Sí denunció, NA=No víctima
    
    # --- Diseño Muestral (Mantener) ---
    "fact_hog",
    "varstrat",
    "conglomerado",
    
    # --- Instrumento (Mantener) ---
    "region",
    
    # --- Agregados (Opcional) ---
    "tipo_delito_dmcs",
    "re_victima",
    
    # --- Sociodemográficas (Kish) ---
    "sexo",
    "edad_cat",
    "educ",
    "nse",
    "estado_civil",
    "pueblo_orig",
    
    # --- Percepción/Contexto (Kish) ---
    "conf_carab",
    "conf_pdi",
    "conf_fisc",
    "aum_del_barrio",
    "tiempo_res",
    
    # --- Desagregadas (Activadas) ---
    "victima_rvi", "denuncio_rvi",
    "victima_rps", "denuncio_rps",
    "victima_rfv", "denuncio_rfv",
    "victima_hur", "denuncio_hur",
    "victima_les_agr", "denuncio_les_agr",
    "victima_rdv", "denuncio_rdv",
    "victima_rddv", "denuncio_rddv"
    
  ) # <-- ¡Sin coma al final!
  
  # Filtra solo las columnas que existen en la base combinada
  columnas_existentes <- columnas_seleccionadas[columnas_seleccionadas %in% names(enusc_bruta_combinada)]
  
  enusc_combinada_completa <- enusc_bruta_combinada |>
    select(all_of(columnas_existentes))
  
  cat("\n✅ Base de datos 'enusc_combinada_completa' creada con", ncol(enusc_combinada_completa), "columnas.\n")
  
  # --- 5.3) Creación de la base SÓLO VÍCTIMAS (para exploración) ---
  enusc_victimas <- enusc_combinada_completa |>
    filter(hogar_victima_dmcs == 1)
  
  cat("✅ Base de datos 'enusc_victimas' creada con", nrow(enusc_victimas), "filas.\n")
  print(head(enusc_victimas))
  
} else {
  cat("\n❌ No se procesó ninguna base de datos exitosamente.\n")
  enusc_combinada_completa <- NULL
  enusc_victimas <- NULL
}
