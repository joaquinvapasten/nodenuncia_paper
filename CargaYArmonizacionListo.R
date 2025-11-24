# ==========================================================
# ARCHIVO: 01_crear_base.R (LECTURA INTELIGENTE)
# ==========================================================
library(readr)
library(dplyr)
library(janitor)
library(purrr)
library(stringr)
library(tibble)
library(tidyr)

options(scipen = 999, dplyr.summarise.inform = FALSE)

# 1. CONFIGURACIÓN
ANIOS <- 2016:2024
CARPETA_DATOS <- "C:/Users/Joaking/Desktop/No denuncia/nodenuncia_paper/data"

# 2. FUNCIÓN DE LECTURA "A PRUEBA DE BALAS"
leer_enusc <- function(path_csv, anio_actual = NULL){
  if (!file.exists(path_csv)) return(NULL)
  
  # --- ESTRATEGIA FUERZA BRUTA ---
  # 1. Intentamos primero con PUNTO Y COMA (Más común en ENUSC antigua)
  tryCatch({
    df <- readr::read_delim(path_csv, delim = ";", 
                            show_col_types = FALSE, 
                            col_types = cols(.default = "c"), # Todo texto
                            na = c("", "NA", " "), 
                            locale = locale(encoding = "UTF-8"))
    
    # VERIFICACIÓN: Si tiene solo 1 columna, el separador estaba mal.
    if (ncol(df) <= 1) {
      # 2. Reintentamos con COMA
      df <- readr::read_delim(path_csv, delim = ",", 
                              show_col_types = FALSE, 
                              col_types = cols(.default = "c"), 
                              na = c("", "NA", " "), 
                              locale = locale(encoding = "UTF-8"))
    }
    
    # Limpiamos nombres al final
    return(janitor::clean_names(df))
    
  }, error = function(e) {
    # Si falla UTF-8, intentamos Latin1 con la misma lógica
    tryCatch({
      df <- readr::read_delim(path_csv, delim = ";", show_col_types = F, col_types = cols(.default = "c"), na=c("","NA"," "), locale=locale(encoding="Latin1"))
      if (ncol(df) <= 1) {
        df <- readr::read_delim(path_csv, delim = ",", show_col_types = F, col_types = cols(.default = "c"), na=c("","NA"," "), locale=locale(encoding="Latin1"))
      }
      return(janitor::clean_names(df))
    }, error = function(e2) return(NULL))
  })
}

getv <- function(df, aliases) {
  nm <- names(df)
  hit <- unlist(aliases)[unlist(aliases) %in% nm]
  if (length(hit) == 0) return(rep(NA_character_, nrow(df)))
  return(as.character(df[[hit[1]]]))
}

# Función numérica corregida para leer decimales chilenos
num <- function(x) {
  # Paso 1: Reemplazar coma decimal por punto
  x_clean <- gsub(",", ".", x) 
  # Paso 2: Convertir a número
  suppressWarnings(as.numeric(x_clean))
}

# 3. DICCIONARIO (Asegúrate que esté completo)
NOMBRES <- list(
  kish = c("kish"),
  fact_hog = c("fact_hog", "fact_hog_reg", "fact_hog_regional_102"),
  varstrat = c("varstrat"),
  conglomerado = c("conglomerado"),
  region = c("enc_region", "enc_region16", "region"),
  
  scr_rvi = c("a1_1_1", "screen_rob_rvi", "rvi"),
  scr_rps = c("b1_1_1", "screen_rob_rps", "rps"),
  scr_rfv = c("c1_1_1", "screen_rob_rfv", "rfv"),
  scr_hur = c("d1_1_1", "screen_rob_hur", "hur"),
  scr_les_agr = c("e1_1_1", "screen_rob_agr", "agr"),
  scr_rdv = c("g1_1_1", "screen_rob_rdv", "rdv"),
  scr_rddv = c("h1_1_1", "screen_rob_rddv", "rddv"),
  
  den_rvi = c("a2_1_1", "rvi_denuncias"),
  den_rps = c("b2_1_1", "rps_denuncias"),
  den_rfv = c("c2_1_1", "rfv_denuncias"),
  den_hur = c("d2_1_1", "hur_denuncias"),
  den_les_agr = c("e2_1_1", "agr_denuncias"),
  den_rdv = c("g2_1_1", "rdv_denuncias"), 
  den_rddv = c("h2_1_1", "rddv_denuncias"),
  
  re_vict_agreg = c("rva_dc", "rvh_dc"),
  sexo = c("rph_sexo", "sexo"),
  edad_cat = c("rph_edad", "edad"),
  educ = c("rph_nivel", "nivel_educ"),
  nse = c("quintil", "rph_nse"),
  estado_civil = c("rph_estado", "rph_estadocivil"),
  pueblo_orig = c("rph_pertenencia_indigena"),
  
  conf_carab = c("p21b_5_1", "ev_confia_cch"),
  conf_pdi = c("p21b_6_1", "ev_confia_pdi"),
  conf_fisc = c("p21b_8_1", "ev_confia_fmp"),
  aum_del_barrio = c("p3_3_1", "p_aumento_barrio"),
  tiempo_res = c("p22_1_1", "antig_sector")
)

# 4. PROCESAMIENTO
procesar_anio <- function(df_raw, yr) {
  kish_var <- getv(df_raw, NOMBRES$kish)
  if (!"1" %in% kish_var) return(NULL)
  df_kish <- df_raw |> filter(kish_var == "1")
  
  df_tmp <- tibble(
    anio = yr,
    fact_hog = num(getv(df_kish, NOMBRES$fact_hog)),
    region = num(getv(df_kish, NOMBRES$region)),
    
    scr_rvi = num(getv(df_kish, NOMBRES$scr_rvi)),
    scr_rps = num(getv(df_kish, NOMBRES$scr_rps)),
    scr_rfv = num(getv(df_kish, NOMBRES$scr_rfv)),
    scr_hur = num(getv(df_kish, NOMBRES$scr_hur)),
    scr_les_agr = num(getv(df_kish, NOMBRES$scr_les_agr)),
    scr_rdv = num(getv(df_kish, NOMBRES$scr_rdv)),
    scr_rddv = num(getv(df_kish, NOMBRES$scr_rddv)),
    
    # Importante: Aquí convertimos el texto " " o "1" a número
    den_rvi_raw = num(getv(df_kish, NOMBRES$den_rvi)),
    den_rps_raw = num(getv(df_kish, NOMBRES$den_rps)),
    den_rfv_raw = num(getv(df_kish, NOMBRES$den_rfv)),
    den_hur_raw = num(getv(df_kish, NOMBRES$den_hur)),
    den_les_agr_raw = num(getv(df_kish, NOMBRES$den_les_agr)),
    den_rdv_raw = num(getv(df_kish, NOMBRES$den_rdv)),
    den_rddv_raw = num(getv(df_kish, NOMBRES$den_rddv)),
    
    re_vict_agreg_raw = num(getv(df_kish, NOMBRES$re_vict_agreg)),
    sexo = num(getv(df_kish, NOMBRES$sexo)),
    edad_cat = num(getv(df_kish, NOMBRES$edad_cat)),
    educ = num(getv(df_kish, NOMBRES$educ)),
    nse = num(getv(df_kish, NOMBRES$nse)),
    estado_civil = num(getv(df_kish, NOMBRES$estado_civil)),
    pueblo_orig = num(getv(df_kish, NOMBRES$pueblo_orig)),
    conf_carab = na_codes(getv(df_kish, NOMBRES$conf_carab)),
    conf_pdi = na_codes(getv(df_kish, NOMBRES$conf_pdi)),
    conf_fisc = na_codes(getv(df_kish, NOMBRES$conf_fisc)),
    aum_del_barrio = na_codes(getv(df_kish, NOMBRES$aum_del_barrio)),
    tiempo_res = na_codes(getv(df_kish, NOMBRES$tiempo_res))
  )
  
  df_final <- df_tmp |> mutate(
    victima_rvi = if_else(is.na(scr_rvi), 0, if_else(scr_rvi==1, 1, 0)),
    victima_rps = if_else(is.na(scr_rps), 0, if_else(scr_rps==1, 1, 0)),
    victima_rfv = if_else(is.na(scr_rfv), 0, if_else(scr_rfv==1, 1, 0)),
    victima_hur = if_else(is.na(scr_hur), 0, if_else(scr_hur==1, 1, 0)),
    victima_les_agr = if_else(is.na(scr_les_agr), 0, if_else(scr_les_agr==1, 1, 0)),
    victima_rdv = if_else(is.na(scr_rdv), 0, if_else(scr_rdv==1, 1, 0)),
    victima_rddv = if_else(is.na(scr_rddv), 0, if_else(scr_rddv==1, 1, 0)),
    
    n_delitos = victima_rvi + victima_rps + victima_rfv + victima_hur + victima_les_agr + victima_rdv + victima_rddv,
    hogar_victima_dmcs = if_else(n_delitos > 0, 1, 0),
    
    # LÓGICA DE DENUNCIA ROBUSTA (Case When)
    denuncio_rvi = case_when(victima_rvi==0 ~ NA_real_, den_rvi_raw==1 ~ 1, den_rvi_raw %in% c(0,2) ~ 0, TRUE ~ NA_real_),
    denuncio_rps = case_when(victima_rps==0 ~ NA_real_, den_rps_raw==1 ~ 1, den_rps_raw %in% c(0,2) ~ 0, TRUE ~ NA_real_),
    denuncio_rfv = case_when(victima_rfv==0 ~ NA_real_, den_rfv_raw==1 ~ 1, den_rfv_raw %in% c(0,2) ~ 0, TRUE ~ NA_real_),
    denuncio_hur = case_when(victima_hur==0 ~ NA_real_, den_hur_raw==1 ~ 1, den_hur_raw %in% c(0,2) ~ 0, TRUE ~ NA_real_),
    denuncio_les_agr = case_when(victima_les_agr==0 ~ NA_real_, den_les_agr_raw==1 ~ 1, den_les_agr_raw %in% c(0,2) ~ 0, TRUE ~ NA_real_),
    denuncio_rdv = case_when(victima_rdv==0 ~ NA_real_, den_rdv_raw==1 ~ 1, den_rdv_raw %in% c(0,2) ~ 0, TRUE ~ NA_real_),
    denuncio_rddv = case_when(victima_rddv==0 ~ NA_real_, den_rddv_raw==1 ~ 1, den_rddv_raw %in% c(0,2) ~ 0, TRUE ~ NA_real_),
    
    n_denuncias = rowSums(across(starts_with("denuncio_"), ~replace_na(., 0))),
    hogar_denuncio_dmcs = if_else(hogar_victima_dmcs==1, if_else(n_denuncias>0, 1, 0), NA_real_),
    cifra_oscura_dmcs = if_else(hogar_victima_dmcs==1, 1 - hogar_denuncio_dmcs, NA_real_),
    re_victima = if_else(hogar_victima_dmcs==1, if_else(re_vict_agreg_raw==1, 1, 0, missing=0), NA_real_)
  )
  return(df_final)
}

# 5. EJECUCIÓN
lista_procesada <- list()
for (yr in ANIOS) {
  f <- file.path(CARPETA_DATOS, paste0("enusc", yr, ".csv"))
  if (!file.exists(f)) f <- file.path(CARPETA_DATOS, paste0("base_enusc_", yr, ".csv"))
  
  cat("Procesando:", yr, "...\n")
  # Leemos sin pasar el año, porque la función es inteligente
  df_raw <- leer_enusc(f) 
  if (!is.null(df_raw)) lista_procesada[[as.character(yr)]] <- procesar_anio(df_raw, yr)
}

# 6. GUARDADO FINAL
if (length(lista_procesada) > 0) {
  base_final <- bind_rows(lista_procesada)
  
  ruta_guardado <- file.path(CARPETA_DATOS, "base_datos_final.rds")
  saveRDS(base_final, file = ruta_guardado)
  cat("\n💾 ¡LISTO! Base guardada exitosamente en:\n", ruta_guardado, "\n")
} else {
  cat("\n❌ Error: No se pudo procesar ningún año.\n")
}