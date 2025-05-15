# Librerias necesarias
library(readxl)
library(tidyverse)
library(openxlsx)
library(dplyr)
library(gplots)
library(ggplot2)
library(tidyr)
library(skimr)
library(writexl)
library(scales)


# Carga de los datos de "Hospital Annual Utilization Report" -------------------------------------------------------------------------
# install.packages("readxl")
setwd("C:/Users/silvi/OneDrive/4º carrera/TFG/Ingenieria del dato/Conjunto datos") #Establece la dirección de la carpeta que quiero
ann_util <- list.files(path = "ANNUAL UTILIZATION", pattern = "*.xlsx", full.names = TRUE) # Lista con los archivos a cargar
for (i in 1:length(ann_util)) {   # Bucle for para importar cada archivo con el nombre que quiero
  nombre <- paste0("au", 2012 + i - 1)  
  assign(nombre, read_xlsx(ann_util[i]))}


# Se añade a cada dataframe una columna con el año
for (i in 2012:2023) {
  nombre <- paste0("au", i)  # Crea el nombre del dataframe ("au2012")
  assign(nombre, 
         get(nombre) %>% 
           mutate(YEAR = i) %>% 
           select(YEAR, everything()))}


# Se eliminan las columnas innecesarias del 2012 al 2017
for (i in 2012:2017) {
  nombre <- paste0("au", i)
  assign(nombre, 
         get(nombre) %>%
           select(-FAC_ADDRESS_TWO, -FAC_PHONE, -FAC_ADMIN_NAME, -BEG_DATE, -END_DATE, 
                  -PARENT_NAME, -PARENT_ADDRESS_ONE, -PARENT_ADDRESS_TWO, -PARENT_CITY, 
                  -PARENT_STATE, -PARENT_ZIP_9, -REPORT_PREP_NAME, -LIC_STATUS, 
                  -LIC_STATUS_DATE, -LIC_ORIG_DATE, -REPORT_STATUS, -MGT_COMPANY, 
                  -MCAL_PROVIDER_NO, -MCARE_PROVIDER_NO, -ACLAIMS_NO, -ASSEMBLY_DIST, 
                  -SENATE_DIST, -CONGRESS_DIST, -CENS_TRACT, -MED_SVC_STUDY_AREA, 
                  -LACO_SVC_PLAN_AREA, -HEALTH_SVC_AREA, -LICENSE_NO, -LONGITUDE, 
                  -LATITUDE, -EMSA_TRAUMA_CTR_DESIG, -EMSA_TRAUMA_PEDS_CTR_DESIG, 
                  -EQUIP_ACQ_OVER_500K, -EQUIP_01_DESCRIP, -EQUIP_01_VALUE, 
                  -EQUIP_01_ACQUI_DT, -EQUIP_01_ACQUI_MEANS, -EQUIP_02_DESCRIP, 
                  -EQUIP_02_VALUE, -EQUIP_02_ACQUI_DT, -EQUIP_02_ACQUI_MEANS, 
                  -EQUIP_03_DESCRIP, -EQUIP_03_VALUE, -EQUIP_03_ACQUI_DT, 
                  -EQUIP_03_ACQUI_MEANS, -EQUIP_04_DESCRIP, -EQUIP_04_VALUE, 
                  -EQUIP_04_ACQUI_DT, -EQUIP_04_ACQUI_MEANS, -EQUIP_05_DESCRIP, 
                  -EQUIP_05_VALUE, -EQUIP_05_ACQUI_DT, -EQUIP_05_ACQUI_MEANS, 
                  -EQUIP_06_DESCRIP, -EQUIP_06_VALUE, -EQUIP_06_ACQUI_DT, 
                  -EQUIP_06_ACQUI_MEANS, -EQUIP_07_DESCRIP, -EQUIP_07_VALUE, 
                  -EQUIP_07_ACQUI_DT, -EQUIP_07_ACQUI_MEANS, -EQUIP_08_DESCRIP, 
                  -EQUIP_08_VALUE, -EQUIP_08_ACQUI_DT, -EQUIP_08_ACQUI_MEANS, 
                  -EQUIP_09_DESCRIP, -EQUIP_09_VALUE, -EQUIP_09_ACQUI_DT, 
                  -EQUIP_09_ACQUI_MEANS, -EQUIP_10_DESCRIP, -EQUIP_10_VALUE, 
                  -EQUIP_10_ACQUI_DT, -EQUIP_10_ACQUI_MEANS, -CAP_EXP_OVER_1MIL, 
                  -PROJ_01_DESCRIP_CAP_EXP, -PROJ_01_PROJTD_CAP_EXP, 
                  -PROJ_01_OSHPD_PROJ_NO, -PROJ_02_DESCRIP_CAP_EXP, 
                  -PROJ_02_PROJTD_CAP_EXP, -PROJ_02_OSHPD_PROJ_NO, 
                  -PROJ_03_DESCRIP_CAP_EXP, -PROJ_03_PROJTD_CAP_EXP, 
                  -PROJ_03_OSHPD_PROJ_NO, -PROJ_04_DESCRIP_CAP_EXP, 
                  -PROJ_04_PROJTD_CAP_EXP, -PROJ_04_OSHPD_PROJ_NO, 
                  -PROJ_05_DESCRIP_CAP_EXP, -PROJ_05_PROJTD_CAP_EXP, 
                  -PROJ_05_OSHPD_PROJ_NO))}

for (i in 2012:2017) {
  nombre <- paste0("au", i)
  assign(nombre, 
         get(nombre) %>%
           rename( ACUTE_PSYCHIATRIC_PATS_TOT_BY_UNIT_ON_1231 = PSY_CENS_PATIENT_TOTL...126,
                   ACUTE_PSYCHIATRIC_PATS_TOT_BY_AGE_ON_1231 = PSY_CENS_PATIENT_TOTL...130,
                   ACUTE_PSYCHIATRIC_PATS_TOT_BY_PAYOR = PSY_CENS_PATIENT_TOTL...141))}

au2014 <- au2014 %>% select(-...318)     ## Elimino la columna innecesaria de "...318" de au2014


# Se eliminan las columnas innecesarias del 2018-2023 

## Primero se corrigen aquellas variables que se llaman de forma distinta
for (i in 2021:2023) {
  nombre <- paste0("au", i)
  assign(nombre, 
         get(nombre) %>%
           rename(OSHPD_PROJ_NO_01 = HCAI_PROJ_NO_01,
                  OSHPD_PROJ_NO_02 = HCAI_PROJ_NO_02,
                  OSHPD_PROJ_NO_03 = HCAI_PROJ_NO_03,
                  OSHPD_PROJ_NO_04 = HCAI_PROJ_NO_04,
                  OSHPD_PROJ_NO_05 = HCAI_PROJ_NO_05))
}
for (i in 2022:2023) {
  nombre <- paste0("au", i)
  assign(nombre, 
         get(nombre) %>%
           rename(CENS_TRACT = CENSUS_KEY))
}
## Ahora sí se pueden eliminar las columnas (2018-2023)
for (i in 2018:2023) {
  nombre <- paste0("au", i)
  assign(nombre, 
         get(nombre) %>%
           select(-FAC_PHONE, -FAC_ADMIN_NAME, -FAC_OP_PER_BEGIN_DT, -FAC_OP_PER_END_DT, 
                  -FAC_PAR_CORP_NAME, -FAC_PAR_CORP_BUS_ADDR, -FAC_PAR_CORP_CITY, 
                  -FAC_PAR_CORP_STATE, -FAC_PAR_CORP_ZIP, -REPT_PREP_NAME, 
                  -SUBMITTED_DT, -REV_REPT_PREP_NAME, -REVISED_DT, -CORRECTED_DT, 
                  -LICENSE_NO, -LICENSE_EFF_DATE, -LICENSE_EXP_DATE, -LICENSE_STATUS, 
                  -TEACH_RURAL, -LONGITUDE, -LATITUDE, -ASSEMBLY_DIST, -SENATE_DIST, 
                  -CONGRESS_DIST, -CENS_TRACT, -MED_SVC_STUDY_AREA, 
                  -LA_COUNTY_SVC_PLAN_AREA, -HEALTH_SVC_AREA, 
                  -FAC_ACQUIRE_EQUIP_OVER_500K, -DESC_EQUIP_01, -DESC_EQUIP_02, 
                  -DESC_EQUIP_03, -DESC_EQUIP_04, -DESC_EQUIP_05, -DESC_EQUIP_06, 
                  -DESC_EQUIP_07, -DESC_EQUIP_08, -DESC_EQUIP_09, -DESC_EQUIP_10, 
                  -PROJ_OVER_1M, -DESC_PROJ_01, -DESC_PROJ_02, -DESC_PROJ_03, 
                  -DESC_PROJ_04, -DESC_PROJ_05, -EQUIP_VAL_01, -EQUIP_VAL_02, 
                  -EQUIP_VAL_03, -EQUIP_VAL_04, -EQUIP_VAL_05, -EQUIP_VAL_06, 
                  -EQUIP_VAL_07, -EQUIP_VAL_08, -EQUIP_VAL_09, -EQUIP_VAL_10, 
                  -PROJ_EXPENDITURES_01, -PROJ_EXPENDITURES_02, -PROJ_EXPENDITURES_03, 
                  -PROJ_EXPENDITURES_04, -PROJ_EXPENDITURES_05, -DT_AQUIRE_EQUIP_01, 
                  -DT_AQUIRE_EQUIP_02, -DT_AQUIRE_EQUIP_03, -DT_AQUIRE_EQUIP_04, 
                  -DT_AQUIRE_EQUIP_05, -DT_AQUIRE_EQUIP_06, -DT_AQUIRE_EQUIP_07, 
                  -DT_AQUIRE_EQUIP_08, -DT_AQUIRE_EQUIP_09, -DT_AQUIRE_EQUIP_10, 
                  -OSHPD_PROJ_NO_01, -OSHPD_PROJ_NO_02, -OSHPD_PROJ_NO_03, 
                  -OSHPD_PROJ_NO_04, -OSHPD_PROJ_NO_05, -MEANS_FOR_ACQUISITION_01, 
                  -MEANS_FOR_ACQUISITION_02, -MEANS_FOR_ACQUISITION_03, 
                  -MEANS_FOR_ACQUISITION_04, -MEANS_FOR_ACQUISITION_05, 
                  -MEANS_FOR_ACQUISITION_06, -MEANS_FOR_ACQUISITION_07, 
                  -MEANS_FOR_ACQUISITION_08, -MEANS_FOR_ACQUISITION_09, 
                  -MEANS_FOR_ACQUISITION_10))
}


# Carga de la tabla comparativa de variables
setwd("C:/Users/silvi/OneDrive/4º carrera/TFG/Ingenieria del dato/Crosswalk") 
crosswalk <- read_excel("Crosswalk ANN UTIL.xlsx")
crosswalk <- crosswalk %>% select(-Notes)

# Se crea una lista vacía para almacenar los dataframes
lista_df <- list()

# Bucle para leer y procesar cada df
for (i in 2012:2017) {
  
  nombre_df <- paste0("au", i)  # Obtener el dataframe con el nombre correspondiente
  df <- get(nombre_df)
  
  for (j in 1:ncol(df)) {
    nombre_columna <- names(df)[j]  # Se obtiene el nombre de la variable 
    
    fila_coincidencia <- which(crosswalk$"ALIRTS (2017)" == nombre_columna) # Se busca la variable en la columna "ALIRTS (2017)"
    if (fila_coincidencia != 0) {
      
      nuevo_nombre <- crosswalk$"SIERA (2019)"[fila_coincidencia] # Si coincide, se remplaza el nombre de la variable por el de "SIERA (2019)"
      names(df)[j] <- nuevo_nombre
      
    } else {
      print(paste("Columna no encontrada en crosswalk:", nombre_columna)) # Imprime el nombre de la variable si no hay coincidencia
      break
    }
  }
  
  lista_df[[i - 2011]] <- df  # Se agrega el dataframe a la lista
}


# Combinación de todos los dataframes en uno 
df_final_2012_2017 <- bind_rows(lista_df)
df_final_2018_2023 <- bind_rows(au2018, au2019, au2020, au2021, au2022, au2023)
df_final_2018_2023 <- df_final_2018_2023 %>% mutate(FAC_NO = as.double(FAC_NO),
                                                    FAC_ZIP = as.character(FAC_ZIP))

df_au <- bind_rows(df_final_2012_2017, df_final_2018_2023)

df_au <- df_au %>% select(-c("EMER_DEPT_HR_DIVERSION_JAN", 
                             "EMER_DEPT_HR_DIVERSION_FEB", "EMER_DEPT_HR_DIVERSION_MAR", 
                             "EMER_DEPT_HR_DIVERSION_APR", "EMER_DEPT_HR_DIVERSION_MAY", 
                             "EMER_DEPT_HR_DIVERSION_JUN", "EMER_DEPT_HR_DIVERSION_JUL", 
                             "EMER_DEPT_HR_DIVERSION_AUG", "EMER_DEPT_HR_DIVERSION_SEP", 
                             "EMER_DEPT_HR_DIVERSION_OCT", "EMER_DEPT_HR_DIVERSION_NOV", 
                             "EMER_DEPT_HR_DIVERSION_DEC", "INPATIENT_SURG_OPER_RM_MINS", 
                             "OUTPATIENT_SURG_OPER_RM_MINS"))


# Carga de los datos de "Emergency Department" -----------------------------------------------------------
setwd("C:/Users/silvi/OneDrive/4º carrera/TFG/Ingenieria del dato/Conjunto datos") 
ed <- list.files(path = "ED", pattern = "*.xlsx", full.names = TRUE)
for (i in 1:length(ed)) {   
  nombre <- paste0("ed", 2012 + i - 1)  
  assign(nombre, read_xlsx(ed[i]))}


# Se añade a cada df una columna con el año
for (i in 2012:2015) {
  nombre <- paste0("ed", i)
  assign(nombre, 
         get(nombre) %>%
           rename(YEAR = datayear) %>% 
           mutate(YEAR = as.double(YEAR)))
  
}

for (i in 2016:2023) {
  nombre <- paste0("ed", i)  
  assign(nombre, 
         get(nombre) %>% 
           mutate(YEAR = i) %>% 
           select(YEAR, everything()))}


# Se eliminan las columnas innecesarias
for (i in 2012:2013) {
  nombre <- paste0("ed", i)
  assign(nombre,
         get(nombre) %>% select(-CONGRESSIONAL_DISTRICT_DESC, -LA_COUNTY_SPA_DESC, -CONTROL_TYPE_DESC, -oshpd9))}

for (i in 2012:2023) {
  nombre <- paste0("ed", i)
  assign(nombre, 
         get(nombre) %>%
           select(-oshpd_id, -ASSEMBLY_DISTRICT_DESC, -SENATE_DISTRICT_DESC, -MSSA_NAME))}

for (i in 2019:2023) {
  nombre <- paste0("ed", i)
  assign(nombre,
         get(nombre) %>% select(-a_Zip_Blank_Invalid, -a_Zip_CA_Resident, -a_Zip_Homeless,
                                -a_Zip_Out_of_State, -a_Zip_Unknown, -Zip_Blank_Invalid,
                                -Zip_CA_Resident, -Zip_Homeless, -Zip_Out_of_State, -Zip_Unknown))}

ed2014 <- ed2014 %>% select(-RURAL_HOSPITAL_FLAG, -TEACHING_HOSPITAL_FLAG, -CONTROL_TYPE_CATEGORY_DESC, -CONGRESS_DIST)

for (i in 2012:2023) {
  nombre_df <- paste0("ed", i)
  df <- get(nombre_df)
  if ("TEACHING_HOSPITAL_FLAG" %in% names(df)) {
    df <- df %>% select(-TEACHING_HOSPITAL_FLAG, -RURAL_HOSPITAL_FLAG)} 
  if ("CONGRESS_DIST" %in% names(df)) {
    df <- df %>% select(-CONGRESS_DIST)} 
  assign(nombre_df, df)}

for (i in 2012:2020) {
  nombre_df <- paste0("ed", i)
  df <- get(nombre_df)
  
  if ("Age_Under_1" %in% names(df)) {  
    df$Age_0_09 <- rowSums(df[, c("Age_Under_1", "Age_01_09")], na.rm = TRUE)
    df <- df %>% relocate(Age_0_09, .before = Age_10_19)
    df$Age_Under_1 <- NULL    
    df$Age_01_09 <- NULL
  } 
  if ("a_Age_Under_1" %in% names(df)) {  
    df$a_Age_0_09 <- rowSums(df[, c("a_Age_Under_1", "a_Age_01_09")], na.rm = TRUE)
    df <- df %>% relocate(Age_0_09, .before = Age_10_19)
    df$a_Age_Under_1 <- NULL    
    df$a_Age_01_09 <- NULL
  }
  assign(nombre_df, df)}


for (i in 2012:2023) {
  nombre_df <- paste0("ed", i)
  df <- get(nombre_df)
  if ("EC_Rail_Motor_Vehicle" %in% names(df)) {  
    df$EC_All_Transport_Types <- rowSums(df[, c("EC_Other_Vehicle_Transport", "EC_Rail_Motor_Vehicle")], na.rm = TRUE)
    df$EC_Other_Vehicle_Transport <- NULL    
    df$EC_Rail_Motor_Vehicle <- NULL
  } 
  if ("a_EC_Rail_Motor_Vehicle" %in% names(df)) {  
    df$a_EC_All_Transport_Types <- rowSums(df[, c("a_EC_Other_Vehicle_Transport", "a_EC_Rail_Motor_Vehicle")], na.rm = TRUE)
    df$a_EC_Other_Vehicle_Transport <- NULL    
    df$a_EC_Rail_Motor_Vehicle <- NULL
  } 
  assign(nombre_df, df)}

for (i in 2012:2023) {
  nombre_df <- paste0("ed", i)
  df <- get(nombre_df)
  if ("a_DX_Other_Reasons" %in% names(df)) {
    df <- df %>% rename(a_dx_Other_Reasons = a_DX_Other_Reasons)
  } 
  
  if ("a_Dx_Other_Reasons" %in% names(df)) {
    df <- df %>% rename(a_dx_Other_Reasons = a_Dx_Other_Reasons)
  } 
  
  assign(nombre_df, df)}


# Carga de la tabla comparativa de variables
setwd("C:/Users/silvi/OneDrive/4º carrera/TFG/Ingenieria del dato/Crosswalk")
crosswalk <- read_excel("Crosswalk ED.xlsx")

# Se crea una lista vacía para almacenar los dataframes
lista_df <- list()

# Bucle para leer y procesar cada df
for (i in 2012:2023) {
  nombre_df <- paste0("ed", i)
  df <- get(nombre_df)
  
  for (j in 1:ncol(df)) {
    nombre_columna <- names(df)[j]
    if (nombre_columna %in% crosswalk$"SIERA (2019)") {
      next
    } else if (nombre_columna %in% crosswalk$"ALIRTS (2017)"){
      fila_coincidencia <- which(crosswalk$"ALIRTS (2017)" == nombre_columna)
      nuevo_nombre <- crosswalk$"SIERA (2019)"[fila_coincidencia]
      names(df)[j] <- nuevo_nombre
    } else if (nombre_columna %in% crosswalk$"OTHERS"){
      fila_coincidencia <- which(crosswalk$"OTHERS" == nombre_columna)
      nuevo_nombre <- crosswalk$"SIERA (2019)"[fila_coincidencia]
      names(df)[j] <- nuevo_nombre
    } else if (nombre_columna %in% crosswalk$"OTHERS2"){
      fila_coincidencia <- which(crosswalk$"OTHERS2" == nombre_columna)
      nuevo_nombre <- crosswalk$"SIERA (2019)"[fila_coincidencia]
      names(df)[j] <- nuevo_nombre
    } else if (nombre_columna %in% crosswalk$"OTHERS3"){
      fila_coincidencia <- which(crosswalk$"OTHERS3" == nombre_columna)
      nuevo_nombre <- crosswalk$"SIERA (2019)"[fila_coincidencia]
      names(df)[j] <- nuevo_nombre
    } else if (nombre_columna %in% crosswalk$"OTHERS4"){
      fila_coincidencia <- which(crosswalk$"OTHERS4" == nombre_columna)
      nuevo_nombre <- crosswalk$"SIERA (2019)"[fila_coincidencia]
      names(df)[j] <- nuevo_nombre
    } else {
      print(paste("Columna no encontrada en crosswalk:",i,":", nombre_columna))
    }
  }
  
  lista_df[[i - 2011]] <- df   # Se agrega el dataframe a la lista
}

# Transformación de la variable "ED_STATIONS_ON_1231" a numérica
class(lista_df[[5]]$ED_STATIONS_ON_1231)
class(lista_df[[6]]$ED_STATIONS_ON_1231)
for (i in 5:6) {
  lista_df[[i]] <- lista_df[[i]] %>% mutate(ED_STATIONS_ON_1231 = as.double(ED_STATIONS_ON_1231)) 
}

# Combinación de todos los df en uno solo
df_ed <- bind_rows(lista_df)

# Combinación y eliminación de las columnas duplicadas
df_ed$a_dx_Other_Reasons <- rowSums(df_ed[, c("a_dx_Other_Reasons...146", "a_dx_Other_Reasons...143")], na.rm = TRUE)
df_ed <- df_ed %>%
  select(-a_dx_Other_Reasons...170, -a_dx_Other_Reasons...138, -a_dx_Other_Reasons...146, -a_dx_Other_Reasons...143)

df_ed <- df_ed %>% select(-c("a_Age_Unknown", "a_All_Other_Languages", "a_blank_inv_lang", "a_disp_Invalid_Blank", 
                             "a_disp_Other", "a_dx_Other_Reasons", "a_Eth_Other_Unknown",  
                             "a_Other_Payer", "a_Other_Unknown", "a_PLS_Other_Unknown", "a_racegrp_inv_blank", 
                             "a_racegrp_other", "a_Unknown_Eth", "a_Unknown_inv_Payer", "a_Unknown_lang", 
                             "a_Unknown_Race", "Age_Unknown", "All_Other", "All_Other_Languages", 
                             "blank_inv_lang", "disp_Invalid_Blank", "disp_Other", "dx_Other_Unknown", 
                             "EC_Other_Accidents", "EC_Other_Factors", "eth_Blank_Invalid", "eth_Unknown", 
                             "Other_Payer", "Other_Unknown", "ed_lic_levl_end","a_EC_Undetermined",
                             "racegrp_inv_blank", "racegrp_other", "racegrp_unknown", "Sex_Other_Unknown", 
                             "Unknown_inv_Payer", "Unknown_lang",  "Zip_Foreign", "PLS_Other_Unknown",
                             "disp_Not_Defined_Elsewhere","a_disp_Not_Defined_Elsewhere", "a_Zip_Foreign", 
                             "dx_Unacceptable_principal_diagno", "a_racegrp_unknown", "a_Sex_Unknown_Other",
                             "EC_Undetermined", "a_eth_Blank_Invalid", "EC_None", "ED_Visit", "Inpatient_from_ED",
                             "SelfPay", "a_SelfPay"))


# Unión de datasets -------------------------------------------------------

# Renombrar columnas 
df_au <- df_au %>% rename(FACILITY_NAME = FAC_NAME)
df_ed <- df_ed %>% rename(FAC_NO = oshpd_id2)

# Crear FAC_NO_YEAR
df_au$FAC_NO_YEAR <- paste(df_au$FAC_NO, df_au$YEAR, sep = ".")
df_au <- df_au %>% relocate(FAC_NO_YEAR, .before = everything())

df_ed$FAC_NO_YEAR <- paste(df_ed$FAC_NO, df_ed$YEAR, sep = ".")
df_ed <- df_ed %>% relocate(FAC_NO_YEAR, .before = everything())

# Filtrar por hospitales comunes
hospitales_comunes <- intersect(df_au$FAC_NO_YEAR, df_ed$FAC_NO_YEAR)

df_au_comun <- df_au %>% filter(FAC_NO_YEAR %in% hospitales_comunes)
df_ed_comun <- df_ed %>% filter(FAC_NO_YEAR %in% hospitales_comunes)

# Eliminar columnas repetidas
df_ed_comun <- df_ed_comun %>% select(-c("FACILITY_NAME", "COUNTY_NAME", "LICENSE_CATEGORY_DESC", "DBA_ADDRESS1", 
                                         "DBA_CITY", "DBA_ZIP_CODE", "LICENSED_BED_SIZE", "TRAUMA_CENTER_DESC", 
                                         "MSSA_DESIGNATION", "RURAL_HOSPITAL_DESC", "TEACHING_HOSPITAL_DESC", 
                                         "CONTROL_TYPE_DESC", "ER_SERVICE_LEVEL_DESC", "FAC_NO_YEAR")) %>%
  relocate(YEAR, .before = everything())

# Unir datasets 
df_unido <- merge(df_au_comun, df_ed_comun, by = c("FAC_NO", "YEAR"), all.x = TRUE)



# Comprobar duplicados --------------------------------------------------------------

# Comprobar si hay variables duplicadas

duplicated_vars <- names(df_unido)[grepl("\\.x$|\\.y$", names(df_unido))]
if (length(duplicated_vars) == 0) {
  print("No hay variables duplicadas.")
} else {
  print("Variables duplicadas encontradas:")
  print(duplicated_vars)
}


# Comprobar que no hay filas duplicadas

if (any(table(df_unido$FAC_NO, df_unido$YEAR) == 0) == FALSE) {
  
  # Identificar los ID de FAC_NO_YEAR no únicos
  conteo_combinaciones <- table(df_unido$FAC_NO_YEAR)  # Contar ocurrencias
  
  combinaciones_no_unicas <- names(conteo_combinaciones[conteo_combinaciones > 1])  # Solo combinaciones duplicadas
  df_unido[df_unido$FAC_NO_YEAR %in% combinaciones_no_unicas, ]
  
  # Filtrar las filas duplicadas según el identificador combinado
  filas_duplicadas <- df_unido[df_unido$FAC_NO_YEAR %in% combinaciones_no_unicas, ]
  
  # Agrupar y resumir duplicados
  datos_duplicados <- filas_duplicadas %>%
    group_by(FAC_NO_YEAR) %>%
    summarise(
      across(where(is.numeric) & !c("YEAR", "FAC_NO"), sum, na.rm = TRUE),  # Sumar las variables numéricas (excepto YEAR y FAC_NO)
      across(where(is.character), first),  # Tomar el primer valor de los caracteres
      across(where(is.factor), first),     # Tomar el primer valor de los factores
      .groups = "drop"
    )
  
  # Eliminar las filas duplicadas originales del df_unido
  df_unido_sin_duplicados <- df_unido[!df_unido$FAC_NO_YEAR %in% combinaciones_no_unicas, ]
  
  # Unir el dataframe sin duplicados con los datos combinados
  df_final <- bind_rows(df_unido_sin_duplicados, datos_duplicados)
  
  # Ordenar por FAC_NO_YEAR
  df_final <- df_final %>% arrange(FAC_NO_YEAR)
  
  print("Se detectaron y corrigieron los hospitales duplicados")
  
} else {
  df_final <- df_unido
  print(" No se detectaron hospitales duplicados")
}



# Porcentaje de nulos ------------------------------------------------------------------------

nulos_total <- sum(is.na(df_unido)) / (nrow(df_unido) * ncol(df_unido)) * 100
cat("Porcentaje de NAs:", round(nulos_total, 2), "%\n")



# Tratamiento de nulos -------------------------------------------------------------------------------

# Reemplazo de nulos en variables numéricas con 0
for (col_name in names(df_unido)) {
  if (is.numeric(df_unido[[col_name]])) {
    df_unido[[col_name]] <- replace_na(df_unido[[col_name]], 0)
  }
}

# Reemplazo en variables categóricas
df_unido <- df_unido %>%
  replace_na(list(
    SHORT_DOYLE_SERVICES_OFFERED = "NO",
    INPATIENT_HOSPICE_PROG_OFFERED = "NO",
    BED_CLASS_GEN_ACUTE_CARE_SERVICE = "NO",
    BED_CLASS_SN_HOSPICE_SERVICE = "NO",
    BED_CLASS_IC_HOSPICE_SERVICE = "NO",
    INPATIENT_PALLIATIVE_CARE_PROG_OFFERED = "NO",
    OUTPATIENT_PALLIATIVE_CARE_SERV_OFFERED = "NO",
    LIC_ED_LEV_BEGIN = "NO",
    LIC_ED_LEV_END = "NO",
    AVAIL_SERVICES_ANESTHESIOLOGIST_24HR = "NO",
    AVAIL_SERVICES_ANESTHESIOLOGIST_ON_CALL = "NO",
    AVAIL_SERVICES_LAB_24HR = "NO",
    AVAIL_SERVICES_LAB_ON_CALL = "NO",
    AVAIL_SERVICES_OPER_RM_24HR = "NO",
    AVAIL_SERVICES_OPER_RM_ON_CALL = "NO",
    AVAIL_SERVICES_PHARMACIST_24HR = "NO",
    AVAIL_SERVICES_PHARMACIST_ON_CALL = "NO",
    AVAIL_SERVICES_PHYSICIAN_24HR = "NO",
    AVAIL_SERVICES_PHYSICIAN_ON_CALL = "NO",
    AVAIL_SERVICES_PSYCHIATRIC_ER_24HR = "NO",
    AVAIL_SERVICES_PSYCHIATRIC_ER_ON_CALL = "NO",
    AVAIL_SERVICES_RADIOLOGY_24HR = "NO",
    AVAIL_SERVICES_RADIOLOGY_ON_CALL = "NO",
    EMER_DEPT_AMBULANCE_DIVERSION_HOURS = "NO",
    OFFER_AMBULATORY_SURG_PROG = "NO",
    OFFER_ALTERNATE_BIRTH_PROG = "NO",
    ALTERNATE_SETTING_LDR = "NO",
    ALTERNATE_SETTING_LDRP = "NO",
    LIC_CARDIOLOGY_CARDIOVASCULAR_SURG_SERVICES = "NEITHER",
    FAC_OPERATED_THIS_YR = "NO",
    TEACH_HOSP = "NO",
    TRAUMA_CTR = "Non-Trauma Center",
    EMSA_TRAUMA_DESIGNATION = "Not Designated",
    EMSA_TRAUMA_DESIGNATION_PEDIATRIC= "Not Pediatric Designated"))

# Eliminar filas vacías en nombres de hospital si aún existiera alguna
df_unido <- df_unido %>% filter(!is.na(FACILITY_NAME))


# Eliminar variables vacias
empty_col <- names(df_unido)[sapply(df_unido, function(col) all(col == 0, na.rm = TRUE))]
df_unido <- df_unido %>% select(!all_of(empty_col))


# Transformar etiquetas específicas
df_unido <- df_unido %>%
  mutate(LIC_CARDIOLOGY_CARDIOVASCULAR_SURG_SERVICES = case_when(
    LIC_CARDIOLOGY_CARDIOVASCULAR_SURG_SERVICES == "Cardiac Catheterization Only" ~ "CATH LAB",
    LIC_CARDIOLOGY_CARDIOVASCULAR_SURG_SERVICES == "Cardiovascular Surgery Services" ~ "CV SURG",
    TRUE ~ LIC_CARDIOLOGY_CARDIOVASCULAR_SURG_SERVICES
  ))

# Reemplazo de "X" por "Yes"
columnas_con_x <- sapply(df_unido, function(col) any(col == "X", na.rm = TRUE))
for (col_name in names(df_unido)[columnas_con_x]) {
  df_unido[[col_name]] <- ifelse(df_unido[[col_name]] == "X", "Yes", df_unido[[col_name]])
}

# Normalizar "YES"/"NO" → "Yes"/"No"
for (col in names(df_unido)) {
  if (is.character(df_unido[[col]])) {
    df_unido[[col]] <- gsub("YES", "Yes", df_unido[[col]])
    df_unido[[col]] <- gsub("NO", "No", df_unido[[col]])
  }
}


# Ver columnas con valores nulos restantes
nulos <- df_unido %>% summarize(across(everything(), ~sum(is.na(.))))
nulos <- nulos %>% select(where(~ !all(. == 0)))
print(nulos)



# Filtrado hospitales con al menos 10 años de histórico -----------------------------

n_distinct(df_unido$FAC_NO)   # Antes
df_unido <- df_unido %>%
  group_by(FAC_NO) %>%
  filter(n() > 10) %>%
  ungroup()
n_distinct(df_unido$FAC_NO)   # Después


# Corrección en los nombres de los hospitales: -------------------------------------------------------------------------

# Carga de la tabla comparativa 
setwd("C:/Users/silvi/OneDrive/4º carrera/TFG/Ingenieria del dato/Crosswalk")
crosswalk <- read_excel("Crosswalk HOSPITALES.xlsx")

# Iteramos a través de cada fila del dataframe crosswalk
for (i in 1:nrow(crosswalk)) {
  # Obtenemos el nombre actual del hospital desde la columna "Hospitales total"
  nombre_actual <- crosswalk$`Hospitales total`[i]
  
  # Obtenemos el nombre único al que queremos reemplazar desde la columna "Hospitales unicos"
  nombre_unico <- crosswalk$`Hospitales unicos`[i]
  
  # Realizamos el reemplazo en la columna FACILITY_NAME de df_unido
  df_unido$FACILITY_NAME[df_unido$FACILITY_NAME == nombre_actual] <- nombre_unico
}


# Outliers ----------------------------------------------------------------------------

outlier_percent <- df_unido %>%
  select(-c("FAC_NO", "YEAR")) %>%
  select(where(is.numeric)) %>%
  summarise(across(everything(), ~{
    # Filtrar los valores mayores que 0 antes de calcular los cuartiles y el IQR
    valores_positivos <- .[. > 0 & !is.na(.)]
    
    if (length(valores_positivos) < 2) {
      # Si no hay suficientes valores positivos para calcular los cuartiles,
      # devolvemos NA o 0 para el porcentaje de outliers
      return(NA)}
    
    q1 <- quantile(valores_positivos, 0.25)
    q3 <- quantile(valores_positivos, 0.75)
    iqr <- q3 - q1
    lower <- q1 - 1.5 * iqr
    upper <- q3 + 1.5 * iqr
    
    # Calcular el porcentaje de outliers basado en el conjunto de datos original
    outliers <- . < lower | . > upper
    num_outliers <- sum(outliers & . > 0, na.rm = TRUE) # Contar solo outliers positivos
    total_valores_positivos <- sum(. > 0 & !is.na(.))
    
    if (total_valores_positivos > 0) {
      return((num_outliers / total_valores_positivos) * 100)
    } else {
      return(0) # Si no hay valores positivos, el porcentaje de outliers es 0
    }
  })) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "outlier_pct") %>%
  arrange(desc(outlier_pct))


# filtro de un procentaje de outliers >10 
outlier_percent <- outlier_percent  %>%
  filter(outlier_pct >= 10)

vars_con_outliers <- outlier_percent$variable


## HISTOGRAMAS ------------------------------------------------------------------------

# Función para dividir el nombre de la variable
wrap_variable_name <- function(name) {
  underscores <- gregexpr("_", name)[[1]]
  if (length(underscores) > 0) {
    mid_point <- nchar(name) / 2
    closest_underscore_index <- which.min(abs(underscores - mid_point))
    split_index <- underscores[closest_underscore_index]
    wrapped_name <- paste0(substr(name, 1, split_index - 1), "\n_", substr(name, split_index + 1, nchar(name)))
    return(wrapped_name)
  } else {
    return(name)
  }
}

wrapped_vars <- sapply(vars_con_outliers, wrap_variable_name)

# Creación de los histogramas (por grupo)
n_vars <- length(wrapped_vars)
n_cols_facet <- 3 
n_rows_facet <- 3 
vars_per_page <- n_cols_facet * n_rows_facet

for (i in 1:ceiling(n_vars / vars_per_page)) {
  start_index <- (i - 1) * vars_per_page + 1
  end_index <- min(i * vars_per_page, n_vars)
  vars_page <- wrapped_vars[start_index:end_index]
  original_vars_page <- vars_con_outliers[start_index:end_index] # Necesitamos los nombres originales para seleccionar las columnas
  
  df_unido_page <- df_unido %>%
    select(all_of(original_vars_page))
  
  df_long_page <- df_unido_page %>%
    pivot_longer(everything(), names_to = "variable_original", values_to = "valor") %>%
    mutate(variable = factor(sapply(variable_original, wrap_variable_name), levels = vars_page))
  
  print(
    ggplot(df_long_page, aes(x = valor)) +
      geom_histogram(bins = 30, fill = "steelblue", color = "white") +
      facet_wrap(~ variable, scales = "free", ncol = n_cols_facet, nrow = n_rows_facet) +
      labs(
        title = paste("Histograma de las variables con Outliers (Página", i, ")"),
        x = "Valor",
        y = "Frecuencia"
      ) +
      theme_minimal())}

## Variables representativas para los histogramas
variables_especificas <- c(
  "INPATIENT_PALLIATIVE_CARE_PROG_PHYSICIAN_CERTIFIED","a_dx_Psychoses_Neuroses",
  "INPATIENT_PALLIATIVE_CARE_PROG_CHAPLAINS", "disp_Hospice_Care",
  "EMER_DEPT_HR_DIVERSION_TOT","a_dx_Pregnancy_Childbirth","disp_Residential_Care",
  "EMS_VISITS_NON_URGENT_TOT","EMS_VISITS_MODERATE_ADMITTED")


# Función para dividir el nombre de la variable
wrap_variable_name <- function(name) {
  underscores <- gregexpr("_", name)[[1]]
  if (length(underscores) > 0) {
    mid_point <- nchar(name) / 2
    closest_underscore_index <- which.min(abs(underscores - mid_point))
    split_index <- underscores[closest_underscore_index]
    wrapped_name <- paste0(substr(name, 1, split_index - 1), "\n_", substr(name, split_index + 1, nchar(name)))
    return(wrapped_name)}}

# Dataframe solo de las variables seleccionadas
df_long_especificas <- df_unido %>%
  select(all_of(variables_especificas)) %>%
  pivot_longer(everything(), names_to = "variable_original", values_to = "valor") %>%
  mutate(variable = factor(sapply(variable_original, wrap_variable_name), levels = sapply(variables_especificas, wrap_variable_name)))

# Creación de los histogramas con las variables seleccionadas
histograma_especificas <- ggplot(df_long_especificas, aes(x = valor)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  facet_wrap(~ variable, scales = "free", ncol = 3, nrow = 3) + 
  labs(
    title = "Histogramas de 9 variables con outliers",
    x = "Valor",
    y = "Frecuencia"
  ) +
  theme_minimal() +
  theme(strip.text.x = element_text(size = 9.5)) 

print(histograma_especificas)


## BOXPLOTS --------------------------------------------------------------------------------

# Calcular media por variable
orden_valores <- df_unido %>%
  select(all_of(vars_con_outliers)) %>%
  summarise(across(everything(), ~mean(., na.rm = TRUE))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "media") %>%
  arrange(media)

vars_con_outliers_ordenado <- orden_valores$variable

# Dividir en grupos de 4 
grupos_boxplot <- split(vars_con_outliers_ordenado, ceiling(seq_along(vars_con_outliers_ordenado)/5))

# Iterar y graficar boxplots por grupo
lista_de_graficos <- list()
for (i in seq_along(grupos_boxplot)) {
  grupo_i <- grupos_boxplot[[i]]
  df_long_i <- df_unido %>%
    select(all_of(grupo_i)) %>%
    pivot_longer(cols = everything(), names_to = "variable", values_to = "valor") %>%
    filter(valor != 0)
  
  plot_i <- ggplot(df_long_i, aes(x = variable, y = valor)) +
    geom_boxplot(outlier.colour = "red") +
    labs(
      title = paste("Boxplots de las variables > 10% de outliers"),
      x = "",
      y = "Valor") +
    scale_x_discrete(labels = c(
      "ALL_OTHER_CATHETERIZATION_PROC" = "ALL_OTHER_\nCATHETERIZATION_PROC",
      "EC_W_Inanimate_Animate_Object" = "EC_W_Inanimate\n_Animate_Object",
      "INPATIENT_PALLIATIVE_CARE_PROG_SOCIAL_WORKER_CERTIFIED" = "INPATIENT_PALLIATIVE_CARE_\nPROG_SOCIAL_WORKER_CERTIFIED",
      "INPATIENT_PALLIATIVE_CARE_PROG_PHYSICIAN_CERTIFIED" = "INPATIENT_PALLIATIVE_CARE_\nPROG_PHYSICIAN_CERTIFIED",
      "CARDIOVASCULAR_SURG_OPER_ADULT_BYPASS_NOT_USED" = "CARDIOVASCULAR_SURG_OPER_\nADULT_BYPASS_NOT_USED",
      "CARDIOVASCULAR_SURG_OPER_BYPASS_NOT_USED_TOT" = "CARDIOVASCULAR_SURG_OPER_\nBYPASS_NOT_USED_TOT",
      "CARDIOVASCULAR_SURG_OPER_PEDIATRIC_BYPASS_NOT_USED" ="CARDIOVASCULAR_SURG_OPER_\nPEDIATRIC_BYPASS_NOT_USED",
      "GEN_ACUTE_CARE_SN_SWING_BEDS" = "GEN_ACUTE_CARE_\nSN_SWING_BEDS",
      "INPATIENT_PALLIATIVE_CARE_PROG_CHAPLAINS"= "INPATIENT_PALLIATIVE_CARE\n_PROG_CHAPLAINS",
      "CARD_CATH_PED_IP_DIAG_VST" = "CARD_CATH_PED_\nIP_DIAG_VST",
      "a_EC_NonTrans_Drowning_Subm" = "a_EC_NonTrans_\nDrowning_Subm",
      "ACUTE_PSYCH_CDRS_ALOS_CY" = "ACUTE_PSYCH_CDRS_\nALOS_CY",
      "ACUTE_RESPIRATORY_CARE_ALOS_PY" = "ACUTE_RESPIRATORY_\nCARE_ALOS_PY",
      "CORONARY_CARE_ALOS_CY" = "CORONARY_CARE_\nALOS_CY",
      "CORONARY_CARE_ALOS_PY" = "CORONARY_CARE_\nALOS_PY")) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  lista_de_graficos[[i]] <- plot_i
  print(plot_i) 
}



# 10 Valores más altos por variable
for (var_name in vars_con_outliers) {
  if (var_name %in% names(df_unido)) {
    top_10_values <- df_unido %>%
      pull(var_name) %>%
      sort(decreasing = TRUE) %>%
      head(10)
    
    cat(paste("Los 10 valores más altos de la variable:", var_name, "\n"))
    print(top_10_values)
    cat("\n") }}

# Corrección outliers
df_unido$CORONARY_CARE_ALOS_CY[df_unido$CORONARY_CARE_ALOS_CY == 481] <- 48.1
df_unido$CORONARY_CARE_ALOS_PY[df_unido$CORONARY_CARE_ALOS_PY == 481] <- 48.1


# DATASET LIMPIO
setwd("C:/Users/silvi/OneDrive/4º carrera/TFG/Entrega memoria tfg")
write.xlsx(df_unido,"df_unido_def_2.xlsx")




# Análisis estadístico: --------------------------------
skim(df_unido)

skim_output <- df_unido %>% 
  select(where(is.numeric)) %>% 
  select(-FAC_NO,-YEAR) %>%
  skim() %>%
  select(-n_missing, -complete_rate)

numeric_cols <- skim_output %>%
  select_if(is.numeric) %>%
  names()

skim_rounded <- skim_output %>%
  mutate(across(all_of(numeric_cols), round, digits = 2))


# write.xlsx(skim_rounded,"skim_output.xlsx")


# Histogramas con medias de camas ----------------------------------------------------

vars_histogramas <- c("TOT_LIC_BEDS", "TOT_LIC_BED_DAYS", "TOT_DISCHARGES", "TOT_CEN_DAYS")

df_long_page <- df_unido %>%
  select(all_of(vars_histogramas)) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valor")

medias <- df_long_page %>%
  group_by(variable) %>%
  summarise(media = mean(valor, na.rm = TRUE))

etiquetas_facet <- c("TOT_LIC_BEDS" = "Total de camas licenciadas" ,
                     "TOT_LIC_BED_DAYS" = "Total de días de cama licenciadas",
                     "TOT_DISCHARGES" = "Total de altas hospitalarias", 
                     "TOT_CEN_DAYS" = "Total de días de paciente (censo)")
n_cols_facet <- 2
n_rows_facet <- 2
ggplot(df_long_page, aes(x = valor)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "white") +
  geom_vline(data = medias, aes(xintercept = media), color = "red", linewidth = 0.6) +
  facet_wrap(~ variable, scales = "free", ncol = 2, labeller = as_labeller(etiquetas_facet)) +
  labs(title = "Distribución de las variables sobre camas hospitalarias",x = "Total de camas", y = "Frecuencia") +
  scale_x_continuous(labels = label_comma(big.mark = ".", decimal.mark = ",")) +
  theme_minimal()


# Subset diagnósticos ----------------------------------------------------

vars_dx <- names(df_unido)[grepl("^dx_", names(df_unido))]
vars_a_dx <- names(df_unido)[grepl("^a_dx_", names(df_unido))]

diagnosticos_urg <- data.frame(
  diagnosticos = vars_dx,
  total_pacientes = sapply(vars_dx, function(var) sum(df_unido[[var]], na.rm = TRUE)),
  row.names = NULL
)

diagnosticos_adm <- data.frame(
  diagnosticos = vars_a_dx,
  total_pacientes = sapply(vars_a_dx, function(var) sum(df_unido[[var]], na.rm = TRUE)),
  row.names = NULL
)

originales <- c(
  "dx_Pregnancy_Childbirth", "dx_Diseases_of_the_Blood", "dx_Circulatory", "dx_Congenital",
  "dx_Digestive", "dx_Endocrine", "dx_Genitourinary", "dx_Infectious",
  "dx_Injury_Poisoning", "dx_Musculoskeletal", "dx_Neoplasms", "dx_Nervous_System",
  "dx_Certain_Perinatal_Conditions", "dx_Psychoses_Neuroses", "dx_Respiratory", "dx_Skin",
  "dx_Symptoms_Signs_NEC", "dx_Births", "dx_Birth_Defects", "dx_Cancer",
  "dx_Ear", "dx_Eye", "dx_Residual", "dx_Factors_Influencing_Health_St"
)

espanol <- c(
  "Embarazo y parto", "Enfermedades de la sangre", "Enfermedades cardiovasculares", "Enfermedades congénitas",
  "Afecciones digestivas", "Trastornos endocrinos", "Enfermedades genitourinarias", "Enfermedades infecciosas",
  "Lesiones y envenenamientos", "Trastornos musculoesqueléticos", "Neoplasias", "Enfermedades del sistema nervioso",
  "Afecciones perinatales", "Trastornos mentales", "Enfermedades respiratorias", "Enfermedades de la piel",
  "Síntomas inespecíficos", "Nacimientos", "Defectos congénitos", "Cáncer",
  "Enfermedades del oído", "Enfermedades del ojo", "Otras enfermedades", "Factores que influyen en la salud"
)

reemplazos <- setNames(espanol, originales)
diagnosticos_urg$diagnosticos <- recode(diagnosticos_urg$diagnosticos, !!!reemplazos)



originales_adm <- c(
  "a_dx_Pregnancy_Childbirth", "a_dx_Diseases_of_the_Blood", "a_dx_Circulatory", "a_dx_Digestive",
  "a_dx_Endocrine", "a_dx_Genitourinary", "a_dx_Infectious", "a_dx_Injury_Poisoning",
  "a_dx_Musculoskeletal", "a_dx_Neoplasms", "a_dx_Nervous_System", "a_dx_Psychoses_Neuroses",
  "a_dx_Respiratory", "a_dx_Skin", "a_dx_Symptoms_Signs_NEC", "a_dx_births",
  "a_dx_Congenital", "a_dx_Certain_Perinatal_Condition", "a_dx_Birth_Defects", "a_dx_Cancer",
  "a_dx_Ear", "a_dx_Eye", "a_dx_Factors_Influencing_Health_"
)

espanol_adm <- c(
  "Embarazo y parto", "Enfermedades de la sangre", "Enfermedades circulatorias", "Afecciones digestivas",
  "Trastornos endocrinos", "Enfermedades genitourinarias", "Enfermedades infecciosas", "Lesiones y envenenamientos",
  "Trastornos musculoesqueléticos", "Neoplasias", "Enfermedades del sistema nervioso", "Trastornos mentales",
  "Enfermedades respiratorias", "Enfermedades de la piel", "Síntomas inespecíficos", "Nacimientos",
  "Enfermedades congénitas", "Afecciones perinatales", "Defectos congénitos", "Cáncer",
  "Enfermedades del oído", "Enfermedades del ojo", "Factores que influyen en la salud"
)
reemplazos_adm <- setNames(espanol_adm, originales_adm)
diagnosticos_adm$diagnosticos <- recode(diagnosticos_adm$diagnosticos, !!!reemplazos_adm)


write.xlsx(diagnosticos_urg,"diagnosticos_urg.xlsx")
write.xlsx(diagnosticos_adm,"diagnosticos_adm.xlsx")


# Gráfico heterogeneidad ----------------------------------------------------------------------------------
plotmeans(ER_TRAFFIC_TOT ~ FAC_NO, data= df_unido, 
          main= "Heterogeneidad entre hospitales en urgencias", 
          xlab = "ID Hospital", ylab = "Nº Pacientes",
          n.label = FALSE)

plotmeans(ER_TRAFFIC_TOT ~ YEAR, data= df_unido, 
          main= "Heterogeneidad entre hospitales en urgencias", 
          xlab = "Año", ylab = "Nº Pacientes", 
          n.label = FALSE)
