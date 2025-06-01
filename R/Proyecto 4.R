
# Librerias
library(readxl)
library(dplyr)
library(siebanxicor)
library(tidyr)
library(httr)
library(jsonlite)
library(lubridate)

# Carga de datos 
ruta <- "Proyecto 4 Cuentas de Captación 2024.xlsx"

# Función para cargar y filtrar por fechas
cargar_filtrar <- function(sheet_name) {
  read_excel(ruta, sheet = sheet_name) %>%
    mutate(Fecha = as.Date(Fecha)) %>%
    filter(Fecha >= as.Date("2019-12-01") & Fecha <= as.Date("2024-06-01"))
}

# Cargar datos de cada hoja 
TBM <- cargar_filtrar("TBM")
BX <- cargar_filtrar("Banamex")
BBVA <- cargar_filtrar("BBVA")
SA <- cargar_filtrar("Santander")
BN <- cargar_filtrar("Banorte")

# CONECTAR CON BANXICO
  
#Token de consulta
setToken("3ee8adc24b480b5f402ac5c93882120368adb7d4d3c4f67895fbe5e8b0470a15")

#Series elegidas
"TIIE a 28 días Tasa de interés promedio mensual, en por ciento anual"
"Tipo de cambio Pesos por dólar E.U.A. Para solventar obligaciones denominadas en moneda extranjera Fecha de determinación (FIX) Cotizaciones promedio"
"Base Monetaria"
"Remesas Familiares Total"
"Índice Nacional de Precios al consumidor Variación mensual"
"Costo de captación a plazo de pasivos en moneda nacional (CCP) Tasa en por ciento anual"
"Agregados Monetarios M2 Instrumentos monetarios a plazo en poder de residentes 4/ Captación a plazo En otras instituciones no bancarias Entidades de ahorro y crédito popular 3/"
"Activos Financieros Internos F1 Instrumentos no monetarios en poder de residentes Fondos de ahorro para la vivienda y el retiro Retiro En Banco de México"

#Realizar la consulta de las series
idSeries <- c("SF283","SF17908","SF29652","SE27803","SP30577","SF286","SF311428","SF311357")
series <- getSeriesData(idSeries, '2019-12-01','2024-06-01')

#Guardar datos
df_series <- as.data.frame(series)

#Eliminar columnas repetidas de fecha 
df_series <- df_series %>% select(-c(3, 5, 7, 9, 11, 13, 15))

df_series <- df_series %>% 
  rename_with(~ gsub("\\.value", "", .x)) 
colnames(df_series)[1] <- "Date"

#Ordenar columnas
df_series <- df_series %>% select(Date, all_of(idSeries))

#Cambiar nombres de las columnas
colnames(df_series) <- c("Fecha","TIIE","FIX","Base_Monetaria","Remesas_Familiares","INPC","Costo_Captacion","Agregados_Monetarios","Activos_Financieros")

#Exportar los datos a excel
library(writexl)
write_xlsx(df_series, "df_series.xlsx")
write_xlsx(df_series, "C:/Users/fer10/Documents/Administración integral de riesgos/Proyecto4-riesgo-de-liquidez/Datos/df_series.xlsx")
