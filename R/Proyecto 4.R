
# Librerias
library(readxl)
library(dplyr)

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

