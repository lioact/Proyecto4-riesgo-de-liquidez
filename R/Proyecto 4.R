
#Librerias
library(readxl)

#Carga de datos 
ruta <- "Proyecto 4 Cuentas de Captación 2024.xlsx"
TBM <- read_excel(ruta, sheet = "TBM")
BX <- read_excel(ruta, sheet = "Banamex")
BBVA <- read_excel(ruta, sheet = "BBVA")
SA <- read_excel(ruta, sheet = "Santander")
BN <- read_excel(ruta, sheet = "Banorte")


