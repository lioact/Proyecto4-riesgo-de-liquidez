
# Cargar librerias
library(readxl)
library(corrplot)
library(goftest)
library(psych)
library(dplyr)
library(glmnet)
library(ggplot2)

# Variables macroeconomicas
ruta <- "~/Administración integral de riesgos/Proyecto4-riesgo-de-liquidez/Datos/df_series.xlsx"
datos <- read_excel(ruta, sheet = "Datos")

# Datos bancos
rutab <- "~/Administración integral de riesgos/Proyecto4-riesgo-de-liquidez/Datos/Proyecto 4 Cuentas de Captación 2024.xlsx"

# Función para cargar y filtrar por fechas
cargar_filtrar <- function(sheet_name) {
  read_excel(rutab, sheet = sheet_name) %>%
    mutate(Fecha = as.Date(Fecha)) %>%
    filter(Fecha >= as.Date("2019-12-01") & Fecha <= as.Date("2024-06-01"))
}

# Cargar datos de cada hoja 
TBM <- cargar_filtrar("TBM")
BX <- cargar_filtrar("Banamex")
BBVA <- cargar_filtrar("BBVA")
SA <- cargar_filtrar("Santander")
BN <- cargar_filtrar("Banorte")

# Unimos las variables macro con las bancarias por Fecha
TBM_join <- left_join(TBM, datos, by = "Fecha")
BX_join <- left_join(BX, datos, by = "Fecha")
BBVA_join <- left_join(BBVA, datos, by = "Fecha")
SA_join <- left_join(SA, datos, by = "Fecha")
BN_join <- left_join(BN, datos, by = "Fecha")

ajustar_y_analizar_ridge <- function(df, banco, variable_respuesta, variables_predictoras) {
  cat("\n\n📌 Modelo Ridge:", banco, "-", variable_respuesta, "\n")
  
  # Filtrado
  df <- df %>% select(all_of(c("Fecha", variable_respuesta, variables_predictoras))) %>% na.omit()
  
  x <- as.matrix(df[, variables_predictoras])
  y <- as.matrix(df[[variable_respuesta]])
  
  if (is.null(y) || var(y) == 0) {
    cat("❌ Variable", variable_respuesta, "inválida (NULL o constante)\n")
    return(NULL)
  }
  
  # Ridge: alpha = 0
  cv_lambda <- cv.glmnet(x, y, alpha = 0)
  lambda_opt <- cv_lambda$lambda.min
  modelo <- glmnet(x, y, alpha = 0, lambda = lambda_opt)
  
  # Coeficientes
  coef_ridge <- coef(modelo)
  print(coef_ridge)
  
  # R2
  y_hat <- predict(modelo, newx = x)
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum((y - y_hat)^2)
  r2 <- 1 - (ss_residual / ss_total)
  cat("\n📈 R²:", round(r2, 4), "\n")
  cat("📍 Lambda óptimo:", round(lambda_opt, 6), "\n")
  
  # Prueba no paramétrica (Spearman)
  prueba_spearman <- cor.test(as.numeric(y), as.numeric(y_hat), method = "spearman")
  cat("\n🔎 Prueba de Spearman:\n")
  print(prueba_spearman)
  
  # Relación económica simple (signo de los coef)
  cat("\n🧠 Interpretación económica:\n")
  for (v in variables_predictoras) {
    beta <- coef_ridge[v, 1]
    sentido <- ifelse(beta > 0, "↑ positiva", ifelse(beta < 0, "↓ negativa", "– nula"))
    cat(v, ": relación", sentido, "\n")
  }
  
  # Gráfico
  df_plot <- data.frame(Fecha = df$Fecha, Real = as.numeric(y), Estimado = as.numeric(y_hat))
  g <- ggplot(df_plot, aes(x = Fecha)) +
    geom_line(aes(y = Real, color = "Real")) +
    geom_line(aes(y = Estimado, color = "Estimado"), linetype = "dashed") +
    labs(title = paste("Modelo Ridge -", banco, "-", variable_respuesta),
         y = variable_respuesta, x = "Fecha", color = "Serie") +
    theme_minimal()
  
  print(g)  # <- Esto fuerza la impresión del gráfico
  
  
  # Retorno opcional por si lo necesitas guardar
  return(list(modelo = modelo, r2 = r2, coef = coef_ridge, prueba = prueba_spearman))
}


bancos <- list(TBM = TBM_join, Banamex = BX_join, BBVA = BBVA_join, Santander = SA_join, Banorte = BN_join)

variables_captacion <- c("Depositos Vista", "Depositos Plazo", "Captación tradicional")
variables_macro <- c("TIIE", "FIX", "Base_Monetaria", "Remesas_Familiares", "INPC",
                     "Costo_Captacion", "Agregados_Monetarios", "Activos_Financieros")

for (banco in names(bancos)) {
  df_banco <- bancos[[banco]]
  
  for (var_resp in variables_captacion) {
    if (var_resp %in% colnames(df_banco)) {
      ajustar_y_analizar_ridge(df_banco, banco, var_resp, variables_macro)
    }
  }
}







































