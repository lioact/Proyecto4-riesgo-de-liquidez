library(readxl)
library(dplyr)
library(ggplot2)
library(glmnet)

# Leer archivo macroeconómico
df_series <- read_excel("Datos/df_series.xlsx", sheet = "Datos")

# Ruta del archivo bancario
Proyecto_4_Cuentas_de_Captación_2024 <- "Datos/Proyecto 4 Cuentas de Captación 2024.xlsx"

# Función para cargar y filtrar por fechas
cargar_filtrar <- function(sheet_name) {
  read_excel(Proyecto_4_Cuentas_de_Captación_2024, sheet = sheet_name) %>%
    mutate(Fecha = as.Date(Fecha)) %>%
    filter(Fecha >= as.Date("2019-12-01") & Fecha <= as.Date("2024-06-01"))
}

# Cargar y unir datos
TBM2 <- left_join(cargar_filtrar("TBM"), df_series, by = "Fecha")
BX2 <- left_join(cargar_filtrar("Banamex"), df_series, by = "Fecha")
BBVA2 <- left_join(cargar_filtrar("BBVA"), df_series, by = "Fecha")
SA2 <- left_join(cargar_filtrar("Santander"), df_series, by = "Fecha")
BN2 <- left_join(cargar_filtrar("Banorte"), df_series, by = "Fecha")

# Función para ajustar y analizar modelo Lasso
ajustar_y_analizar_lasso <- function(df, banco, variable_respuesta, variables_predictoras) {
  cat("\n\nModelo Lasso:", banco, "-", variable_respuesta, "\n")
  
  # Filtrado de variables y NA
  df <- df %>% select(all_of(c("Fecha", variable_respuesta, variables_predictoras))) %>% na.omit()
  
  # Estandarización de predictores
  x_raw <- as.matrix(df[, variables_predictoras])
  x <- scale(x_raw)
  
  # Variable respuesta
  y <- as.matrix(df[[variable_respuesta]])
  
  if (is.null(y) || var(y) == 0) {
    cat("Variable", variable_respuesta, "inválida (NULL o constante)\n")
    return(NULL)
  }
  
  # Modelo de Lasso: alpha = 1
  cv_lambda <- cv.glmnet(x, y, alpha = 1)
  lambda_opt <- cv_lambda$lambda.min
  modelo <- glmnet(x, y, alpha = 1, lambda = lambda_opt)
  
  # Coeficientes
  coef_lasso <- coef(modelo)
  print(coef_lasso)
  
  # R²
  y_hat <- predict(modelo, newx = x)
  ss_total <- sum((y - mean(y))^2)
  ss_residual <- sum((y - y_hat)^2)
  r2 <- 1 - (ss_residual / ss_total)
  cat("\nR²:", round(r2, 4), "\n")
  cat("Lambda óptimo:", round(lambda_opt, 6), "\n")
  
  # Prueba no paramétrica (Spearman)
  prueba_spearman <- cor.test(as.numeric(y), as.numeric(y_hat), method = "spearman")
  cat("\nPrueba de Spearman:\n")
  print(prueba_spearman)
  
  # Interpretación económica
  cat("\nInterpretación económica:\n")
  for (v in variables_predictoras) {
    beta <- coef_lasso[v, 1]
    sentido <- ifelse(beta > 0, " positiva", ifelse(beta < 0, " negativa", " nula"))
    cat(v, ": relación", sentido, "\n")
  }
  
  # Gráfico
  df_plot <- data.frame(Fecha = df$Fecha, Real = as.numeric(y), Estimado = as.numeric(y_hat))
  g <- ggplot(df_plot, aes(x = Fecha)) +
    geom_line(aes(y = Real, color = "Real")) +
    geom_line(aes(y = Estimado, color = "Estimado"), linetype = "dashed") +
    labs(title = paste("Modelo Lasso -", banco, "-", variable_respuesta),
         y = variable_respuesta, x = "Fecha", color = "Serie") +
    theme_minimal()
  
  print(g)
  
  return(list(modelo = modelo, r2 = r2, coef = coef_lasso, prueba = prueba_spearman))
}

# Definir variables
variables_captacion <- c("Depositos Vista", "Depositos Plazo", "Captación tradicional")
variables_macro <- c("TIIE", "FIX", "Base_Monetaria", "Remesas_Familiares", "INPC",
                     "Costo_Captacion", "Agregados_Monetarios", "Activos_Financieros")

bancos <- list(TBM = TBM2, Banamex = BX2, BBVA = BBVA2, Santander = SA2, Banorte = BN2)

# Ejecutar el análisis Lasso para cada banco y variable
for (banco in names(bancos)) {
  df_banco <- bancos[[banco]]
  
  for (var_resp in variables_captacion) {
    if (var_resp %in% colnames(df_banco)) {
      ajustar_y_analizar_lasso(df_banco, banco, var_resp, variables_macro)
    }
  }
}
