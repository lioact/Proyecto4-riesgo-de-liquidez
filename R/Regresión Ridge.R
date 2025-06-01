library(readxl)
library(dplyr)
library(ggplot2)
library(glmnet)
library(tidyr)

# Leer datos
df_series <- read_excel("Datos/df_series.xlsx", sheet = "Datos")
archivo_bancos <- "Datos/Proyecto 4 Cuentas de Captación 2024.xlsx"

# Función para cargar y filtrar
cargar_filtrar <- function(sheet_name) {
  read_excel(archivo_bancos, sheet = sheet_name) %>%
    mutate(Fecha = as.Date(Fecha)) %>%
    filter(Fecha >= as.Date("2019-12-01") & Fecha <= as.Date("2024-06-01"))
}

# Cargar bancos
TBM2     <- left_join(cargar_filtrar("TBM"), df_series, by = "Fecha")
BX2      <- left_join(cargar_filtrar("Banamex"), df_series, by = "Fecha")
BBVA2    <- left_join(cargar_filtrar("BBVA"), df_series, by = "Fecha")
SA2      <- left_join(cargar_filtrar("Santander"), df_series, by = "Fecha")
BN2      <- left_join(cargar_filtrar("Banorte"), df_series, by = "Fecha")

# Variables
variables_captacion <- c("Depositos Vista", "Depositos Plazo", "Captación tradicional")
variables_macro <- c("TIIE", "FIX", "Base_Monetaria", "Remesas_Familiares", "INPC",
                     "Costo_Captacion", "Agregados_Monetarios", "Activos_Financieros")

bancos <- list(TBM = TBM2, Banamex = BX2, BBVA = BBVA2, Santander = SA2, Banorte = BN2)

# Función principal para Ridge
ajustar_y_graficar_ridge <- function(df_banco, banco, variables_captacion, variables_macro) {
  df_plot_all <- data.frame()
  
  for (variable_respuesta in variables_captacion) {
    if (!(variable_respuesta %in% colnames(df_banco))) next
    
    df <- df_banco %>% select(all_of(c("Fecha", variable_respuesta, variables_macro))) %>% na.omit()
    if (nrow(df) == 0) next
    
    x <- scale(as.matrix(df[, variables_macro]))
    y <- as.matrix(df[[variable_respuesta]])
    
    if (is.null(y) || var(y) == 0) {
      cat("Variable", variable_respuesta, "inválida (NULL o constante)\n")
      next
    }
    
    # Ridge: alpha = 0
    cv_lambda <- cv.glmnet(x, y, alpha = 0)
    lambda_opt <- cv_lambda$lambda.min
    modelo <- glmnet(x, y, alpha = 0, lambda = lambda_opt)
    y_hat <- predict(modelo, newx = x)
    
    # Evaluación
    ss_total <- sum((y - mean(y))^2)
    ss_residual <- sum((y - y_hat)^2)
    r2 <- 1 - (ss_residual / ss_total)
    prueba_spearman <- cor.test(as.numeric(y), as.numeric(y_hat), method = "spearman")
    coef_ridge <- coef(modelo)
    
    # Resultados en consola
    cat("\n---", banco, "-", variable_respuesta, "---\n")
    cat("R²:", round(r2, 4), "\n")
    cat("Lambda óptimo:", round(lambda_opt, 6), "\n")
    cat("Coeficientes:\n"); print(coef_ridge)
    cat("Prueba Spearman:\n"); print(prueba_spearman)
    cat("Interpretación económica:\n")
    for (v in variables_macro) {
      beta <- coef_ridge[v, 1]
      sentido <- ifelse(beta > 0, "positiva", ifelse(beta < 0, "negativa", "nula"))
      cat(v, ": relación", sentido, "\n")
    }
    
    # Datos para gráfico
    df_result <- data.frame(
      Fecha = df$Fecha,
      Variable = variable_respuesta,
      Real = as.numeric(y),
      Estimado = as.numeric(y_hat)
    )
    df_plot_all <- bind_rows(df_plot_all, df_result)
  }
  
  # Gráfico combinado
  df_long <- df_plot_all %>%
    pivot_longer(cols = c("Real", "Estimado"), names_to = "Tipo", values_to = "Valor") %>%
    mutate(Serie = paste(Variable, Tipo, sep = "_"))
  
  g <- ggplot(df_long, aes(x = Fecha, y = Valor, color = Serie, linetype = Tipo)) +
    geom_line(size = 1) +
    labs(title = paste("Regresión Ridge --", banco),
         x = "Fecha", y = "Valor", color = "Serie", linetype = "") +
    theme_minimal()
  
  print(g)
}

# Ejecutar para todos los bancos
for (banco in names(bancos)) {
  ajustar_y_graficar_ridge(bancos[[banco]], banco, variables_captacion, variables_macro)
}
