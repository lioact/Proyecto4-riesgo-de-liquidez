
# Cargar librerias
library(readxl)
library(corrplot)
library(goftest)
library(psych)
library(dplyr)
library(glmnet)
library(ggplot2)

# Variables macroeconómicas
df_series <- read_excel("Datos/df_series.xlsx", sheet = "Datos")

# Datos bancos
Proyecto_4_Cuentas_de_Captación_2024 <- "Datos/Proyecto 4 Cuentas de Captación 2024.xlsx"

# Función para cargar y filtrar por fechas
cargar_filtrar <- function(sheet_name) {
  read_excel(Proyecto_4_Cuentas_de_Captación_2024, sheet = sheet_name) %>%
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
TBM2 <- left_join(TBM, df_series, by = "Fecha")
BX2 <- left_join(BX, df_series, by = "Fecha")
BBVA2 <- left_join(BBVA, df_series, by = "Fecha")
SA2 <- left_join(SA, df_series, by = "Fecha")
BN2 <- left_join(BN, df_series, by = "Fecha")

ajustar_y_analizar_ridge <- function(df, banco, variable_respuesta, variables_predictoras) {
  cat("\n\n Modelo Ridge:", banco, "-", variable_respuesta, "\n")
  
  df <- df %>% select(all_of(c("Fecha", variable_respuesta, variables_predictoras))) %>% na.omit()
  
  x <- as.matrix(df[, variables_predictoras])
  y <- as.matrix(df[[variable_respuesta]])
  
  if (is.null(y) || var(y) == 0) {
    cat("Variable", variable_respuesta, "inválida (NULL o constante)\n")
    return(NULL)
  }
  
  cv_lambda <- cv.glmnet(x, y, alpha = 0)
  lambda_opt <- cv_lambda$lambda.min
  modelo <- glmnet(x, y, alpha = 0, lambda = lambda_opt)
  
  y_hat <- predict(modelo, newx = x)
  
  # Para graficar más tarde
  df_plot <- data.frame(
    Fecha = df$Fecha,
    Serie = rep(variable_respuesta, nrow(df)),
    Real = as.numeric(y),
    Estimado = as.numeric(y_hat)
  )
  
  return(df_plot)
}

for (banco in names(bancos)) {
  df_banco <- bancos[[banco]]
  df_graficas <- data.frame()  # Para guardar todos los datos
  
  for (var_resp in variables_captacion) {
    if (var_resp %in% colnames(df_banco)) {
      df_r <- ajustar_y_analizar_ridge(df_banco, banco, var_resp, variables_macro)
      if (!is.null(df_r)) {
        df_graficas <- bind_rows(df_graficas, df_r)
      }
    }
  }
  
  # Crear gráfico combinado
  if (nrow(df_graficas) > 0) {
    df_graficas_long <- df_graficas %>%
      tidyr::pivot_longer(cols = c("Real", "Estimado"), names_to = "Tipo", values_to = "Valor")
    
    g <- ggplot(df_graficas_long, aes(x = Fecha, y = Valor, color = Tipo, linetype = Tipo)) +
      geom_line() +
      facet_wrap(~Serie, scales = "free_y", ncol = 1) +
      labs(title = paste("Modelo Ridge -", banco),
           x = "Fecha", y = "Captación", color = "Serie", linetype = "Serie") +
      theme_minimal()
    
    print(g)
  }
}

