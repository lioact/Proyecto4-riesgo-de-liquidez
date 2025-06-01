library(readxl)
library(dplyr)
library(ggplot2)
library(glmnet)
library(tidyr)


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
ajustar_y_analizar_lasso_completo <- function(df, banco, variables_respuesta, variables_predictoras) {
  cat("\n\nAnálisis completo para:", banco, "\n")
  
  df_plot_all <- data.frame(Fecha = df$Fecha)
  resultados <- list()
  
  for (var_resp in variables_respuesta) {
    cat("\n--- Variable:", var_resp, "---\n")
    
    df_filtrado <- df %>%
      dplyr::select(all_of(c("Fecha", var_resp, variables_predictoras))) %>%
      na.omit()
    
    y <- as.numeric(df_filtrado[[var_resp]])
    
    if (is.null(y) || var(y) == 0) {
      cat("Variable", var_resp, "inválida (nula o constante)\n")
      next
    }
    
    x <- scale(as.matrix(df_filtrado[, variables_predictoras]))
    
    # Ajuste Lasso
    cv_lambda <- cv.glmnet(x, y, alpha = 1)
    lambda_opt <- cv_lambda$lambda.min
    modelo <- glmnet(x, y, alpha = 1, lambda = lambda_opt)
    y_hat <- predict(modelo, newx = x)
    
    # R2
    ss_total <- sum((y - mean(y))^2)
    ss_residual <- sum((y - y_hat)^2)
    r2 <- 1 - (ss_residual / ss_total)
    
    # Spearman
    prueba_spearman <- cor.test(y, y_hat, method = "spearman")
    
    # Coeficientes
    coef_lasso <- coef(modelo)
    
    cat("R²:", round(r2, 4), "\n")
    cat("Lambda óptimo:", round(lambda_opt, 6), "\n")
    cat("Coeficientes:\n")
    print(coef_lasso)
    
    cat("Prueba Spearman:\n")
    print(prueba_spearman)
    
    cat("Interpretación económica:\n")
    for (v in variables_predictoras) {
      beta <- coef_lasso[v, 1]
      sentido <- ifelse(beta > 0, "positiva", ifelse(beta < 0, "negativa", "nula"))
      cat(v, ": relación", sentido, "\n")
    }
    
    # Agregar al dataframe de graficación
    df_nuevo <- data.frame(
      Fecha = df_filtrado$Fecha
    )
    df_nuevo[[paste0(var_resp, "_Real")]] <- y
    df_nuevo[[paste0(var_resp, "_Estimado")]] <- as.numeric(y_hat)
    
    df_plot_all <- merge(df_plot_all, df_nuevo, by = "Fecha", all.x = TRUE)
    
    # Guardar resultados
    resultados[[var_resp]] <- list(modelo = modelo, r2 = r2, coef = coef_lasso, prueba = prueba_spearman)
  }
  
  # 📈 Gráfico combinado
  df_long <- df_plot_all %>%
    pivot_longer(-Fecha, names_to = "Serie", values_to = "Valor")
  
  g <- ggplot(df_long, aes(x = Fecha, y = Valor, color = Serie, linetype = grepl("Estimado", Serie))) +
    geom_line(size = 1) +
    scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed")) +
    labs(title = paste("Modelo Lasso -", banco, "(3 variables de captación)"),
         x = "Fecha", y = "Valor", color = "Serie", linetype = "") +
    theme_minimal()
  
  print(g)
  
  return(resultados)
}




for (banco in names(bancos)) {
  df_banco <- bancos[[banco]]
  ajustar_y_analizar_lasso_completo(df_banco, banco, variables_captacion, variables_macro)
}
