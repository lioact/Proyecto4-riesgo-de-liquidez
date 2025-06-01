library(readxl)
library(dplyr)
library(ggplot2)
library(tidyr)
library(MASS)       # Para stepAIC
library(janitor)    # Para clean_names
library(psych)      # Para describe y control adicional

# Leer archivo macroeconómico
df_series <- read_excel("Datos/df_series.xlsx", sheet = "Datos") %>%
  clean_names()

# Ruta del archivo bancario
archivo_bancos <- "Datos/Proyecto 4 Cuentas de Captación 2024.xlsx"

# Función para cargar y limpiar
cargar_filtrar <- function(sheet_name) {
  read_excel(archivo_bancos, sheet = sheet_name) %>%
    clean_names() %>%
    mutate(fecha = as.Date(fecha)) %>%
    filter(fecha >= as.Date("2019-12-01") & fecha <= as.Date("2024-06-01"))
}

# Cargar bancos y unir con macro
TBM <- left_join(cargar_filtrar("TBM"), df_series, by = "fecha")
BX  <- left_join(cargar_filtrar("Banamex"), df_series, by = "fecha")
BBVA <- left_join(cargar_filtrar("BBVA"), df_series, by = "fecha")
SA   <- left_join(cargar_filtrar("Santander"), df_series, by = "fecha")
BN   <- left_join(cargar_filtrar("Banorte"), df_series, by = "fecha")

# Lista de bancos
bancos <- list(TBM = TBM, Banamex = BX, BBVA = BBVA, Santander = SA, Banorte = BN)

# Variables
variables_macro <- names(df_series)[names(df_series) != "fecha"]
variables_captacion <- c("depositos_vista", "depositos_plazo", "captacion_tradicional")

# Función de análisis automático
ajustar_y_analizar_stepAIC <- function(df, banco, variables_respuesta, variables_predictoras) {
  cat("\n\n📊 Análisis para:", banco, "\n")
  
  df_plot_all <- data.frame(fecha = df$fecha)
  resultados <- list()
  
  for (var_resp in variables_respuesta) {
    cat("\n--- Variable:", var_resp, "---\n")
    
    df_filtrado <- df %>%
      dplyr::select(all_of(c("fecha", var_resp, variables_predictoras))) %>%
      na.omit()
    
    y <- df_filtrado[[var_resp]]
    if (is.null(y) || var(y) == 0) {
      cat("⚠️ Variable", var_resp, "inválida (nula o constante)\n")
      next
    }
    
    # Modelo completo
    formula_inicial <- as.formula(paste("`", var_resp, "` ~ ", paste(variables_predictoras, collapse = " + "), sep = ""))
    modelo_inicial <- lm(formula_inicial, data = df_filtrado)
    
    # Modelo stepwise
    modelo_step <- stepAIC(modelo_inicial, direction = "both", trace = FALSE)
    y_hat <- predict(modelo_step)
    
    # Métricas
    ss_total <- sum((y - mean(y))^2)
    ss_resid <- sum((y - y_hat)^2)
    r2 <- 1 - ss_resid / ss_total
    spearman <- cor.test(y, y_hat, method = "spearman")
    
    cat("R²:", round(r2, 4), "\n")
    cat("Prueba Spearman:\n")
    print(spearman)
    
    cat("Coeficientes del modelo final:\n")
    print(summary(modelo_step)$coefficients)
    
    cat("Interpretación económica:\n")
    coefs <- coef(modelo_step)[-1]  # sin intercepto
    for (v in names(coefs)) {
      sentido <- ifelse(coefs[[v]] > 0, "positiva", ifelse(coefs[[v]] < 0, "negativa", "nula"))
      cat(v, ": relación", sentido, "\n")
    }
    
    # Para gráfico
    df_graf <- df_filtrado %>%
      mutate(Real = y,
             Estimado = y_hat) %>%
      dplyr::select(fecha, Real, Estimado)
    
    colnames(df_graf)[2:3] <- c(paste0(var_resp, "_Real"), paste0(var_resp, "_Estimado"))
    df_plot_all <- merge(df_plot_all, df_graf, by = "fecha", all.x = TRUE)
    
    # Guardar resultados
    resultados[[var_resp]] <- list(modelo = modelo_step, r2 = r2, spearman = spearman)
  }
  
  # 📈 Gráfico combinado
  df_long <- df_plot_all %>%
    pivot_longer(-fecha, names_to = "Serie", values_to = "Valor")
  
  g <- ggplot(df_long, aes(x = fecha, y = Valor, color = Serie, linetype = grepl("Estimado", Serie))) +
    geom_line(size = 1) +
    scale_linetype_manual(values = c("FALSE" = "solid", "TRUE" = "dashed")) +
    labs(title = paste("Regresión Lineal Múltiple -", banco),
         x = "Fecha", y = "Valor", color = "Serie", linetype = "") +
    theme_minimal()
  
  print(g)
  return(resultados)
}

# Ejecutar para todos los bancos
for (banco in names(bancos)) {
  df_banco <- bancos[[banco]]
  ajustar_y_analizar_stepAIC(df_banco, banco, variables_captacion, variables_macro)
}
