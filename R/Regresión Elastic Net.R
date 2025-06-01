
# Librerías y carga de datos
library(readxl)
library(caret)
library(corrplot)
library(goftest)
library(psych)
library(dplyr)
library(glmnet)
library(ggplot2)
library(tidyr)

df_series <- read_excel("Datos/df_series.xlsx", sheet = "Datos")

Proyecto_4_Cuentas_de_Captación_2024 <- "Datos/Proyecto 4 Cuentas de Captación 2024.xlsx"

# Función para cargar y filtrar fechas
cargar_filtrar <- function(sheet_name) {
  read_excel(Proyecto_4_Cuentas_de_Captación_2024, sheet = sheet_name) %>%
    mutate(Fecha = as.Date(Fecha)) %>%
    filter(Fecha >= as.Date("2019-12-01") & Fecha <= as.Date("2024-06-01"))
}

# Cargar hojas
TBM <- cargar_filtrar("TBM")
BX <- cargar_filtrar("Banamex")
BBVA <- cargar_filtrar("BBVA")
SA <- cargar_filtrar("Santander")
BN <- cargar_filtrar("Banorte")

# Unir con variables macro
TBM2 <- left_join(TBM, df_series, by = "Fecha")
BX2 <- left_join(BX, df_series, by = "Fecha")
BBVA2 <- left_join(BBVA, df_series, by = "Fecha")
SA2 <- left_join(SA, df_series, by = "Fecha")
BN2 <- left_join(BN, df_series, by = "Fecha")

# Crear lista de bancos
bancos <- list(
  Banxico = TBM2,
  Banamex = BX2,
  BBVA = BBVA2,
  Santander = SA2,
  Banorte = BN2
)

# Variables
variables_macro <- colnames(df_series)[-1]
variables_captacion <- c("Captación tradicional", "Depositos Plazo", "Depositos Vista")

# Función del modelo ajustado
ajustar_y_analizar_elastic_net <- function(df, banco, variable_respuesta, variables_predictoras, alpha = 0.5) {
  df <- df %>% select(all_of(c("Fecha", variable_respuesta, variables_predictoras))) %>% na.omit()
  x <- scale(as.matrix(df[, variables_predictoras]))
  y <- as.matrix(df[[variable_respuesta]])
  
  if (is.null(y) || var(y) == 0) return(NULL)
  
  modelo <- glmnet(x, y, alpha = alpha, lambda = cv.glmnet(x, y, alpha = alpha)$lambda.min)
  y_hat <- predict(modelo, newx = x)
  
  df_plot <- data.frame(Fecha = df$Fecha,
                        Valor = c(as.numeric(y), as.numeric(y_hat)),
                        Tipo = rep(c("Real", "Estimado"), each = nrow(df)),
                        Variable = variable_respuesta)
  return(df_plot)
}

# Lista para almacenar resultados por banco
resultados_banco <- list()

for (banco in names(bancos)) {
  df_banco <- bancos[[banco]]
  graficas_var <- list()
  
  for (var_resp in variables_captacion) {
    if (var_resp %in% colnames(df_banco)) {
      df_plot <- ajustar_y_analizar_elastic_net(df_banco, banco, var_resp, variables_macro)
      if (!is.null(df_plot)) {
        graficas_var[[var_resp]] <- df_plot
      }
    }
  }
  
  # Unir los 3 dataframes de variables
  resultados_banco[[banco]] <- bind_rows(graficas_var)
}

# Generar gráfico final por banco
for (banco in names(resultados_banco)) {
  df_plot <- resultados_banco[[banco]]
  g <- ggplot(df_plot, aes(x = Fecha, y = Valor, color = Variable, linetype = Tipo)) +
    geom_line(size = 1) +
    labs(title = paste("Modelo Elastic Net -", banco, "(3 variables de captación)"),
         y = "Valor", x = "Fecha", color = "Variable", linetype = "Tipo") +
    theme_minimal()
  
  print(g)
}