import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LassoCV
from sklearn.preprocessing import StandardScaler
from scipy.stats import spearmanr
from pathlib import Path

# Leer archivo macroeconómico
df_series = pd.read_excel("Datos/df_series.xlsx", sheet_name="Datos")

# Ruta del archivo bancario
ruta_bancaria = Path("Datos/Proyecto 4 Cuentas de Captación 2024.xlsx")

# Función para cargar y filtrar por fechas
def cargar_filtrar(sheet_name):
    df = pd.read_excel(ruta_bancaria, sheet_name=sheet_name)
    df["Fecha"] = pd.to_datetime(df["Fecha"])
    return df[(df["Fecha"] >= "2019-12-01") & (df["Fecha"] <= "2024-06-01")]

# Cargar y unir datos
TBM2 = pd.merge(cargar_filtrar("TBM"), df_series, on="Fecha", how="left")
BX2 = pd.merge(cargar_filtrar("Banamex"), df_series, on="Fecha", how="left")
BBVA2 = pd.merge(cargar_filtrar("BBVA"), df_series, on="Fecha", how="left")
SA2 = pd.merge(cargar_filtrar("Santander"), df_series, on="Fecha", how="left")
BN2 = pd.merge(cargar_filtrar("Banorte"), df_series, on="Fecha", how="left")

# Función para ajustar y analizar modelo Lasso
def ajustar_y_analizar_lasso(df, banco, variable_respuesta, variables_predictoras):
    print(f"\n\nModelo Lasso: {banco} - {variable_respuesta}")

    cols = ["Fecha", variable_respuesta] + variables_predictoras
    df_filtrado = df[cols].dropna()

    if df_filtrado[variable_respuesta].nunique() <= 1:
        print(f"Variable {variable_respuesta} inválida (constante o nula)")
        return None

    X_raw = df_filtrado[variables_predictoras].values
    y = df_filtrado[variable_respuesta].values

    scaler = StandardScaler()
    X_scaled = scaler.fit_transform(X_raw)

    # ⚠️ Aumentamos el número de iteraciones para evitar problemas de convergencia
    lasso_cv = LassoCV(cv=5, max_iter=10000, n_alphas=1000).fit(X_scaled, y)
    y_pred = lasso_cv.predict(X_scaled)

    r2 = lasso_cv.score(X_scaled, y)
    print(f"\nR²: {round(r2, 4)}")
    print(f"Lambda óptimo: {round(lasso_cv.alpha_, 6)}")

    coef = pd.Series(lasso_cv.coef_, index=variables_predictoras)
    print("\nCoeficientes:")
    print(coef)

    # Prueba de Spearman
    rho, pval = spearmanr(y, y_pred)
    print("\nPrueba de Spearman:")
    print(f"rho = {rho:.4f}, p-valor = {pval:.4f}")

    # Interpretación económica
    print("\nInterpretación económica:")
    for var in variables_predictoras:
        beta = coef[var]
        if beta > 0:
            sentido = "positiva"
        elif beta < 0:
            sentido = "negativa"
        else:
            sentido = "nula"
        print(f"{var}: relación {sentido}")

    # Gráfico
    df_plot = pd.DataFrame({
        "Fecha": df_filtrado["Fecha"],
        "Real": y,
        "Estimado": y_pred
    })

    plt.figure(figsize=(10, 5))
    sns.lineplot(data=df_plot, x="Fecha", y="Real", label="Real")
    sns.lineplot(data=df_plot, x="Fecha", y="Estimado", label="Estimado", linestyle="--")
    plt.title(f"Modelo Lasso - {banco} - {variable_respuesta}")
    plt.xlabel("Fecha")
    plt.ylabel(variable_respuesta)
    plt.legend()
    plt.grid(True)
    plt.tight_layout()
    plt.show()

    return {"modelo": lasso_cv, "r2": r2, "coef": coef, "spearman": (rho, pval)}

# Definir variables
variables_captacion = ["Depositos Vista", "Depositos Plazo", "Captación tradicional"]
variables_macro = ["TIIE", "FIX", "Base_Monetaria", "Remesas_Familiares", "INPC",
                   "Costo_Captacion", "Agregados_Monetarios", "Activos_Financieros"]

bancos = {
    "TBM": TBM2,
    "Banamex": BX2,
    "BBVA": BBVA2,
    "Santander": SA2,
    "Banorte": BN2
}

# Ejecutar el análisis Lasso para cada banco y variable
for banco, df_banco in bancos.items():
    for var_resp in variables_captacion:
        if var_resp in df_banco.columns:
            ajustar_y_analizar_lasso(df_banco, banco, var_resp, variables_macro)

