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
def ajustar_y_analizar_lasso(df, banco, variables_respuesta, variables_predictoras):
    print(f"\n\nAnálisis completo para: {banco}")

    df_plot_all = pd.DataFrame({"Fecha": df["Fecha"]})
    resultados = {}

    plt.figure(figsize=(14, 7))

    for var_resp in variables_respuesta:
        print(f"\n\n--- Variable: {var_resp} ---")

        cols = ["Fecha", var_resp] + variables_predictoras
        df_filtrado = df[cols].dropna()

        if df_filtrado[var_resp].nunique() <= 1:
            print(f"Variable {var_resp} inválida (constante o nula)")
            continue

        X_raw = df_filtrado[variables_predictoras].values
        y = df_filtrado[var_resp].values

        scaler = StandardScaler()
        X_scaled = scaler.fit_transform(X_raw)

        lasso_cv = LassoCV(cv=5, max_iter=10000, n_alphas=1000).fit(X_scaled, y)
        y_pred = lasso_cv.predict(X_scaled)

        r2 = lasso_cv.score(X_scaled, y)
        coef = pd.Series(lasso_cv.coef_, index=variables_predictoras)

        rho, pval = spearmanr(y, y_pred)

        print(f"R²: {round(r2, 4)}")
        print(f"Lambda óptimo: {round(lasso_cv.alpha_, 6)}")
        print("Coeficientes:")
        print(coef)

        print("Prueba de Spearman:")
        print(f"rho = {rho:.4f}, p-valor = {pval:.4f}")

        print("Interpretación económica:")
        for var in variables_predictoras:
            beta = coef[var]
            sentido = "positiva" if beta > 0 else "negativa" if beta < 0 else "nula"
            print(f"{var}: relación {sentido}")

        # Guardar para graficar todo junto
        df_plot_all = df_plot_all.merge(df_filtrado[["Fecha"]].assign(**{
            f"{var_resp}_Real": y,
            f"{var_resp}_Estimado": y_pred
        }), on="Fecha", how="left")

        resultados[var_resp] = {
            "modelo": lasso_cv,
            "r2": r2,
            "coef": coef,
            "spearman": (rho, pval)
        }

        # Gráfico combinado
        sns.lineplot(x=df_filtrado["Fecha"], y=y, label=f"{var_resp} Real")
        sns.lineplot(x=df_filtrado["Fecha"], y=y_pred, label=f"{var_resp} Estimado", linestyle="--")

    plt.title(f"Modelo Lasso - {banco} (3 variables de captación)")
    plt.xlabel("Fecha")
    plt.ylabel("Valor")
    plt.legend()
    plt.grid(True)
    plt.tight_layout()
    plt.show()

    return resultados


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

for banco, df_banco in bancos.items():
    ajustar_y_analizar_lasso(df_banco, banco, variables_captacion, variables_macro)




























