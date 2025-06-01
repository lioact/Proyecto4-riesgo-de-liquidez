import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path
from sklearn.metrics import r2_score
from scipy.stats import spearmanr
from itertools import combinations
import matplotlib.dates as mdates


# Cargar datos macro y de bancos
df_series = pd.read_excel("Datos/df_series.xlsx", sheet_name="Datos")

# Función para cargar y filtrar por fechas
def cargar_filtrar(path, sheet_name):
    df = pd.read_excel(path, sheet_name=sheet_name)
    df['Fecha'] = pd.to_datetime(df['Fecha'])
    return df[(df['Fecha'] >= '2019-12-01') & (df['Fecha'] <= '2024-06-01')]

ruta_bancos = Path("Datos/Proyecto 4 Cuentas de Captación 2024.xlsx")
bancos = {
    "TBM": cargar_filtrar(ruta_bancos, "TBM"),
    "Banamex": cargar_filtrar(ruta_bancos, "Banamex"),
    "BBVA": cargar_filtrar(ruta_bancos, "BBVA"),
    "Santander": cargar_filtrar(ruta_bancos, "Santander"),
    "Banorte": cargar_filtrar(ruta_bancos, "Banorte")
}

# Variables
variables_captacion = ["Depositos Vista", "Depositos Plazo", "Captación tradicional"]
variables_macro = df_series.columns.drop("Fecha").tolist()

# Función de selección hacia atrás (backward) similar a stepAIC
def stepwise_selection(X, y, initial_list=[], threshold_out=0.05):
    included = list(X.columns)
    while True:
        model = sm.OLS(y, sm.add_constant(X[included])).fit()
        pvalues = model.pvalues.iloc[1:]  # Excluye intercepto
        worst_pval = pvalues.max()
        if worst_pval > threshold_out:
            worst_feature = pvalues.idxmax()
            included.remove(worst_feature)
        else:
            break
    return included

# Análisis por banco y variable
# Crear listas para almacenar resultados de cada variable
resultados = []

for banco, df_banco in bancos.items():
    print(f"\n\nAnálisis para: {banco}")
    df = pd.merge(df_banco, df_series, on="Fecha", how="left")

    fig, ax = plt.subplots(figsize=(12, 6))

    for var_resp in variables_captacion:
        if var_resp not in df.columns:
            print(f"Variable {var_resp} no encontrada en {banco}")
            continue

        print(f"\n--- Variable: {var_resp} ---")
        columnas = ["Fecha", var_resp] + variables_macro
        df_filtrado = df[columnas].dropna()

        y = df_filtrado[var_resp]
        X = df_filtrado[variables_macro]

        try:
            selected_vars = stepwise_selection(X, y)
            if not selected_vars:
                print("No se seleccionaron variables significativas.")
                continue

            X_selected = sm.add_constant(X[selected_vars])
            model = sm.OLS(y, X_selected).fit()
            y_hat = model.predict(X_selected)

            print(model.summary())

            # Spearman
            corr, pval = spearmanr(y, y_hat)
            print(f"Spearman rho: {corr:.4f}, p-valor: {pval:.4g}")

            # Interpretación económica
            print("Interpretación económica:")
            for v in selected_vars:
                beta = model.params[v]
                sentido = "positiva" if beta > 0 else "negativa" if beta < 0 else "nula"
                print(f"{v}: relación {sentido}")

            # Graficar resultados en un solo gráfico
            ax.plot(df_filtrado["Fecha"], y, label=f"{var_resp}_Real", linewidth=2)
            ax.plot(df_filtrado["Fecha"], y_hat, linestyle='--', label=f"{var_resp}_Estimado")

        except Exception as e:
            print(f"Error en el modelo para {var_resp} en {banco}: {e}")

    # Ajustes generales del gráfico del banco
    ax.set_title(f"Modelo Lasso - {banco} (3 variables de captación)", fontsize=14)
    ax.set_xlabel("Fecha")
    ax.set_ylabel("Valor")
    ax.legend(title="Serie", loc="upper left", fontsize=9)
    ax.grid(True)
    ax.xaxis.set_major_formatter(mdates.DateFormatter("%Y"))
    ax.xaxis.set_major_locator(mdates.YearLocator())
    plt.tight_layout()
    plt.show()

