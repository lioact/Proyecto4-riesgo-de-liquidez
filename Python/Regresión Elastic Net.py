import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import ElasticNetCV
from sklearn.preprocessing import StandardScaler
from scipy.stats import spearmanr
from pathlib import Path

# ================================
# Cargar datos
# ================================
df_series = pd.read_excel("Datos/df_series.xlsx", sheet_name="Datos")
proyecto_path = Path("Datos/Proyecto 4 Cuentas de Captación 2024.xlsx")

def cargar_filtrar(sheet_name):
    df = pd.read_excel(proyecto_path, sheet_name=sheet_name)
    df["Fecha"] = pd.to_datetime(df["Fecha"])
    return df[(df["Fecha"] >= "2019-12-01") & (df["Fecha"] <= "2024-06-01")]

TBM = cargar_filtrar("TBM")
BX = cargar_filtrar("Banamex")
BBVA = cargar_filtrar("BBVA")
SA = cargar_filtrar("Santander")
BN = cargar_filtrar("Banorte")

# Merge con variables macro
TBM2 = pd.merge(TBM, df_series, on="Fecha", how="left")
BX2 = pd.merge(BX, df_series, on="Fecha", how="left")
BBVA2 = pd.merge(BBVA, df_series, on="Fecha", how="left")
SA2 = pd.merge(SA, df_series, on="Fecha", how="left")
BN2 = pd.merge(BN, df_series, on="Fecha", how="left")

bancos = {
    "Banxico": TBM2,
    "Banamex": BX2,
    "BBVA": BBVA2,
    "Santander": SA2,
    "Banorte": BN2
}

variables_macro = df_series.columns.drop("Fecha").tolist()
variables_captacion = ["Captación tradicional", "Depositos Plazo", "Depositos Vista"]

# ================================
# Función principal
# ================================
def ajustar_y_analizar(df, banco, y_var, x_vars, l1_ratio=0.5):
    df = df[["Fecha", y_var] + x_vars].dropna()
    if df.empty or df[y_var].var() == 0:
        print(f"Datos insuficientes o varianza nula para {banco} - {y_var}")
        return None

    X = df[x_vars].values
    y = df[y_var].values

    # Escalar solo X, no y
    scaler = StandardScaler()
    X_scaled = scaler.fit_transform(X)

    # Definir rango alphas similar a glmnet de R
    alphas_range = np.logspace(-4, 1, 100)

    # Ajuste con ElasticNetCV
    model_cv = ElasticNetCV(l1_ratio=l1_ratio, alphas=alphas_range, cv=5, random_state=42, max_iter=10000)
    model_cv.fit(X_scaled, y)

    y_hat = model_cv.predict(X_scaled)

    ss_total = np.sum((y - np.mean(y)) ** 2)

    ss_residual = np.sum((y - y_hat) ** 2)
    r2 = 1 - ss_residual / ss_totals

    spearman_corr, spearman_p = spearmanr(y, y_hat)

    print(f"\n--- {banco} - {y_var} ---")
    print(f"R²: {r2:.4f}")
    print(f"Lambda óptimo (alpha): {model_cv.alpha_:.6f}")
    print("Coeficientes:")
    for var, coef in zip(x_vars, model_cv.coef_):
        print(f"  {var}: {coef:.6f}")
    print("Prueba Spearman:")
    print(f"  Coeficiente: {spearman_corr:.4f}, p-valor: {spearman_p:.4g}")
    print("Interpretación económica:")
    for var, coef in zip(x_vars, model_cv.coef_):
        sentido = "positiva" if coef > 0 else "negativa" if coef < 0 else "nula"
        print(f"  {var}: relación {sentido}")

    df_plot = pd.DataFrame({
        "Fecha": list(df["Fecha"]) + list(df["Fecha"]),
        "Valor": np.concatenate([y, y_hat]),
        "Tipo": ["Real"] * len(y) + ["Estimado"] * len(y),
        "Variable": [y_var] * len(y) * 2
    })

    return df_plot


# ================================
# Ajuste de modelos por banco
# ================================
sns.set(style="whitegrid")
resultados = {}

for banco, df_banco in bancos.items():
    graficas = []
    for var in variables_captacion:
        if var in df_banco.columns:
            df_plot = ajustar_y_analizar(df_banco, banco, var, variables_macro)
            if df_plot is not None:
                graficas.append(df_plot)
    if graficas:
        resultados[banco] = pd.concat(graficas, ignore_index=True)

# ================================
# Mostrar gráficos por banco
# ================================
for banco, df_plot in resultados.items():
    print(f"\n====== GRÁFICA BANCO: {banco} ======")
    input("Presiona ENTER para mostrar la gráfica...")
    plt.figure(figsize=(10, 6))
    sns.lineplot(data=df_plot, x="Fecha", y="Valor", hue="Variable", style="Tipo", linewidth=2)
    plt.title(f"Modelo Elastic Net - {banco} (3 variables de captación)")
    plt.xlabel("Fecha")
    plt.ylabel("Valor")
    plt.xticks(rotation=45)
    plt.legend(title="Variable / Tipo")
    plt.tight_layout()
    plt.show()

input("\nFin del script. Presiona ENTER para cerrar...")
