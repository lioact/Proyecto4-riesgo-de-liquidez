# Importar librerías
pip install pandas numpy scikit-learn scipy matplotlib
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.preprocessing import StandardScaler
from sklearn.linear_model import RidgeCV
from sklearn.metrics import r2_score
from scipy.stats import spearmanr
import warnings

warnings.filterwarnings("ignore")


from google.colab import drive
drive.mount('/content/drive')

# Cargar variables macroeconómicos
ruta_macro = '/content/drive/MyDrive/Colab Notebooks/Regresión Ridge/df_series.xlsx'
datos = pd.read_excel(ruta_macro, sheet_name="Datos")

# Cargar datos bancarios
ruta_bancos = '/content/drive/MyDrive/Colab Notebooks/Regresión Ridge/Proyecto 4 Cuentas de Captación 2024.xlsx'
def cargar_filtrar(sheet_name):
    df = pd.read_excel(ruta_bancos, sheet_name=sheet_name)
    df['Fecha'] = pd.to_datetime(df['Fecha'])
    return df[(df['Fecha'] >= "2019-12-01") & (df['Fecha'] <= "2024-06-01")]



