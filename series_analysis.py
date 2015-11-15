#-*- coding: cp1252 -*-
__author__ = 'Rodrigo'

import datetime as dat
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd

# TIEMPO
"""
Se definen las variables de tiempo.
t0  : tiempo inicial
t   : tiempo final
delt: dif. en d�as entre t0 y t
"""
t_fina = dat.datetime.today()
t_inic = t_fina
delt = dat.timedelta(days = 2*365)
t = t_fina.strftime('%Y-%m-%d')
t_inic -= delt
t0 = t_inic.strftime('%Y-%m-%d')
print(t0)
print(t)

# DATOS
"""
Se obtiene la informaci�n de la p�gina quandl.
usdmxn : dataframe de precios
"""
url = "https://www.quandl.com/api/v3/datasets/BNP/USDMXN.csv?start_date="+t0
usdmxn  = pd.read_csv(url).sort("Date")
print(usdmxn[["Date","USD/MXN"]][0:5])

# GR�FICA
"""
Se realiza la gr�fica de serie de tiempo para la paridad USD-MXN

"""
fig = plt.figure()             # se define una figura
axes = fig.add_subplot(111)    # se define que se trabajar� con una subfig
axes.plot(range(0,len(usdmxn["Date"])),usdmxn["USD/MXN"])
axes.set_title("Tipo de cambio - USD/MXN")
axes.set_xlabel("Periodo de tiempo")
axes.set_ylabel("Pesos")

plt.show() 