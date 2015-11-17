#-*- coding: UTF-8 -*-
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
delt: dif. en días entre t0 y t
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
Se obtiene la información de la página quandl.
usdmxn : dataframe de precios
"""
url = "https://www.quandl.com/api/v3/datasets/BNP/USDMXN.csv?start_date="+t0
usdmxn  = pd.read_csv(url).sort("Date")
rend = usdmxn["USD/MXN"][0:len(usdmxn)]
# usdmxn["USD/MXN"][1:len(usdmxn)]/usdmxn["USD/MXN"][0:(len(usdmxn)-1)]
usdmxn["media"] = usdmxn["USD/MXN"][0]
print(usdmxn[["Date","USD/MXN"]][0:5])

# GRÁFICA
"""
Se realiza la gráfica de serie de tiempo para la paridad USD-MXN

"""
fig = plt.figure()             # se define una figura
axes = fig.add_subplot(111)    # se define que se trabajará con una subfig
axes.plot(range(0,len(usdmxn["Date"])),usdmxn["USD/MXN"])
axes.set_title("Tipo de cambio - USD/MXN")
axes.set_xlabel("Periodo de tiempo")
axes.set_ylabel("Pesos")

plt.show() 
