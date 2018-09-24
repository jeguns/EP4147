
# ================================== #
# Principios de Finanzas: Capítulo 2 #
# ================================== #

# 1. Considerando una tasa efectiva anual de $6.17\%$, determine el valor presente de una anualidad de:
# a. Un pago anticipado de 1200 soles

im = 1.0617^(1/12)-1
im
library(FinCal)
pv.annuity(r = im, n = 1, pmt = -1200, type = 1) # type = 1 : pago anticipado


# b. 2 pagos mensuales anticipados de 1200 soles cada uno

pv.annuity(r = im, n = 2, pmt = -1200, type = 1)
1200+1200/(1+im)

# c. 10 pagos mensuales anticipados de 1200 soles cada uno

pv.annuity(r = im, n = 10, pmt = -1200, type = 1)

# 2. Una empresa consigue un financiamiento de 15 mil soles, el cual será liquidado durante 10 meses 
# al inicio de cada uno de estos, a una tasa de 2% mensual. ¿Cuál es el valor de cada prestación?

pmt(r = 0.02, n = 10, pv = -15000, fv = 0, type = 1) # pago anticipado


# 3. Una persona invierte 2800 soles al inicio de cada año, durante 2 años. Considerando una TEA de 1%, ¿Cuál es el monto final resultante de la inversión?

fv.annuity(r = 0.01, n = 2, pmt = -2800, type = 1) # anticipado

2800*(1+0.01)^2 + 2800*(1+0.01)

# 4. Asuma el caso anterior pero los pagos se realizan durante 15 años. Determine el valor futuro.

fv.annuity(r = 0.01, n = 15, pmt = -2800, type = 1) 

# 5. Una persona realiza un depósito de 5000 soles durante todos los meses de diciembre durante desde el año 2001 hasta el año 2016, a un tasa de interés del 6% anual. Decide retirar su dinero en diciembre de 2017. ¿Cuál será el monto a ser retirado?

fv.annuity(r = 0.06, n = 16, pmt = -5000, type = 1) 

S = 0
for(i in 1:16){  S = S + 1.06^i}
5000*S




# 3. Considerando una tasa efectiva anual de 6.17%, determine el valor presente de una anualidad de:

# a. Un pago vencido de 1200 soles

pv.annuity(r = im, n = 1, pmt = -1200, type = 0) # type = 0 : pago vencido

1200/(1+im)

# b. 2 pagos mensuales vencidos de 1200 soles cada uno

pv.annuity(r = im, n = 2, pmt = -1200, type = 0)

1200/(1+im) + 1200/(1+im)^2

# c. 10 pagos mensuales vencidos de 1200 soles cada uno

pv.annuity(r = im, n = 10, pmt = -1200, type = 0)

# 4. Una empresa consigue un financiamiento de 15 mil soles, el cual será liquidado durante 10 meses al final de cada uno de éstos, a una tasa de 2% mensual. ¿Cuál es el valor de cada prestación?

pmt(r = 0.02, n = 10, pv = -15000, fv = 0, type = 0)

# 8. En el ejercicio anterior, ¿Cuál sería el monto a ser retirado en el año 2016?

fv.annuity(r = 0.06, n = 16, pmt = -5000, type = 0) 

S = 1
for(i in 1:15){  S = S + 1.06^i}
5000*S







