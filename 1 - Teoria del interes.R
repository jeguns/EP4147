
# ================================== #
# Principios de Finanzas: Capítulo 1 #
# ================================== #

# Usted realiza el préstamo de un capital de 10 mil soles al 15% anual. Luego de 4 meses, ¿cuál es el interés que recibirá por el préstamo?

vf.simple = function(C,i,t){
  return(C*(1+i*t))
} 
C  = 10000
i  = 0.15
t  = 4/12
VF = vf.simple(C,i,t)
I  = VF - C
I

# Patricia presta a Abraham un monto de 5000 soles al 4% trimestral. Luego de 5 meses y 10 días, ¿cuál es el monto total que Abraham debe devolver a Patricia?

vf.simple(C = 5000, i = 0.04, t = 5*1/3 + 10*1/90)

# Una persona aplicó 120 soles y luego de 230 días obtuvo 150 soles. Determine la tasa de interés diaria considerada en este caso.

i.simple = function(C,t,vf){
  return((vf/C-1)/t)
}
i.simple(C = 120, t = 230, vf = 150)*100

# ¿Cuál sería la tasa de interés semestral en el caso anterior?

i.simple(C = 120, t = 230*1/180, vf = 150)*100

# Un capital aplicado durante n meses rindió el 25% de su valor inicial, al considerar una tasa de 2.5% mensual. ¿Cuántos meses duró este préstamo?

t.simple = function(C, i, vf){
  return(((vf/C)-1)/i)
}
t.simple(C = 2, i = 0.025, vf = 1.25*2)

t.simple(C = 100, i = 0.025, vf = 1.25*100)

# Un joven recibe una herencia de 20 mil soles. Parte de ese capital fue impuesto a un interés simple de 6% anual y el resto a 4.5%. Este último monto genera, en un año, un interés de 60 soles más que el primero. ¿Qué cantidad impuso a cada tanto por ciento?

X = matrix(c(0.06,1,-0.045,1),ncol=2)
X
Y = c(-60,20000)
Y
solve(t(X)%*%X)%*%(t(X)%*%Y)

# Un capital de 250 mil soles fue aplicado a un interés compuesto de 3% mensual durante año y medio. ¿Cuál es el interés obtenido?

library(FinCal)
Cf = fv.simple(r = 0.03, n = 18, pv = -250000)
I  = Cf - 250000
I

# Calcule la tasa de interés compuesto anual que se ha aplicado a un capital de 50 mil soles para que al cabo de 4 años se convierta en 75346.56 soles.

vf = 75346.56
c  = 50000.00
t  = 4
i  = (vf/c)^(1/t)-1
i*100

fv.simple(r = i, n = 4, pv = -50000)

# Una aseguradora ofrece a Alberto la siguiente indemnización ante el robo de un vehículo: 20 mil soles ahora o 21211.92 soles dentro de 45 días. ¿Cuál es la tasa de interés capitalizable mensualmente utilizada por la aseguradora?

Cf = 21211.92
C  = 20000.00
t  = 1.5
i  = (Cf/C)^(1/t)-1
i

fv.simple(r = i, n = 1.5, pv = -20000)

# Una inversión duplica su valor en 18 meses a una determinada tasa de interés compuesto. ¿En cuánto tiempo lo triplicará?

i = 2^(1/18)-1
i
t = log(3)/log(1+i)
t
fv.simple(r = i, n = 18, pv = -1000)
fv.simple(r = i, n = t, pv = -1000)
fv.simple(r = i, n = 18, pv = -20000)
fv.simple(r = i, n = t, pv = -20000)

# Un banco ofrece una TEA de 0.05% al momento de ahorar en una Cuenta Simple. Determine el monto que debe ser invertido para retirar 15000 soles dentro de 45 meses.

library(FinCal)
pv.simple(r = 0.0005, n = 45/12, fv = -15000)

im = (1+0.0005)^(1/12)-1
im*100 # ¿es equivalente o propocional a la TEA de 0.05%?

pv.simple(r = im, n = 45, fv = -15000)

# ¿Qué capital debe ser invertido para ganar 500 soles luego de año y medio a una tasa nominal anual de 3% capitalizable mensualmente?

tem = 0.03/12
tem

C = 500/((1+tem)^18-1)
C

# Se desea hacer un retiro de 1000 soles de una cuenta de ahorros el 1 de abril del 2018, 2000 soles el 1 de abril del 2019 y 500 soles el 1 de abril del 2021. ¿Cuánto dinero debe ser depositado el 1 de abril del 2017 si esta cuenta gana un interés de 0.75% anual?

C = 1000/1.0075 + 2000/1.0075^2 + 500/1.0075^4
año1 = fv.simple(r=0.0075, n=1, pv=-C) - 1000
año1
año2 = fv.simple(r=0.0075, n=1, pv=-año1) - 2000
año2
año3 = fv.simple(r=0.0075, n=2, pv=-año2) - 500
año3

# Se desea hacer un retiro de 1000 soles de una cuenta de ahorros el 1 de abril del 2018, 2000 soles el 1 de abril del 2019 y 500 soles el 1 de abril del 2021. Además de ello, se tiene programado realizar un depósito de 100 soles cada 1 de abril desde el 2018 hasta el 2021, excepto el año 2020, en el que se planea depositar 200 soles. ¿Cuánto dinero debe ser depositado el 1 de abril del 2017 si esta cuenta gana un interés de 0.75% anual?

C = 1000/1.0075 + 2000/1.0075^2 + 500/1.0075^4 - 100/1.0075 - 100/1.0075^2 - 200/1.0075^3 - 100/1.0075^4
C
año1 = fv.simple(r=0.0075, n=1, pv=-C) - 1000 + 100
año1
año2 = fv.simple(r=0.0075, n=1, pv=-año1) - 2000 + 100 
año2
año3 = fv.simple(r=0.0075, n=1, pv=-año2) + 200
año3
año4 = fv.simple(r=0.0075, n=1, pv=-año3) - 500 + 100
año4

# ¿Cuál es la equivalencia anual de una tasa de interés compuesto de 1% mensual?

(1.01^12-1)*100

# ¿Cuál es la equivalencia mensual de una tasa de interés compuesto de 1% anual?

(1.01^(1/12)-1)*100

# ¿Cuál es la equivalencia trimestral de una tasa de interés compuesto de 3% bimestral?

(1.03^(3/2)-1)*100

# ¿Cuál es la tasa efectiva anual de una tasa nominal anual de 24% capitalizable mensualmente?
  
library(lifecontingencies)
nominal2Real(i = 0.24, k = 12)

# La tasa nominal anual de 24% capitalizable mensualmente es proporcional a una tasa nominal mensual de 2% capitalizable mensualmente, es decir a una tasa efectiva mensual de 2%, la cual es pasada a TEA:
# TEA=(1+0.02)^12−1=0.2682418

# ¿Cuál es la tasa efectiva anual de una tasa nominal anual de 24% capitalizable trimestralmente?

nominal2Real(i = 0.24, k = 4)

# ¿Cuál es la tasa efectiva anual de una tasa nominal anual de 24% capitalizable semestralmente?

nominal2Real(i = 0.24, k = 2)


# ¿Cuál es la TEA equivalente a una TNA de 6% capitalizable trimestralmente?

nominal2Real(i = 0.06, k = 4)

# ¿Cuál es la tasa efectiva bimestral equivalente a una tasa nominal semestral de 18% capitalizable mensualmente?
# La tasa nominal semestral de 18% capitalizable mensualmente es proporcional a una nominal anual de 36% capitalizable mensualmente.

ia = nominal2Real(i = 0.36, k = 12)
ia

# La TEA de 42.57609% es equivalente a una Tasa Efectiva Bimestral
# 1+0.4257609=(1+ib)^6
# Entonces:
# is=1.4257609^(1/6)−1

ib = (1+ia)^(1/6)-1
ib

# Un capital de 10458 soles es aplicado a una tasa nominal anual de 1.2%, capitalizable bimestralmente ¿Cuál es el monto del capital final luego de un año?

ia = nominal2Real(i = 0.012, k = 6)
ia

library(FinCal)
fv.simple(r = ia, n = 1, pv = -10458)

# ¿A qué tasa nominal anual capitalizable trimestralmente equivale una tasa efectiva anual del 46.41%?

real2Nominal(i = 0.4641, k = 4)

  
  

