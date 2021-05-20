#Importando datos de precios de cierre de Starbucks y Microsoft
library(readr)

# sbux.df <- read_csv("A CURSO SERIES TEMPORALES (NUEVO)/Clases Nuevas/Starbuck/sbuxPrices.csv")
# View(sbux.df)

telemetry <- read_csv("~/Developer/Niment/Predictive_Maintenance/Data/telemetry.csv")
View(telemetry)

####
# Para ir en paralelo con el curso (solo una variable) selecciono solo una máquina y uno de las variables.
# En mi caso, en lugar de el dataset de starbucks analizaré las vibraciones
# y en lugar de los datos de microsoft usaré la presión de la pieza.
###

library(dplyr)
telemetry %>% filter(machineID == 1) %>%
   select(c(datetime, vibration)) -> vibration

# sbux.ts = ts(data=sbux.df$Adj.Close, frequency = 12,
#              start=c(1993,3), end=c(2008,3))

###
# Lo más común en objetos ts es representar datos con frecuencia mensual trimestral o anual.
# la variable frecuencia determina cuantas divisiones hace en un año que es la unidad por defecto
# SI forzamos que tenga otra frecuencia a las usuales, perdemos la información temporal que 
# los objetos ts suelen mantener

# Atención!!!! los objetos ts están limitados a series con frecuencia mensual trimestral o anual
# Permiten hacer la división en frec horaria pero no mantiene las características.
###

vib.ts = ts(data = vibration$vibration, frequency = 365*24, #frecuencia horaria.
             start=c(2015), end=c(2016))
class(vib.ts) 


# msft.df <- read_csv("A CURSO SERIES TEMPORALES (NUEVO)/Clases Nuevas/Starbuck/msftPrices.csv")
# View(sbux.df)
telemetry %>% filter(machineID == 1) %>%
   select(c(datetime, pressure)) -> pressure

# msft.ts = ts(data=msft.df$Adj.Close, frequency = 12,
#              start=c(1993,3), end=c(2008,3))

pres.ts = ts(data = pressure$pressure, frequency = 365*24,
             start=c(2015), end=c(2016))

#Fechas y frecuencia de la serie
# sbux.ts 
# start(sbux.ts) 
# end(sbux.ts) 
# frequency(sbux.ts) 
vib.ts 
start(vib.ts) 
end(vib.ts) 
frequency(vib.ts) 

#Subconjunto de la serie de tiempo
# tmp = sbux.ts[1:5] 
tmp = vib.ts[1:5]
class(tmp)

# tmp = window(sbux.ts, start=c(1993, 3), end=c(1993,8))
tmp = window(vib.ts, start = c(2015, 1), end = c(2015,2))
class(tmp)


#Combinando dos series (dos columnas)

# sbuxmsft.ts = cbind(sbux.ts, msft.ts) 
combi.ts = cbind(vib.ts, pres.ts) 
class(combi.ts) 
#mts: multiple time series

#Seleccionando las primeras 5 filas:
# window(sbuxmsft.ts, start=c(1993, 3), end=c(1993,7)) 



#Plot objeto ts
# plot(sbux.ts, col="blue", lwd=2, ylab="Adjusted close",
#      main="Monthly closing price of SBUX") 
plot(vib.ts[1:240], col="blue", lwd=2, type="line", ylab="Adjusted close",
           main="Monthly closing price of SBUX") # 10 dias de valores


#Dibujar un subconjunto (Acercar)
# plot(window(sbux.ts, start=c(2000,3), end=c(2008,3)),
#       ylab="Adjusted close",col="blue", lwd=2,
#       main="Monthly closing price of SBUX") 

plot(window(vib.ts, start=c(2015,0.1), end=c(2015,100)),
            ylab="Adjusted close",col="blue", lwd=2,
            main="Monthly closing price of SBUX") 




#Plot para múltiples columnas
#En gráficos diferentes
# plot(sbuxmsft.ts) 
plot(combi.ts)

#En el mismo gr?fico
plot(sbuxmsft.ts, plot.type="single",
      main="Monthly closing prices on SBUX and MSFT",
      ylab="Adjusted close price",
      col=c("blue", "red"), lty=1:2)
legend(1994, 35, legend=c("SBUX","MSFT"), col=c("blue", "red"),
          lty=1:2)


################### zoo
library(zoo) 

# En caso de que las fechas no esten ya creadas en el dataset: 

#Fecha
td = seq(as.Date("1993/3/1"), as.Date("2008/3/1"), "months") 
class(td) 
head(td) 
tail(td)

#En caso de si tener las fechas:

#Alternativa
# td2 = as.Date(sbux.df$Date, format="%m/%d/%Y") 
# head(td2) 
td = strptime(vibration$datetime, format = "%d/%m/%Y %I:%M:%S %p") # se introducen NAs!
td[4200] #NA
td[6114] #"2015-12-09 23:00:00 CET"
td[6115] #NA

td[6114:8000]
td[8755] # "2016-01-01 CET"

############################################################################ Reflexión ####
# strptime y as.Date se les complica hacer una frecuencia de tiempo horaria
# lubridate va bien, permite mayor precisión hasta de segundos y no da problemas con AM/PM
############################################################################
library(lubridate)
td2 = mdy_hms(vibration$datetime)
head(td2,12)
td2[6114]
td2[6115:6125]


#Combinando el ?ndice de tiempo a las dos series de precios
# sbux.z = zoo(x=sbux.df$Adj.Close, order.by=td) 
# msft.z = zoo(x=msft.df$Adj.Close, order.by=td) 


vib.z = zoo(x = vibration$vibration, order.by = td2)
pres.z = zoo(x = pressure$pressure, order.by = td2)
str(vib.z)
class(pres.z)
head(vib.z,12)

# class(sbux.z) 
# str(sbux.z) 
# head(sbux.z) 

#Extrayendo el indice de tiempo y los datos
# index(sbux.z) 
# coredata(sbux.z) 
index(vib.z)
coredata(vib.z) 

#Start and End
# start(sbux.z) 
# end(sbux.z)
start(vib.z)
end(vib.z) 

#Ventaja de zoo: extraer subconjunto indexando con las fechas
vib.z[as.Date(c("2015/3/1", "2015/3/1"))]  # Not working!!!!!!!

#window() también funciona
# window(sbux.z, start=as.Date("2000/3/1"), end=as.Date("2003/3/1"))
window(vib.z, start = as.Date("2015/3/1 02:00:00"), end = as.Date("2015/3/2 12:00:00"))
# Parece que no entiende de horas, solo llega hasta nivel diario

######################################## Combinando dos series ####
# sbuxmsft.z = cbind(sbux.z, msft.z) 
# class(sbuxmsft.z) 
# head(sbuxmsft.z) 
combo = cbind(vib.z, pres.z)
#unique(index(vib.z))
view(combo) # La frecuencia temporal se ha asignado como indice!

#Plot
plot(vib.z, col="blue", lty=1, lwd=2, ylim=c(0,150), main="Monthly closing prices of SBUX and MFST",
     ylab="Adjusted close price")
lines(pres.z, col="red", lty=2, lwd=2)
legend(x="topleft", legend=c("SBUX","MSFT"), col=c("blue","red"),
         lty=1:2)

#Alternativa, las dos a la vez
plot(combo, plot.type="single", col=c("blue","red"), lty=1:2,
         lwd=2,main="Monthly closing prices of SBUX and MFST",
         ylab="Adjusted close price")
legend(x="topleft", legend=c("SBUX","MSFT"), col=c("blue","red"),
         lty=1:2) 

############################## Alternativa Importar con zoo ####

#Importar datos directamente como objeto zoo

sbux.z2 = read.zoo("A CURSO SERIES TEMPORALES (NUEVO)/Clases Nuevas/Starbuck/sbuxPrices.csv",
                   format="%m/%d/%Y", sep=",", header=T) 


#Importar datos de Yahoo Finance
library(tseries)
SBUX.z = get.hist.quote(instrument="sbux", start="1993-03-01",
                           end="2020-06-01", quote="AdjClose",
                           provider="yahoo", origin="1970-01-01",
                           compression="d", retclass="zoo") 

View(SBUX.z)

MSFT.z = get.hist.quote(instrument="msft", start="1993-03-01",
                        end="2020-06-01", quote="AdjClose",
                        provider="yahoo", origin="1970-01-01",
                        compression="d", retclass="zoo") 

#Plot
plot(cbind(SBUX.z,MSFT.z), plot.type="single", col=c("blue","red"), lty=1:2,
     lwd=2,main="Monthly closing prices of SBUX and MFST",
     ylab="Adjusted close price")
legend(x="topleft", legend=c("SBUX","MSFT"), col=c("blue","red"),
       lty=1:2)



################################################################### dygraphs ####

#Liber?a dygraphs
install.packages("dygraphs")
library(dygraphs)
# dygraph(SBUX.z, "Monthly closing prices of SBUX")
# dygraph(cbind(SBUX.z,MSFT.z), "Monthly closing prices of SBUX and MFST")
dygraph(vib.z)

#############################################################3 Datos diarios 
#Generamos datos aleatorios
datos <- rnorm(78, 0, 10)
fechas <- seq(as.Date("2020-03-06"), as.Date("2020-05-22"), by = "day")
as.numeric(format(fechas[1], "%j")) #  te indica a que "numero del dia del año" corresponde la fecha concreta

miserie.ts<-ts(datos,start=c(2016,66), frequency=365)
plot(miserie.ts)


library(zoo)
miserie.z=zoo(datos, fechas)
plot(miserie.z)
dygraph(miserie.z)




