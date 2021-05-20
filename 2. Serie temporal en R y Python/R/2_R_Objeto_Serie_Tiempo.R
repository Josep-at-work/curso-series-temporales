### Objeto serie de tiempo en R

# Creamos unos datos
mydata = runif(n = 50, min = 10, max = 45) # 50 valores entre 10 y 45

# ts es la clase "time series"
# Vamos a poner que empieza en 1956 - y con una frecuencia de 4 observaciones por año (cuatrimestres)
mytimeseries = ts(data = mydata, 
                  start = 1956, frequency = 4) # frequency = observaciones por año

# Veamos el gráfico de la serie
plot(mytimeseries)

# Clase
class(mytimeseries)

# Tiempos
time(mytimeseries)

# Redefiniendo el inicio "start"
mytimeseries = ts(data = mydata, 
                  start = c(1956,3), frequency = 4) # En este caso la serie empezaria en el mes 3 del 1956.




### Plots 
# Nottem dataset
# Serie de tiempo que contiene el promedio de temperaturas en el castillo de Nottingham en grados Fahrenheit durante 20 a?os.

# Est?ndar
plot(nottem) 


# Otros paquetes
library(forecast)
library(ggplot2)


autoplot((nottem))

autoplot(nottem) + ggtitle("Autoplot of Nottingham temperature data")




### Datos faltantes y Outliers 
## Import ts.Rmissing.csv

setwd("~/Developer/GitHub/Courses/curso-series-temporales/2. Serie temporal en R y Python/R")

mydata = read.csv('Rmissing.csv')
# mydata = Rmissing # Rmissing se ha importado directamente con la url del archivo en raw que se encuentra en el github

# Convertir la segunda columna en una serie de tiempo sin especificar frecuencia
myts = ts(mydata$mydata)
myts

# Comprobar si hay NAs y outliers
summary(myts)
plot(myts) 

# En el gráfico podemos ver los 5 missing values, además de 4 outliers.

# Usando zoo para localizar y rellenar valores faltantes
library(zoo)
myts.NAlocf = na.locf(myts) #LOCF: last observation carried forward (copia la última observacion antes del NA)
myts.NAfill = na.fill(myts, 33) #rellena con el valor que le pongamos

# Detección automática de outliers con la librería forecast
library(forecast)
myts1 = tsoutliers(myts) # Detecta y sugiere una sustitución del outlier.
myts1
plot(myts)

# Tambi?n hay un m?todo para NA en el paquete forecast
myts.NAinterp = na.interp(myts) #rellena NA con interpolaci?n

# Limpiando NAs y outliers con tsclean del paquete forecast 
mytsclean = tsclean(myts)
plot(mytsclean)
summary(mytsclean)







