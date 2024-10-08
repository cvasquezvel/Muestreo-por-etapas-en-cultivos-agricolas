setwd("D:/cvasquez/Muestreo")
library(readxl)
Arandano <- read_excel("Datos conteo.xlsx")
attach(Arandano)
# is.na(`Bayas Maduras`) <- 0
# Arandano$`Bayas para cosecha` <- `Bayas iniciando a Cremas`+`Bayas Cremas`+`Bayas Maduras`

table(Semana)
table(A�o, Semana)
table(A�o, Semana, M�dulo)

# summarytools::descr(Semana22)

library(psych)
multi.hist(Arandano[,c(15,16,17,18,22)])

Censo <- read.csv("Censo.csv",header = T)

### margen de error para M.A.S.
library(raster)
library(dplyr)
MuesSem <- Arandano %>%
  dplyr::filter(Semana %in% "22")
# ndef real es 1470
# ndef real es 10800
x = MuesSem$`Bayas para cosecha`
is.na(x) <- 0
# x = as.numeric(as.vector(MuesSem$`Bayas iniciando a Cremas`+MuesSem$`Bayas Cremas`)) #  + MuesSem$`Bayas iniciando a Cremas`
x = na.omit(x)
n = length(x) # tama�o de muestra previo
n
N = nrow(Censo) # tama�o de la poblaci�n censada
N
var = var(x) # variancia del vector
var
sd = sd(x) # desviaci�n est�ndar del vector
sd
alfa = c(0.1,0.05,0.025,0.01) # nivel de significancia
alfa

meanest = mean(x) # Media de la muestra preliminar
meanest
varest = (var/n)*(1-(n/N)) # variancia del promedio estimado
varest
sdest = sqrt(varest) # desviaci�n est�ndar del promedio estimado
sdest

d = qt((1-(alfa/2)),n-1)*sdest # Error de la estimaci�n de la media. Se emplea una distribuci�n de student (t) debido a que la variancia de la poblaci�n no es conocida
d

propd <-  d/meanest #Raz�n de error estimado en base a la media estimada
propd

# dsquare = qnorm(1-alfa/2)*(var/n)*((N-n)/N)
# dsquare = (qt(1-alfa/2,n-1)^2)*(var/n)*((N-n)/N)
# d = sqrt(dsquare)
# d = qt(1-alfa/2,n-1)*sqrt((var/n)*((N-n)/N))
# d = qt(1-alfa/2,n-1)*sdest

# Para estimar promedios cuando la variancia es conocida

perr = c(0.1, 0.05, 0.025, 0.01) # proporci�n de error deseado
d.deseado = meanest*perr # Error deseado de la estimaci�n de la media
d.deseado

no = ((qt((1-(alfa/2)),n-1)*sd)/d.deseado[1])^2 # Tama�o de muestra preliminar con proporci�n de error de 0.1

ndef = no/(1+(no/N)) # Tama�o de muestra corregida
round(ndef,0) # Redondeo

# Considerar max 5400

no = ((qt((1-(alfa/2)),n-1)*sd)/d.deseado[2])^2 # Tama�o de muestra preliminar con proporci�n de error de 0.05

ndef = no/(1+(no/N)) # Tama�o de muestra corregida
round(ndef,0) # Redondeo

no = ((qt((1-(alfa/2)),n-1)*sd)/d.deseado[3])^2 # Tama�o de muestra preliminar con proporci�n de error de 0.025

ndef = no/(1+(no/N)) # Tama�o de muestra corregida
round(ndef,0) # Redondeo

no = ((qt((1-(alfa/2)),n-1)*sd)/d.deseado[4])^2 # Tama�o de muestra preliminar con proporci�n de error de 0.01

ndef = no/(1+(no/N)) # Tama�o de muestra corregida
round(ndef,0) # Redondeo

# Tama�o de muestra en base al coeficiente de variaci�n deseado del estimador

cv = cv(x)/100 # Coeficiente de variaci�n del vector
cv
cvo = c(0.1, 0.05, 0.025, 0.01) # Coeficiente de variaci�n esperado

no = (cv/cvo)^2 # Tama�o de muestra preliminar

ndef = no/(1+(no/N)) # Tama�o de muestra corregido
round(ndef,0) # Redondeo

### Anexo

semana = as.vector(levels(as.factor(Arandano$Semana)))
semana

# MuesSem <- Arandano %>%
#   dplyr::filter(Semana %in% "22")

colnames(Arandano)
str(Arandano)

library(tibble)

muestreo <- function(datos,sem){
  b <- length(sem)
  MuesSem <- c()
  for(i in 1:b){
    MuesSem[i] <-   datos %>%
      dplyr::filter(Semana %in% sem[i]) %>%
      split(sem[i])
  }
  return(MuesSem)
}

muestrasem <- muestreo(Arandano,semana)
muestrasem

MuesSem <-   Arandano %>%
  dplyr::filter(Semana %in% semana) %>%
  split(Arandano$Semana) %>%
  enframe

nrow(MuesSem)

ncal <- function(datos, censo, alfa, prop.err){
  r <- nrow(datos)
  ndef = c()
  for (i in 1:r) {
    x = datos[[2]][[i]]$`Bayas para cosecha`
    n = length(x) # tama�o de muestra previo
    N = nrow(censo) # tama�o de la poblaci�n censada
    var = var(x) # variancia del vector
    sd = sd(x) # desviaci�n est�ndar del vector
    meanest = mean(x)
    d.deseado = meanest*prop.err # Error deseado de la estimaci�n de la media
    no = ((qt((1-(alfa/2)),n-1)*sd)/d.deseado)^2 # Tama�o de muestra preliminar con proporci�n de error de 0.1
    ndef[i] = round(no/(1+(no/N)),0) # Tama�o de muestra corregida
  }
  return(ndef)
}

ncal1 <- ncal(MuesSem, Censo, 0.05, 0.1)
ncal2 <- ncal(MuesSem, Censo, 0.05, 0.05)
ncal3 <- ncal(MuesSem, Censo, 0.05, 0.025)
ncal4 <- ncal(MuesSem, Censo, 0.05, 0.01)
ncal5 <- ncal(MuesSem, Censo, 0.05, 0.005)
ncal6 <- ncal(MuesSem, Censo, 0.05, 0.001)

tapply(Arandano$`Bayas para cosecha`,Arandano$Semana,length)

options(scipen = "99")

MAS <- data.frame(x = semana,
                  y = ncal3)
plot(MAS$x[-c(1,2,3)],MAS$y[-c(1,2,3)], type = "h",
     xlab = "Semana",
     ylab = "Tama�o de muestra",
     main = "Tama�o de muestra semanal con
0.05 de proporci�n de error")

boxplot(MAS$y[-c(1,2)])
median(MAS$y[-c(1,2)])
quantile(MAS$y[-c(1,2)],0.75)


boxplot(MAS$y[-c(1,2,3,4,5,6)])
median(MAS$y[-c(1,2,3,4,5,6)])
quantile(MAS$y[-c(1,2,3,4,5,6)],0.75)

### Selecci�n de muestra

Censo$PesoMAS <- n/nrow(Censo)

# 3500 plantas
n = 3500
muestra <- Censo[sample(nrow(Censo),n,F),]

library(dplyr)

#Muestra sin reemplazo
muestra2<- Censo %>%
  sample_n(size=n,replace=FALSE)

head(muestra2)

#Muestra con pesos
muestra3<- Censo %>%
  sample_n(size=n,weight=PesoMAS)

head(muestra3)

#Muestra con una proporci�n de casos
muestra4<- Censo %>%
  sample_frac(0.0008321004)

head(muestra4);dim(muestra4)



