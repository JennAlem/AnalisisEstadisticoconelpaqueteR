

Notas <- c(4.47, 4.47); 
Notas
data.entry(Notas) 
Notas 
length(Notas)

# 4º) Guarda el vector de datos en un archivo.

write(Notas, "Notas.txt")


#5º) Limpia el área de trabajo (Workspace) 
ls()
rm(list=ls(all=TRUE)) 
ls()

#6º) Lee o recupera el vector de datos desde el archivo de texto.

X <- scan("Notas.txt", what = double(0), na.strings = "NA", flush=FALSE)
ls()
# Si el vector contiene valores reales se ocupa: what = double(0)

#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------

#7º) Crea la tabla de frecuencias.
# Define el número k de los intervalos o clases.
# Usa el Método de Herbert A. Sturges para determinar dicho número.
n <- length(X); 
n
k <- 1+3.322*logb(n, 10); 
k 
k <- round(k); 
k


# Calcula el ancho o amplitud a de cada intervalo 
#a=rango/k 
rango <- max(X)-min(X); 
rango

a=rango/k; 
a
a <- round(a, 3); 
a

# Define los límites y puntos medios de cada uno de los 
#k intervalos l
limites <- seq(from=min(X)-0.01/2, to=max(X)+0.01/2, by=a)
limites 
options(digits=4)
ci <- cbind(1:k); 
ci
for(i in 2:length(limites)) ci[i-1, 1] <- (limites[i] + limites[i-1])/2 
ci


# Encuentra las frecuencias absolutas fi para cada intervalo.

options(digits=2)
fi <- cbind(table(cut(X, breaks = limites, labels=NULL, include.lowest=FALSE, right=FALSE, dig.lab=4)))
fi


#---------------------------------------------------------------
#-------------------------NOTAS---------------------------------
#---------------------------------------------------------------

# "breaks" es un vector o secuencia de cortes 1:6, o el número de clases.

# "labels" indica que no hay nombres para los intervalos o clases,
#por defecto las etiquetas tienen la notación (a, b].

# "include.lowest" indica que si un X[i] es igual al corte 
#inferior (0 superior, para right=FALSE) el valor debe ser incluido.

# "right" indica que sí el intervalo debe ser cerrado a la derecha 
#y abierto a la izquierda, o viceversa.

# "dig.lab" es un entero el cual es usado cuando las etiquetas no 
#son dadas, determina el número de dígitos usado en el formato de 
#números de cortes.

#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------


# Encuentra las frecuencias relativas o proporciones fri. options(digits=4)
fri <- fi/n; fri

# Encuentra las frecuencias acumuladas ascendentes Fi options(digits=2)
Fi <- cumsum(fi); Fi

# Encuentra las frecuencias relativas acumuladas Fri options(digits=4)
Fri <- Fi/n; Fri

# Completa la tabla de frecuencias.
tablaFrec <- data.frame(ci=ci, fi=fi, fri=fri, Fi=Fi, Fri=Fri)
tablaFrec
# Nuevamente puede usar el comando xtable para importar a código LATEX.


#8º) Crea el histograma de frecuencias
h <- hist(X, breaks=c(limites[1]-a, limites, limites[k+1]+a), 
          freq = TRUE, probability = FALSE, include.lowest = FALSE,
          right = TRUE, main = "Histograma de frecuencias", 
          col="lightyellow", lty=1, border="purple", 
          xlab=" Notas de aspirantes", ylab="Frecuencia (fi)", 
          axes=TRUE, labels=FALSE)
text(h$mids, h$density, h$counts, adj=c(0.5, -0.5), col="red") 
rug(jitter(X)) # adiciona marcas de los datos

# h es un objeto del tipo lista que contiene atributos del histograma 
is.list(h)
h

#----------------------------------------------------------
#----------------------------------------------------------
#_---------------------------------------------------------


#9º) Aproxima al histograma la función de densidad normal
h <- hist(X, breaks=c(limites[1]-a, limites, limites[k+1]+a), 
          freq = FALSE, probability = TRUE, include.lowest = FALSE, 
          right = TRUE, main="Aproximación a una Normal\n", 
          col="lightyellow",lty=1,border="purple", 
          xlab="Notas de aspirantes\n", 
          ylab="Frecuencia relativa (fri)",
          axes=TRUE, labels=FALSE)

text(h$mids, h$density, h$counts, adj=c(0.5, 0.2), col="red") 

rug(jitter(X)) # adiciona marcas de los datos

curve(dnorm(x, mean=mean(X), sd=sd(X)), col = 2, 
      lty = 2,lwd = 2, add = TRUE)

#----------------------------------------------------------
#----------------------------------------------------------
#_---------------------------------------------------------

#10º) Crea el polígono de frecuencias

h <- hist(X, breaks=c(limites[1]-a, limites, limites[k+1]+a), 
          freq = TRUE, probability=FALSE, include.lowest=FALSE,
          right=TRUE, main = "Polígono de frecuencias",
          col="lightyellow", lty=1, border="purple", 
          xlab=" Notas de aspirantes",	ylab="Frecuencia (fi)",
          axes=TRUE, labels=FALSE)

text(h$mids, h$density, h$counts, adj=c(0.5, -0.5), col="red") 

rug(jitter(X)) # adiciona marcas de los datos

vCi <- c(h$mids[1]-a, h$mids, h$mids[k+1]+a)
vCi 

vfi <- c(0, h$counts, 0); vfi

lines(vCi, vfi, col="blue", type="l")


#----------------------------------------------------------
#----------------------------------------------------------
#_---------------------------------------------------------

#11º) Crea la Ojiva ascendente o polígono de frecuencias 
#acumuladas ascendentes 
Fia <- c(0, Fi)
Fia

plot(limites, Fia, type = "p", pch=1, col = "blue", 
     main="Ojiva ascendente", xlab="Notas de aspirantes", 
     ylab="Frecuencia acumulada (Fi)")

text(limites, h$density, Fia, adj=c(0.5, -0.5), col="red") 

lines(limites, Fia, col="black", type="l")


#----------------------------------------------------------
#----------------------------------------------------------
#_---------------------------------------------------------

#            EL 12 NO SE HARA

#----------------------------------------------------------
#----------------------------------------------------------
#_---------------------------------------------------------


#13º) Otros gráficos: 

# Gráfico de cajas
  boxplot(X, main="Gráfico de caja", xlab="Notas", 
          notch=FALSE, data=parent.frame(), plot=TRUE, 
          border="red", col="yellow",horizontal=TRUE)

#Observación: en la función boxplot(), sí plot es FALSE se 
#produce un resumen de los valores (los cinco números).

# Una variante del boxplot, es el notched boxplot de McGill, 
#Larsen y Tukey, el cual adiciona intervalos de confianza para 
#la mediana, representados con un par de cuñas a los lados 
#de la caja:

windows()
boxplot(X, main="Gráfico de caja", xlab="X = Notas", 
        notch=TRUE, data=parent.frame(), plot=TRUE, 
        border="red", col="yellow",horizontal=TRUE)

# Varios gráficos en una misma ventana

par(mfrow=c(1,2)) # Divide la ventana gráfica en dos partes 
#(1 fila, 2 columnas) 
mtext(side=3, line=0, cex=2, outer=T, "Titulo para Toda la Página")

hist(X)
boxplot(X)

#----------------------------------------------------------
#----------------------------------------------------------
#_---------------------------------------------------------


#  PRACTICA 9 - ANÁLISIS DE DE UNA VARIABLE BIDIMENSIONALCATEGÓRICA

#----------------------------------------------------------
#----------------------------------------------------------
#_---------------------------------------------------------


#Ejemplo:

# Se selecciona aleatoriamente una muestra de 18 personas adultas, 
# para estudiar si existe relación entre su estado civil y su ocupación.


# REALICE UN ANÁLISIS ESTADÍSTICO DE LOS DATOS.
# 1º) Activa tu directorio de trabajo. 

getwd()
setwd("C:/Users/MINEDUCYT/OneDrive/Documentos")

#2º) Limpia de objetos el área de trabajo (Workspace). 
ls()
rm(list=ls(all=TRUE)) 
ls()




