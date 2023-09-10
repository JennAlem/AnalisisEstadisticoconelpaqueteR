
#               1. USO DE LA FUNCIÓN READ.TABLE().

# 5º) Recuperar los objetos o datos guardados en el archivo "datos01.txt" 

Entrada1 <- read.table("D:/Beatriz/Ciclo II 2023/Analisis Estadistico con el Paquete R/Practica 1 de septiembre/datos01.txt", header=T)
Entrada1 

# No existe diferencia entre ambos archivos a la hora de leerlos


# 6º) Leer los datos contenidos en el archivo “mexico.dat”

Mexico <- read.table("mexico.dat.txt")
Mexico



#                   2. USO DE LA FUNCIÓN SCAN().

# La función scan() es más flexible que read.table() y permite realizar 
# lecturas más complejas, como puede consultar en la ayuda: help(scan)

#              Ejemplo 1: 

# Leer sólo las dos primeros objetos o columnas del archivo "datos01.txt"

Edat1 <- scan("datos01.txt", list(X1=0, X2=0), 
              skip = 1, flush = TRUE, quiet = TRUE)
Edat1 

#              Ejemplo 2: 
  
# Crear un archivo con la función cat() y luego recuperarlo 

cat("TITULO Línea extra", "2 3 5 7", "11 13 17", 
    file="datos02.txt", sep="\n")

# El archivo lo recuperamos con la función scan():

pp <- scan("datos02.txt", skip = 1, quiet= TRUE)
pp

#             3. USO DE LA FUNCIÓN READ.CSV().

# 1º) Ingresar al Microsoft Excel y crear la hoja de datos que se pide:

# 2º) Regresar al entorno de R y recuperar el archivo "HojaE1.csv". 

hojaR <- read.csv("HojaE1.csv", sep = ";", strip.white = TRUE)
hojaR

# el tipo de objeto que es hojaR con: 

is.matrix(hojaR)
is.list(hojaR)
is.data.frame(hojaR)

# Acceda a la componente Producto de hojaR con: 
  
hojaR$Producto


# Observe que R toma está columna (variable de caracteres) 
# como un Factor Nominal, verifíquelo tecleando:

is.vector(hojaR$Producto)
is.factor(hojaR$Producto)

# ¿Qué tipo de objeto es la columna Cantidad.S1? 

is.vector(hojaR$Cantidad.S1)
is.factor(hojaR$Cantidas.S1)


#                     4. USO DEL PAQUETE RODBC.


install.packages(c("RODBC"))

library(RODBC)

# Seleccionar	el	archivo	(el	cual	puede	contener	más	de	una	hoja	
# de	datos) “contaminación_mexico.xls”, con la instrucción:

datos.xls <- odbcConnectExcel(file.choose())

# Seleccionar la hoja en la cual se encuentran los datos 

datoshoja1.xls <- sqlFetch(datos.xls,"contaminacion_mexico")


#              5.IMPORTAR DATOS DE SPSS HACIA R.

install.packages(c("foreign")) 

library(foreign)


#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------


#      1. ESTRUCTURA CONDICIONAL: LA ORDEN IF() Y IFELSE()

# Ejemplo 1: 
# 
#   if(x>0) y<-1 else y<-0, le asigna a la variable "y" un 
# valor de 1 si x es mayor que 0, en caso contrario le asigna 
# el valor 0.


#        Ejemplo 2:

# Se quiere definir una función para calcular la media de un vector 
# de datos. Una definición podría ser:

  media <- function(x)
  {
    n = length(x) suma <- 0.0
    for(i in 1:n) suma = suma + x[i] media = suma/n
  }




