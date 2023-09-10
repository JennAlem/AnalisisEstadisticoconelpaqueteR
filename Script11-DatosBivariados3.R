
#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 1º) Activa tu directorio de trabajo. 

getwd()

setwd("D:/Beatriz/Ciclo II 2023/Analisis Estadistico con el Paquete R/Practica 5")

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 2º) Crea un nuevo script y llámale "Script11-DatosBivariados3" 

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 3º) Crea dos vectores con los datos.

Fuma = c("Si","No","No","Si","No","Si","Si","Si","No","Si")
Fuma
Cantidad = c(1,2,2,3,3,1,2,1,3,2)
Cantidad

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 4º) Crea una hoja de datos que tenga como componentes o columnas los 
# dos vectores.

Estudia <- data.frame(Fuma=Fuma, Cantidad=Cantidad)
Estudia

# Puedes editar los datos utilizando 

fix(Estudia)

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 5º) Guarda la hoja de datos en un archivo.

write.table(Estudia, file="Estudia.txt", append=FALSE, quote=TRUE,	
            sep=" ", na="NA", col.names=TRUE)

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 6º) Elimina los objetos almacenados en el área de trabajo (Workspace).

ls()
rm(list=ls(all=TRUE)) 
ls()

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 7º) Recupera desde el archivo la hoja de datos. 

Estudia <- read.table("Estudia.txt", header=TRUE) 
Estudia

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 8º) Conecta la hoja de datos a la segunda ruta o lista de búsqueda, 

attach(Estudia, pos=2)
search()

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 9º) Crea una tabla de contigencia o de doble entrada.

tablaCont <- table(Estudia)
tablaCont

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 10º) Calcula las tablas de proporciones o de probabilidades. 

options(digits=3) # sólo imprime 3 lugares decimales

# Proporciones basadas en el total de la muestra, la suma de filas y 
# columnas suman 1 

propTotal <- prop.table(tablaCont)
propTotal

# Proporciones basadas en el total por fila, cada fila suma 1 

propFila <- prop.table(tablaCont, 1)
propFila

# Proporciones basadas en el total por columna, cada columna suma 1 

propCol <- prop.table(tablaCont, 2)
propCol

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 11º) Construya los gráficos de barras de la variable bidimensional.

# Gráfico de barras apiladas con la frecuencia de Cantidad como altura 

barplot(table(Estudia$Cantidad, Estudia$Fuma), 
        beside = FALSE, horizontal=FALSE, 
        main="Gráfico de barras (Fuma, Cantidad de horas de estudio)",
        legend.text =T, xlab="Fuma", ylab="Cantidad de horas-estudio")

# Gráfico de barras apiladas con la frecuencia de Fuma como altura

barplot(table(Estudia$Fuma, Estudia$Cantidad), 
        beside = FALSE,  horizontal=FALSE,
        main="Gráfico de barras (Cantidad de horas de estudio,Fuma)",
        legend.text =T, xlab="Cantidad de horas-estudio", ylab="Fuma")

# Gráfico de barras no apiladas y colocación de leyenda # Crear un factor para los nombres en la leyenda Fuma=factor(Estudia$Fuma); Fuma

barplot(table(Estudia$Cantidad, Estudia$Fuma), 
        main="Gráfico de barras (Fuma, Cantidad de horas de estudio)",
        xlab="Fuma", ylab="Cantidad de horas-estudio", beside=TRUE, 
        legend.text=T)

barplot(table(Estudia$Cantidad, Estudia$Fuma), 
        main="Gráfico de barras (Fuma, Cantidad de horas de estudio)",
        xlab="Fuma", ylab="Cantidad de horas-estudio", beside=TRUE, 
        legend.text=c("menor que 5", "5-10", "mayor que 10"))


#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 12º) Realiza la prueba o contraste Chi-cuadrado para las 
# probabilidades dadas 

chisq.test(tablaCont)

# Sí p-value > α aceptar H0 : Las variables son independientes
# Recuerde que las frecuencias esperadas deben ser mayores a 5 
# para poder utilizarlas.

# Probabilidades esperadas para la prueba Chi-cuadrada 

chisq.test(tablaCont) $expected



#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------








