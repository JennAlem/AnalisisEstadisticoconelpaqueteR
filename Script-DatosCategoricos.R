

#           1.  ANÁLISIS ESTADÍSTICO DE DATOS CATEGÓRICOS.


# Ejemplo: Se realiza un estudio para conocer las preferencias sobre el 
# tipo de gaseosa que se consume: "CC"=Coca Cola, "PC"=Pepsi Cola,
# "SC"=Salva Cola, para ello se toma una muestra aleatoria de 20 
# personas.

# 1°) Activar el directorio de trabajo getwd()

setwd("D:/Beatriz/Ciclo II 2023/Analisis Estadistico con el Paquete R/Practica 1 de septiembre")

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------


# 2°) Crear un nuevo script y llamarle Script-DatosCategoricos

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 3°) Crear un vector con el tipo de gaseosa y otro con la muestra 
# generada aleatoriamente:
  
Tipo <- c("CC", "PC", "SC")
Tipo

# crea un vector en las que contiene los tres tipos de refrescos 

Consumo <- sample(Tipo, 20, replace=TRUE)
Consumo

# Suponiendo que se quiere editar o agregar datos 

data.entry(Consumo)

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 4°) Guarde el vector en un archivo de datos

# Guardar los datos en su directorio de trabajo 

write(Consumo, "Consumo.txt")

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 5°) Eliminar los objetos que existen en el espacio de trabajo 
# (Workspace) 

ls()
rm(list=ls(all=TRUE)) 
ls()

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 6°) Leer o recuperar el vector de datos o archivo de texto

Consumo	<-	scan("Consumo.txt",	what	=	character(),	
                na.strings	=	"NA", flush=FALSE)
Consumo

ls()

# Si el vector contiene caracteres se ocupa: what = character()
# na.strings =”NA”, le indica a R que los valores faltantes 
#son identificados con “NA”

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 7°) Crear la tabla de distribución de frecuencias y proporciones 

frec <- table(Consumo)
frec
prop <- table(Consumo)/length(Consumo)
prop

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 8°) Conocer un resumen de los datos 

summary(Consumo)


#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 9°) Realizar un gráfico de barras

# Para las frecuencias absolutas

barplot(frec, main="Gráfico de barras", xlab=" Consumo", 
        col=c("yellow", "white", "red"), sub="Agosto-2012")

# Para las frecuencias relativas

barplot(prop, main="Gráfico de barras", xlab=" Consumo\n",  
        col=c("yellow",   "white", "red"), sub="Agosto-2012")


#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 10°) Realizar un gráfico de pastel

pie(frec, main="Gráfico de pastel", xlab="Tipo de Consumo", 
    col=c("yellow", "white", "cyan"), sub="Agosto-2012")

# Se puede especificar nombres para las categorías y el color de 
# los sectores 

names(frec) = c("Coca Cola", "Pepsi", "Salva Cola")

pie(frec, main="Gráfico de pastel", xlab=" Consumo", radius=0.8, 
    col=c("red", "gray", "cyan"), sub="Agosto-2012")

# Los colores se asignas dependiendo del orden en que han sido 
# especificados por names() 

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 11°) Colocar valores numéricos en los sectores del gráfico 

n <- length(frec)
hoja <- data.frame(frec)
hoja

etiq <- c(paste(hoja$Var1, "-", hoja$Freq))
etiq

pie(frec, main="Gráfico de pastel", labels=etiq, col=rainbow(n), 
    border=TRUE)


#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------













