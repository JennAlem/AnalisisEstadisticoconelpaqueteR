
#Ejemplo:

# Se selecciona aleatoriamente una muestra de 18 personas adultas, 
# para estudiar si existe relación entre su estado civil y su ocupación.


# REALICE UN ANÁLISIS ESTADÍSTICO DE LOS DATOS.

#----------------------------------------------------------
#_---------------------------------------------------------

# 1º) Activa tu directorio de trabajo. 

getwd()
setwd("D:/Beatriz/Ciclo II 2023/Analisis Estadistico con el Paquete R/Practica 5")

#2º) Limpia de objetos el área de trabajo (Workspace). 
ls()
rm(list=ls(all=TRUE)) 
ls()

#----------------------------------------------------------
#_---------------------------------------------------------

#3º) Crea un nuevo Script y llámale "Script09-DatosBivariados1"

#----------------------------------------------------------
#_---------------------------------------------------------

#4º) Crea en Excel una hoja de datos con dos columnas o variables

# Recuerda que al guardar la hoja, el tipo de archivo es de extensión .csv(delimitado por comas).
# Llámale al archivo: HojaCat

#----------------------------------------------------------
#_---------------------------------------------------------

# 5º) Recupera desde el entorno de R la hoja de datos de Excel.

#    HojaCat <- read.csv("HojaCat.csv", strip.white=TRUE);HojaCat

library(readr)
HojaCat <- read_delim("HojaCat.csv", delim = ";", 
                      escape_double = FALSE, trim_ws = TRUE)
HojaCat

#----------------------------------------------------------
#_---------------------------------------------------------


#6º) Conecta la hoja de datos a la segunda ruta o lista de búsqueda. 
attach(HojaCat, pos=2) # pos especifica la posición donde buscar la conexión search()

#----------------------------------------------------------
#----------------------------------------------------------

#7º) Crea una tabla de contigencia o de doble entrada 

tablaCont <- table(HojaCat)
tablaCont 
length(HojaCat)

# Note que esta instrucción no devuelve el número de elementos, 
#sino más bien el número de variables o columnas consideradas en 
#el conjunto de datos.

# Encuentra la suma de cada fila de la tabla de contingencia 
# Distribución marginal de X=Estado civil

suma.filas <- apply(tablaCont, 1, sum)
suma.filas # El 1 indica que son totales por fila

# Encuentra la suma de cada fila de la tabla de contingencia 
# distribución marginal de Y=Ocupación

suma.columnas <- apply(tablaCont, 2, sum)
suma.columnas # 2 indica que son totales por columna

# Gráficos de barras para tabla de contingencia. 

# Barras apiladas
barplot(t(tablaCont), main="Gráfico de barras (Estado, Ocupación)",
        xlab="Estado civil", ylab="Ocupación", legend.text=TRUE)

# # Note que t(tablaCont) indica que las barras representan el 
# Estado civil de los encuestados y que éstas se subdividen en 
# cada una de las diferentes ocupaciones consideradas.
# # En caso de usar únicamente tablaCont; las barras 
# representarán las diferentes ocupaciones y éstas estarán 
# subdividas en cada uno de los estados civiles.

# Barras agrupadas

barplot(t(tablaCont), main="Gráfico de barras (Estado, Ocupación)", 
        xlab="Estado civil", ylab="Ocupación",
        beside=TRUE, legend.text=TRUE)

# Note que la instrucción beside =TRUE, indica que por cada una 
# de las diferentes ocupaciones se creará una barra para cada 
# estado civil. Note que al usar beside =FALSE se 
# obtiene el mismo gráfico de la instrucción anterior.


barplot(tablaCont, main="Gráfico de barras (Ocupación, Estado)", 
        xlab="Ocupación\n", ylab="Estado civil", beside=TRUE, 
        legend.text=TRUE)



#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------


#8º) Calcula tablas de proporciones o de probabilidades.

# Guardar las todas las opciones iniciales y modificar número 
#de decimales 
op <- options()
options(digits=3) # sólo imprime 3 lugares decimales 
options('digits')

# Proporciones basadas en el total de la muestra, la suma de filas 
#y columnas suman 1. 

propTotal <- prop.table(tablaCont)
propTotal

barplot(t(propTotal), main="Gráfico de barras (Estado, Ocupación)", 
        xlab="Estado civil\n", ylab="Ocupación", beside=TRUE, 
        legend.text=TRUE)


# Proporciones basadas en el total por fila, cada fila suma 1.

propFila <- prop.table(tablaCont, 1)
propFila

# Total por fila se indica en 1

barplot(t(propFila), main="Gráfico de barras (Estado, Ocupación)", 
        xlab="Estado civil\n", ylab="Ocupación", beside=TRUE, 
        legend.text=TRUE)

# Proporciones basadas en el total por columna, cada columna suma 1. 

propColum <- prop.table(tablaCont, 2)
propColum

# Total por columna se indica en 2

barplot(propColum, main="Gráfico de barras (Ocupación, Estado)", 
        xlab="Ocupación\n", ylab="Estado civil", beside=TRUE, 
        legend.text=TRUE)

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------


# 9º) Otra forma de elaborar los gráficos de barras para el 
# vector bidimensional categórico. 

# Gráfico de barras no apiladas y colocación de leyenda

barplot(table(Ocupacion, Estado), 
        main="Gráfico de barras (Estado, Ocupación)", 
        xlab = "Estado civil", ylab="Ocupación", beside=TRUE, 
        legend.text=T)

barplot(table(Estado, Ocupacion), 
        main="Gráfico de barras (Ocupación, Estado)", 
        xlab = "Ocupación", ylab="Estado civil", beside=TRUE, 
        legend.text=TRUE)


barplot(table(Estado, Ocupacion), 
        main="Gráfico de barras (Ocupación, Estado)", 
        xlab="Ocupación", ylab="Estado civil", beside=TRUE, 
        legend.text=c("menor que 2", "2-3", "mayor que 3"))

# Note que se puede definir a conveniencia la leyenda que se 
# desea incorporar en el gráfico con la instrucción legend.text


#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------


# 10º) Realizar la prueba o contraste Chi-cuadrado de independencia

prueba <- chisq.test(tablaCont)
prueba

# Tenga en cuenta que las frecuencias esperadas deben ser todas 
#mayores a 5

# Frecuencias absolutas esperadas para la prueba Chi-cuadrada 
prueba$expected # fij = fi./No. column

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------


# ME QUEDE EN LA PAGINA 10


