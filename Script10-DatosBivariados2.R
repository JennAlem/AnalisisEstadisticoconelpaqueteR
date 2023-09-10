
# Ejemplo 1:
#   Se están estudiando tres procesos (A, B, C) para fabricar pilas 
# o baterías. Se sospecha que el proceso incide en la duración 
# (en semanas) de las baterías, es decir, que la duración 
# (en semanas) de los procesos es diferente. Se seleccionan 
# aleatoriamente cinco baterías de cada proceso y al medirles 
# aleatoriamente su duración los datos que se obtienen, son los 
# siguientes:
  

# Nota: Cuando los datos bivariados se obtiene de una variable 
# cualitativa y otra cuantitativa, los valores cuantitativos de cada 
# categoría o nivel de la variable cualitativa se consideran como 
# muestras o grupos diferentes. Cada muestra se describe aplicando 
# la representación y medidas de resumen de una variable univariada 
# pero de manera conjunta.

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------


# 1º) Activa tu directorio de trabajo. 

getwd()

setwd("D:/Beatriz/Ciclo II 2023/Analisis Estadistico con el Paquete R/Practica 5")

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

#  2º) Crea un nuevo script y llámale "Script10-DatosBivariados2"

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 3º) Crea un vector de datos para cada proceso descrito en el problema.

A <- c(100,96,92,96,92)
A
B <- c(76,80,75,84,82)
B
C <- c(108,100,96,98,100)
C

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 4º) Crea una hoja de datos teniendo como componentes (columnas) los 
# tres vectores (se puede hacer pues el número de datos en cada proceso 
# es igual, de lo contrario se debería de crear dos variables una para 
# la duración de cada proceso y otra para identificar a qué proceso 
# corresponde).

Baterias <- data.frame(procesoA=A, procesoB=B, procesoC=C)
Baterias # Para editar los datos puede utilizar la función fix()
fix(Baterias)

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

#5º) Guarda la hoja de datos en un archivo.

write.table(Baterias, file="Baterias.txt", 
            append=FALSE, quote=TRUE, sep=" ", 
            na="NA", col.names=TRUE)

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 6º) Elimina todos objetos que existen en el espacio de 
# trabajo (Workspace) 

ls()
rm(list=ls(all=TRUE))
ls()

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 7º) Recupera la hoja de datos, para probar si fue guardada.

Baterias <- read.table("Baterias.txt", header=TRUE)
Baterias

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 8º) Conecta o adjunta la hoja de datos a la segunda ruta o 
# lista de búsqueda. 

attach(Baterias, pos=2)
search()

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 9º) Dibuja un gráfico horizontal de puntos para los tres procesos.

stripchart(Baterias, main="Gráfico de puntos para los tres procesos", 
           method = "stack", vertical = FALSE, col="blue", pch=1, 
           xlab="Duración (semanas)", ylab="Proceso")

# Note que con ayuda de este gráfico podemos observar sí los tres
# procesos se comportan de manera distinta o parecida en cuanto a
# duración en semanas de las baterías.

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 10º) Muestra un resumen estadístico para los tres procesos. 
summary(Baterias)

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 11º) Dibuja un gráfico de cajas (box-plot) para los tres procesos. 

# Horizontal
boxplot(Baterias, width=NULL, varwidth=TRUE, names, add= FALSE, 
        horizontal = TRUE, main="Gráfico de caja por proceso", 
        border=par("fg"), col=c("yellow", "cyan", "red"), 
        xlab = "Duración (semanas)", ylab="Proceso")

# Vertical
boxplot(Baterias, width=NULL, varwidth=TRUE, names, add= FALSE, 
        horizontal = FALSE, main="Gráfico de caja por proceso", 
        border=par("fg"), col=c("yellow", "cyan", "red"), 
        xlab = "Duración (semanas)", ylab="Proceso")

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 12º) Presenta la matriz de covarianzas muestral. 

options(digits=3) # sólo imprime 3 lugares decimales 
S <- var(Baterias)
S

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 13º) Presenta la desviación estándar de cada proceso. 
desv <- sd(Baterias)
desv


#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 14º) Realiza un análisis de varianza de una vía, para probar la 
# hipótesis nula de que el proceso no influye en la duración de las
# baterías, es decir, que no hay diferencias entre los tres procesos.
# 
# 
# 
# H0 : μA = μB = μC
# , no existe diferencias entre los tres procesos.
# 
# H1 : μi ≠ μ j , por lo menos un par i ≠
# j , de procesos difieren en la duración de las baterías.


# Concatena los tres vectores dentro de un vector simple, junto con 
# un  vector factor indicador de la categoría o tratamiento (A, B, C) 
# que origina cada observación. El resultado es un data.frame que tiene
# como componentes los dos vectores anteriores.

Baterias <- stack(Baterias)
Baterias
names(Baterias) # Muestra los encabezados de los vectores


# Prueba de igualdad de medias por descomposición de la varianza en 
# dos fuentes de variación: la variabilidad que hay entre los grupos 
# (debida a la variable independiente o los tratamientos), y la 
# variabilidad que existe dentro de cada grupo (variabilidad no 
# explicada por los tratamientos).

aov.Baterias <- aov(values~ind, data=Baterias)

# values~ind relaciona los valores muestrales con los respectivos 
# grupos 

summary(aov.Baterias)

# Note que es necesario la instrucción anterior para poder 
# visualizar la tabla ANOVA

# Decisión: ya que α = 0.05 > p-value obtenido, entonces se rechaza Ho

# Prueba de igualdad de medias en un diseño de una vía 
# (o unifactorial) asumiendo que las varianzas de los grupos son 
# iguales

oneway.test(values~ind, data=Baterias, var.equal = TRUE)


#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 15º) Deshace la concatenación del vector de valores y el vector 
# indicador de categoría.

Baterias = unstack(Baterias)
Baterias


#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------


# 16º) Desconecta la hoja de datos de la segunda ruta o lista de 
# búsqueda. 

detach(Baterias, pos=2)
search()










