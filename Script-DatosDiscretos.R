
#           Ejemplo:

# En cierta colonia de San Salvador se selecciona aleatoriamente 
# una muestra de 30 hogares, al medir el número de hijos en cada 
# unidad muestral se obtienen los siguientes datos:
  
  
#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 1°) Activar el directorio de trabajo getwd()

getwd()

setwd("D:/Beatriz/Ciclo II 2023/Analisis Estadistico con el Paquete R/Practica 1 de septiembre")


#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 2º) Crear un nuevo Script y llamarle "Script-DatosDiscretos" 

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 3º) Crear el vector de datos.

Hijos <- c(2, 1) 
data.entry(Hijos) 
Hijos 
length(Hijos)

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 4º) Guardar el vector de datos en un archivo de texto. 

write(Hijos, "Hijos.txt")

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 5º) Limpiar el área de trabajo (Workspace) 

ls()
rm(list=ls(all=TRUE))
ls()

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

#6º) Leer o recuperar el vector de datos o archivo de texto

X <- scan("Hijos.txt", what = integer(0), na.strings = "NA", 
          flush=FALSE) 
ls()

# Si el vector contiene caracteres se usa: what = character() 
# Si el vector contiene reales se ocupa: what = double(0)

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 7º)Elaborar el gráfico de puntos y diagrama de tallo-hojas 
# (stem-and-leaf) 

# Gráfico de puntos

stripchart(X, method="stack", vertical=FALSE, col="blue", 
           pch=1, main="Gráfico de\n puntos", xlab="Número de hijos")

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

#8º) Crear la tabla de frecuencias completa 

# frecuencias individuales
fab <- table(X); fab # frecuencias absolutas
fre <- fab/length(X); fre # frecuencias relativas
Fac <- cumsum(fab); Fac # frecuencias acumuladas
Far <- Fac/length(X); Far # frecuencias acumuladas relativas

# tabla de frecuencias completa options(digits=2)
tabla <- data.frame(fab=fab, fre=fre, Fac=Fac, Far=Far) 
names(tabla) <- c("X", "fab", "free.X", "fre", "Fac", "Far") 
tabla
tfre <- data.frame(X=tabla$X, fab=tabla$fab, fre=tabla$fre, 
                   Fac=tabla$Fac, Far=tabla$Far) 
tfre


#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 9º) Calcular los estadísticos descriptivos de la variable 

# Estadísticos de tendencia central de los datos 

media <- mean(X, na.rm = FALSE)
media

# na.rm = FALSE, le indica a R que los datos faltantes son 
# omitidos en el cálculo de la media.

for(i in 1:length(X)) if (fab[i] == max(fab)) break()
moda <- names(fab[i])
moda 
# R no tiene incorporada una función para la moda

mediana <- median(X)
mediana

# Estadísticos de dispersión o variabilidad de los datos
range(X) # Devuelve el valor mínimo y máximo del conjunto de datos.

cuasivar <- var(X)
cuasivar 
s <- sd(X)
s

# Devuelve la cuasivarianza y la cuasivarianza muestral

quantile(X,c(0.25, 0.5, 0.75)) # Cálculo de Q1, Q2, Q3

quantile(X, 0.6)
# En general se pueden encontrar cualquier percentil

# Conocer un resumen de los datos 

resumen <- summary(X)
resumen # Min, Q1, Median, Mean, Q3, Max

fivenum(X)
# min, cuartil menor, mediana, cuartil mayor, max


#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 10º) Elaborar los gráficos que se le pueden aplicar a la variable 
# discreta 

# Gráfico de barras (por ser pocos valores)

barplot(tfre[[2]], main="Gráfico de barras", 
        xlab="X = Número Hijos\n", ylab="frecuencia", 
        col=c("yellow", "blue", "white", "orange", "cyan", "red"), 
        sub="Agosto-2012")

# Gráfico de pastel (por ser pocos valores)

pie(tfre[[2]], main="Gráfico de pastel", xlab="Número Hijos \n", 
    col=c("yellow", "blue", "white", "orange", "cyan", "red"), 
    sub="Agosto-2012")

# Se puede especificar nombres para las categorías

names(fab) = c("Cero", "Uno", "Dos", "Tres", "Cuatro", "Cinco")

pie(fab, main="Gráfico de pastel", xlab="X = Número Hijos\n", 
    col=c("yellow", "blue", "white", "orange", "cyan", "red"), 
    sub="Agosto-2012")

# Gráfico de cajas (box-plot) es la representación gráfica de los 
# cinco números 

# Horizontal

boxplot(X, main="Gráfico de caja", ylab="Número de hijos\n")

# Vertical

boxplot(X, main="Gráfico de caja", xlab=" Número de hijos\n", 
        plot=TRUE, border="red", col="yellow", horizontal=TRUE)








