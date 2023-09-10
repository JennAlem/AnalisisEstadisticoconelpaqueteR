
#          REALICE UN ANÁLISIS ESTADÍSTICO.
#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 1º) Activa tu directorio de trabajo 

getwd()
setwd("D:/Beatriz/Ciclo II 2023/Analisis Estadistico con el Paquete R/Practica 5")

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 2º) Crea un nuevo script y llámale "Script11-DatosBivariados4" 

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 3º) Crea los dos vectores para las dos variables
# Número de usuarios = Variable explicativa o independiente 

usuarios <- c(10, 15, 20, 20, 25, 30, 30)
usuarios

tiempo = c(1.0, 1.2, 2.0, 2.1, 2.2, 2.0, 1.9)
tiempo

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 4º) Crea una hoja de datos que tenga como componentes o columnas 
# los dos vectores.

Sistema <- data.frame(Usuarios=usuarios, Tiempo=tiempo)
Sistema # Para editar o ampliar los datos puede utilizar la función fix() fix(Sistema)

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 5º) Guarda la hoja de datos en un archivo.

write.table(Sistema,   file="Sistema.txt",   append=FALSE,   
            quote=TRUE,	sep=" ", na="NA", col.names = TRUE)

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


# 7º) Recupera la hoja de datos.

Sistema <- read.table("Sistema.txt", header=TRUE)
Sistema

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 8º) Conecta la hoja de datos a la segunda ruta o lista de búsqueda. 

attach(Sistema, pos=2)
search()

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 9º) Muestra un resumen de principales estadísticos de las variables. 

summary(Sistema)
cov(Sistema) # Matriz de covarianzas
cor(Sistema, use = "all.obs", method="pearson") # Matriz de correlaciones

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 10º) Elabora un gráfico de dispersión para analizar alguna relación 
# entre las variables.

plot(Usuarios, Tiempo, xlim= c(5, 35), ylim= c(0.0, 2.5), 
     type = "p", pch=1, col = "blue", 
     main = "Gráfico de dispersión (Usuarios, Tiempo)", 
     xlab="Número de usuarios", ylab="Tiempo de ejecución")

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 11º) Para identificar un punto arbitrario, se procede de la 
# siguiente manera:

  #Sin cerrar la ventana del gráfico anterior, ejecuta la siguiente instrucción

  identify(Usuarios, Tiempo, n=1) # n=1 indica que solamente será un punto seleccionado

# Y luego selecciona un punto en el gráfico haciendo clic con el ratón. 
# Esto es útil para identificar puntos que podrían ser atípicos.
# Deberá aparecer en la R-Console el índice que corresponde a este punto.

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 12º) Aplica la función lm() para encontrar el modelo lineal que se 
# ajusta a los datos.
reg.Y.X <- lm(Tiempo ~ -1 + Usuarios, Sistema, 
              na.action=NULL, method="qr", model=TRUE) 
#-1 indica que no se toma en cuenta la constante en el modelo.

summary(reg.Y.X)
# Note que es necesaria la instrucción anterior para poder visualizar los resultados más sobresalientes de la regresión encontrada. Nos muestra la estimación de los parámetros junto con su significancia, el coeficiente de determinación.

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 13º) Agrega la recta de regresión al gráfico de dispersión. 

abline (reg.Y.X)
# Observación: Alternativamente si quiere una recta más "exacta" use:
  lines(Usuarios, 0.079437*Usuarios)

#----------------------------------------------------------
#----------------------------------------------------------
#----------------------------------------------------------

# 14º) Efectúa una análisis de variabilidad del modelo o descomposición
# de la varianza. 

reg.anova <- anova(reg.Y.X)
reg.anova



