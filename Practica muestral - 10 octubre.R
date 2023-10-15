# Practica muestral


#                     5.3 Simulación



library(dplyr)
library(tidyverse)

n <- 10 # Número de observaciones en cada muestra
r <- 10000 # Número de replicas / número de muestras a recolectar
muestras <- matrix(rexp(n*r,3),r)
muestras.df <- data.frame(t(muestras))

# Calculamos la media de cada muestra, y además la media de la población,
# que es el promedio de todas las medias muestrales:

media <- data.frame(X1=apply(muestras.df,2,mean))
media_poblacional <- mean(media$X1)


# Gráficamos la distribución de solo una muestra, X1 por ejemplo:

label.left <- paste0("Media de 1 \n muestra: ",round(media$X1[1],3))
grafica1 <- muestras.df %>%
  ggplot() + geom_histogram(aes(x=X1),bins=10) +
  annotate("text", x=0.6 , y=2.75 , label=label.left)
grafica1


# Ahora gráfiquemos la distribución de todos los promedios
# de las 10,000 muestras seleccionadas:

label.right <- paste0("Media de las \n distribución \n muestral \n con ",
                      r," muestras \n de tamaño \n 10 cada una:0.332")

grafica2 <- media %>% ggplot() +
  geom_histogram(aes(x=X1)) +
  annotate("text",x=0.7, y=900, label=label.right)
grafica2


library(cowplot)
cowplot::plot_grid(grafica1,grafica2)



n <- 200 # Número de observaciones en cada muestra
r <- 10000 # Número de replicas / número de muestras a recolectar
muestras <- matrix(rexp(n*r,3),r)
muestras.df <- data.frame(t(muestras))

media <- data.frame(X1=apply(muestras.df,2,mean))
media_poblacional <- mean(media$X1)

label.left <- paste0("Media de 1 \n muestra: ",round(media$X1[1],3))
grafica1 <- muestras.df %>% ggplot() +
  geom_histogram(aes(x=X1),bins=10) +
  annotate("text", x=1.2 , y=40 , label=label.left)

label.right <- paste0("Media de las \n distribución \n muestral \n con ", r," muestras \n de tamaño \n 200 cada una:0.333")
grafica2 <- media %>% ggplot() +
  geom_histogram(aes(x=X1)) +
  scale_x_continuous(limits = c(0.2,0.6)) +
  xlab("medias") +
  annotate("text",x=0.5, y=1500, label=label.right)

cowplot::plot_grid(grafica1,grafica2)

#------------------------------------------------
#------------------------------------------------
#------------------------------------------------
#------------------------------------------------


#                            Tarea

#Cual es la probabilidad de tener una media de 0.6 (mayor, menor o igual)


#                         Investigar:

# Valor actual Neto
# Tasa interno de retorn TIR












































































