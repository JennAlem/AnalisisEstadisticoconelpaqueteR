library(ggplot2)

a = 0
b = 90
x = 15
punif(x, min = a, max = b, lower.tail = TRUE)

k 
punif(x, min = a, max = b, lower.tail = TRUE)

#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------


# cual es la probabilidad de que en ambas citas se tarde más de 55 min

# Definir los parámetros de la distribución uniforme
a <- 0
b <- 90

# Calcular la probabilidad de que la persona se tarde más de 55 minutos en una cita
p <- punif(55, min = a, max = b, lower.tail = FALSE)

# Calcular la probabilidad de que la persona se tarde más de 55 minutos en ambas citas
p^2

#-------------------------------------------------------------------
#-------------------------------------------------------------------
#_-----------------------------------------------------------------


# Una empresa busca personal, el perfil solicitado es que sean personas 
# extrovertidas y creativas. Se han presentado 50 candidatos y el 
# criterio establecido es que superen el percentil 80 en creatividad y 
# extroversión. Se sabe que la variable extroversión se distribuye 
# normalmente con media 5 y desviación típica 1 y que la variable 
# creatividad se distribuye con una t-student y tiene parámetros de 10 
# grados de libertad, además ambas puntuaciones son independientes. 
# (X=extroversión, Y=creatividad).

x=80
df=10
pt(x, df, lower.tail = TRUE, log.p= FALSE)

pnorm(80, 5, 1, lower.tail = TRUE)










n=16
x2=4.5
media=5
ee = 1/sqrt(n)

pnorm(x2, 5, ee, lower.tail = F)


# 1 persona (Distribucion individual o algo asi)

pnorm(5.6, 5, 1, lower.tail = F)


# 16 personas (Distribucion muestral)

pnorm(5.6, 5, ee, lower.tail = F)




# MARCAPASOS

#                        A

# Definir la media de la distribución exponencial
media <- 7

# Calcular la probabilidad de que el marcapasos dure al menos 5 años
p <- pexp(5, rate = 1/media, lower.tail = T) # = P(x<5) 

p

p <- pexp(5, rate = 1/media, lower.tail = F) # = P(x >= 5) 

p

#                    B 

# Calcular la probabilidad de que el marcapasos dure menos de 3 años
p <- pexp(3, rate = 1/media, lower.tail = T)

p

#-------------------------------------------------------------

# Definir la media de la distribución exponencial

media <- 7

# Calcular la probabilidad de que un modelo de marcapasos que ya ha 
#durado 4 años dure otros 4 años

p <- pexp(8, rate = 1/media, lower.tail = T) - 
  pexp(4, rate = 1/media, lower.tail = T)

p


p <- pexp(4, rate = 1/media, lower.tail = F) /
  pexp(8, rate = 1/media, lower.tail = F)
p
