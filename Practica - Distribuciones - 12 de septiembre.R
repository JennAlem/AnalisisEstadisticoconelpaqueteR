
# dpois() distribucion

# ppois() se obtienen las probabilidades

#qpois() tambien

# rpois es para numeros aleatorios

# No se juega con los datos, se establecen distintos escenarios para
# evaluar cual se adapta mejor a los datos.

lambda = 0.9
valor = 4
dpois(valor, lambda = lambda)

# El resultado significa que:
# P(x=4) = 0.0111146

# En promedio se espera que lleguen 0.9 pacientes

# Se debe garantizar la aleatoriedad.

# En un intervalo de tiempo va a ocurrir un determinado evento

# lambda es el promedio de ocurencia en un evento determinado 
# Ocurre 0.9 eventos en un día




rango = 0:8
rango

# Deinir tabla
# Un data.frame es una estructura de datos en donde voy a definir 
# mis datos


df = data.frame(x=rango, y=dpois(rango, lambda = lambda))

#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------


ggplot(df, aes(x = x, y=y,
               fill=factor(ifelse(x==valor, "valor", "resto"))))  +
  
  
  geom_bar(width = 0.9, stat="identity", 
           position = position_dodge(), )  +                 
  
  
  labs(x="Personas condenadas", y= "Probabilidad") +  
  labs(fill = "")  +
  
  theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=-0.3)) +  
  theme_linedraw() +                                                       
  theme(legend.position = "left") +                                        
  facet_grid(~"dpois Personas Condenadas")+
  theme(strip.text = element_text(size = 17),
        panel.grid.major = element_line(color = alpha("black", 0.3)),  
        
        panel.grid.minor = element_line(color = alpha("black", 0.3)))

#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------


# Calcular la probabilidad de que un día cualquiera se reciban 
# a lo mucho 2 pacientes

# Se debe garantizar la aleatoriedad.


#                  lambda = 0.9 --> 1 dia

# Problema: P(x <= 2)


lambda2 = 0.9
dpois(0, lambda = lambda2)
dpois(1, lambda = lambda2)
dpois(2, lambda = lambda2)

# P(x<=2)= 

pp = dpois(0, lambda = lambda2) + dpois(1, lambda = lambda2) + 
  dpois(2, lambda = lambda2)

pp

valor2 = 2
ppois(valor2, lambda = lambda2)

rango2 = 0:8
df2 = data.frame(x=rango2, y= dpois(rango2, lambda = lambda2))


# lamda es el valor esperado, 

# lambda es el promedio de ocurencia en un evento determinado 
# Ocurre 0.9 eventos en un día

#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------


ggplot(df2, aes(x = x, y=y,
               fill=factor(ifelse(x==valor, "valor", "resto"))))  +
  
  
  geom_bar(width = 0.5, stat="identity", 
           position = position_dodge(), )  +                 
  
  
  labs(x="Personas condenadas", y= "Probabilidad") +  
  labs(fill = "")  +
  
  theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=-0.3)) +  
  theme_linedraw() +                                                       
  theme(legend.position = "right") +                                        
  facet_grid(~"dpois Personas Condenadas")+
  theme(strip.text = element_text(size = 17),
        panel.grid.major = element_line(color = alpha("black", 0.3)),  
        
        panel.grid.minor = element_line(color = alpha("black", 0.3)))

#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------

lambda3 = 0.9

qpois(0.5, lambda = lambda3)

#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------


r = 8

rpois(r, lambda = lambda3)

# Es una simulación
# El resultado es: 1 0 0 1 1 0 0 1
# Estos son los pacientes que puede tener la clnica en diferentes dias


r2 = 30

rpois(r2, lambda = lambda3)

# Si la simulacion se quiere cuantificar se utiliza dpois()


#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------


#                       DISTRIBUCION NORMAL


# ¿La diatribucion normal es parametrica o no parametrica?

# Es parametrica porque ya sabe la probabilidad cuando ya 
# estandarizamos

# Investigar sobre eso


# Las parametricas cuando los datos no influyen en el grafico 
# de los datos
# Poason tiene una forma sesgada a la derecha.


# Son no parametricas si el grafico cambia segun los datos

#                       Investigar:
#¿Cuales son las distribuciones parametricas y no parametricas?
# ¿Porque las tablas Z llegan hasta 3?


#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------


#                       DISTRIBUCION NORMAL


media = 3
desv.tip = 0.3
valor1 = 3
valor2 = 2.8

# Distribuciones discretas:
# Si tiene sentido preguntar, cual es la probabilidad de x=3
# pero en una distribucion continua, tiene sentido?
# R/ No porque el domnio es infinito (media)

# sd es estandar desviecion: desviacion estandar

pnorm(valor1, mean = media, sd = desv.tip)

pnorm(valor2, mean = media, sd = desv.tip)

pnorm(valor1, mean = media, sd = desv.tip) - pnorm(valor2, mean = media, sd = desv.tip)

#                Reto:

rango5 = c(media - 3*desv.tip, media + 3*desv.tip)

# Es 3 porque va afianzar el 99%
# Mas de 3 desviaciones no es significativo

dnorm(3,0.3)
#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------

# Investigar para que se usa el "args"

ggplot(data.frame(x=rango5), aes(x=rango5)) +
  stat_function(fun = dnorm, n = 101, 
                args = list(mean=media, sd = desv.tip)) +
  
  geom_area(stat = "function", fun = dnorm, 
            args = list(mean=media, sd = desv.tip),
            fill = "red", xlim = c(valor1, valor2), alpha = 0.6) +
  labs(x="Rango", y= "Probabilidad") +  
  labs(fill = "") +
  
  theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=-0.3)) +  
  theme_linedraw() +                                                       
  theme(legend.position = "right") + 
  
  facet_grid(~"Distribución Normal")+
  theme(strip.text = element_text(size = 17),
        panel.grid.major = element_line(color = alpha("black", 0.3)),  
        
        panel.grid.minor = element_line(color = alpha("black", 0.3)))

# Agregar titulos


#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------






