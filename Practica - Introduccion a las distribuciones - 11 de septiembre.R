
n = 50
p = 0.35
valor = 20
dbinom(valor, size = n, prob=p)
# La probabilidad de que de 50 personas capturadas una sea condenada 
# es de 0.35 y la prob de que 20 sean condenadas es de 0.08750881

rango = 0:n

df = data.frame(x = rango, y = dbinom(rango, size = n, prob = p))

geom_bar(df)

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
  facet_grid(~"dbinom Personas Condenadas")+
  theme(strip.text = element_text(size = 17),
        panel.grid.major = element_line(color = alpha("black", 0.3)),  
        
        panel.grid.minor = element_line(color = alpha("black", 0.3)))

#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------

valor2 = 14
n = 50
p = 0.35
pbinom(valor2, size = n, prob=p, lower.tail = FALSE)
pbinom(valor2, size = n, prob=p)

# Encontrar P(x>14) para esto hay que calcular todas las anteriores
# Para estose utiliza lower.tail  o se puede utilizar teoria de 
#complementos: 1-pbinom(14,50,035)

#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------
rango = -50:n

df2 = data.frame(x = rango, y = pbinom(rango, size = n, prob = p, 
                                       lower.tail = FALSE))

ggplot(df, aes(x = x, y=y,
               fill=factor(ifelse(x>14, "valor", "resto"))))  +
  
  
  geom_bar(width = 0.9, stat="identity", 
           position = position_dodge(), )  +                 
  
  
  labs(x="Personas condenadas", y= "Probabilidad") +  
  labs(fill = "")  +
  
  theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=-0.3)) +  
  theme_linedraw() +                                                       
  theme(legend.position = "left") +                                        
  facet_grid(~"dbinom Personas Condenadas")+
  theme(strip.text = element_text(size = 17),
        panel.grid.major = element_line(color = alpha("black", 0.3)),  
        
        panel.grid.minor = element_line(color = alpha("black", 0.3)))


#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------

# Encontrar la probabilidad de P(12<x<20) y su grafico
# Se sombreara una parte
n = 50
p = 0.35
valor2 = 14
rango = -50:n

pbinom(valor2, size = n, prob = p, 
       lower.tail = FALSE)

pbinom(valor2<19 & valor2>13, size = n, prob = p)

df2 = data.frame(x = rango, y = pbinom(valor2, size = n, prob = p)


ggplot(df2, aes(x = x, y=y,
               fill=factor(ifelse(x>11 & x<20, "valor", "resto"))))  +
  
  
  geom_bar(width = 0.9, stat="identity", 
           position = position_dodge(), )  +                 
  
  
  labs(x="Personas condenadas", y= "Probabilidad") +  
  labs(fill = "")  +
  
  theme(axis.text.x = element_text(angle = -45, vjust = 1, hjust=-0.3)) +  
  theme_linedraw() +                                                       
  theme(legend.position = "left") +                                        
  facet_grid(~"dbinom Personas Condenadas")+
  theme(strip.text = element_text(size = 17),
        panel.grid.major = element_line(color = alpha("black", 0.3)),  
        
        panel.grid.minor = element_line(color = alpha("black", 0.3)))


#------------------------------------------------------------------
#------------------------------------------------------------------
#------------------------------------------------------------------

#              Valor de X dado una probabilidad


a = 0.3
qbinom(a, size=n, prob = p)
a = 0.4
qbinom(a, size=n, prob = p)

#                   Modelacion y simulacion

#   rbinom( ) da una muestra aleatorio de numeros binomial


rbinom(10, size = n, prob = p)

# Cada vez que se ejecuta ese codigo se generan numeros aleatorios
# y R genera valores diferentes. Si usted quiere generar los mismos 
# valores se puede fijar una semilla mediante la funcion set.seed()

# Al escribir un numero dentro del codigo anterior este pasa a tener
# EL valor de una lista de numeros

# Ejemplo

set.seed(1)
rbinom(10,size=n, prob=p)











