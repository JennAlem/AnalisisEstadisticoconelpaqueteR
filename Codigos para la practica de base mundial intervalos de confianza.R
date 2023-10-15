
# Base de datos

library(readr)
banco_mundial <- read_delim("D:/Beatriz/Ciclo II 2023/Analisis Estadistico con el Paquete R/Practica Intervalos de confianza - Indicadores del banco mundial/indicadores_banco_mudial.csv",
                            delim = ";", escape_double = FALSE, col_types = cols(pib_pc = col_number()),
                            trim_ws = TRUE)
head(banco_mundial)
dim(banco_mundial)


#                           General

#Esperanza

t.test(banco_mundial$esperanza, conf.level = 0.95)

#PIB

t.test(banco_mundial$pib_pc, conf.level = 0.95)

#------------------------------------------------
#------------------------------------------------

#                      Filtrar los datos por país


argentina <- subset(banco_mundial, pais == "Argentina")
chile <- subset(banco_mundial, pais == "Chile")
colombia <- subset(banco_mundial, pais == "Colombia")
mexico <- subset(banco_mundial, pais == "México")
brasil <- subset(banco_mundial, pais == "Brasil")
costa_rica <- subset(banco_mundial, pais == "Costa Rica")
panama <- subset(banco_mundial, pais == "Panamá")
paraguay <- subset(banco_mundial, pais == "Paraguay")
uruguay <- subset(banco_mundial, pais == "Uruguay")


#         Realiza prueba t de student para esperanza de vida


t1 <- t.test(argentina$esperanza, conf.level = 0.95)
t2 <- t.test(chile$esperanza, conf.level = 0.95)
t3 <- t.test(colombia$esperanza, conf.level = 0.95)
t4 <- t.test(mexico$esperanza, conf.level = 0.95)
t5 <- t.test(brasil$esperanza, conf.level = 0.95)
t6 <- t.test(costa_rica$esperanza, conf.level = 0.95)
t7 <- t.test(panama$esperanza, conf.level = 0.95)
t8 <- t.test(paraguay$esperanza, conf.level = 0.95)
t9 <- t.test(uruguay$esperanza, conf.level = 0.95)

#------------------------------------------------


# argentina
t1
# chile
t2
# colombia
t3
# mexico
t4
# brasil
t5
# costa_rica
t6
# panama
t7
# paraguay
t8
# uruguay
t9

#------------------------------------------------
#------------------------------------------------

#             Realiza prueba t de student para PIB

p1 <- t.test(argentina$pib_pc, conf.level = 0.95)
p2 <- t.test(chile$pib_pc, conf.level = 0.95)
p3 <- t.test(colombia$pib_pc, conf.level = 0.95)
p4 <- t.test(mexico$pib_pc, conf.level = 0.95)
p5 <- t.test(brasil$pib_pc, conf.level = 0.95)
p6 <- t.test(costa_rica$pib_pc, conf.level = 0.95)
p7 <- t.test(panama$pib_pc, conf.level = 0.95)
p8 <- t.test(paraguay$pib_pc, conf.level = 0.95)
p9 <- t.test(uruguay$pib_pc, conf.level = 0.95)

# argentina
p1
# chile
p2
# colombia
p3
# mexico
p4
# brasil
p5
# costa_rica
p6
# panama
p7
# paraguay
p8
# uruguay
p9

#------------------------------------------------
#------------------------------------------------

#                   Pruebas de Normalidad

# H0: La variable presenta una distribución normal
# H1: La variable presenta una distribución no normal

shapiro.test(banco_mundial$esperanza)

shapiro.test(banco_mundial$pib_pc)

# Los datos de las columnas no son normales

#             Distribucion a la que pertenece


library(ggplot2)
library(gridExtra)
library(fitdistrplus)
library(cowplot)

x11()

g1 <- ggplot(banco_mundial, aes(sample = esperanza)) +
  geom_qq() +
  geom_abline(intercept = 0, slope = 2) +
  ggtitle("Esperanza de vida") + labs(subtitle = "General")

g2 <- ggplot(banco_mundial, aes(sample = pib_pc)) +
  geom_qq() +
  geom_abline(intercept = 0, slope = 2) +
  ggtitle("PIB per capita") + labs(subtitle = "General")

# Coloca los gráficos
grid.arrange(g1, g2, ncol = 2)


#    Prueba de exponencial

espe <- banco_mundial$esperanza

fit1<-fitdist(espe, "exp")
ks.test(espe,"pexp",fit1$estimate)

# En este caso, el valor de D es 0.54534 y el valor p es muy pequeño
# (menor que 2.2e-16), lo que sugiere que los datos no se distribuyen
# exponencialmente. Además, se muestra un mensaje de advertencia que
# indica que hay empates en los datos, lo que puede afectar la precisión
# de la prueba de Kolmogorov-Smirnov.



# Crear un histograma
ggplot(banco_mundial, aes(x = pib_pc)) +
  geom_histogram()

# Crear un histograma con densidad de probabilidad
ggplot(banco_mundial, aes(x = pib_pc)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density()

#------------------------------------------------
argentina <- subset(banco_mundial, pais == "Argentina")
chile <- subset(banco_mundial, pais == "Chile")
colombia <- subset(banco_mundial, pais == "Colombia")
mexico <- subset(banco_mundial, pais == "México")

brasil <- subset(banco_mundial, pais == "Brasil")
costa_rica <- subset(banco_mundial, pais == "Costa Rica")
panama <- subset(banco_mundial, pais == "Panamá")
paraguay <- subset(banco_mundial, pais == "Paraguay")
uruguay <- subset(banco_mundial, pais == "Uruguay")
#----------------------------------------------


argen <- ggplot(argentina, aes(x = pib_pc)) +
  geom_density()+
  ggtitle("Argentina")

chil <- ggplot(chile, aes(x = pib_pc)) +
  geom_density()+
  ggtitle("Chile")

colom <- ggplot(colombia, aes(x = pib_pc)) +
  geom_density()+
  ggtitle("Colombia")

Mex <- ggplot(mexico, aes(x = pib_pc)) +
  geom_density()+
  ggtitle("Mexico")

bras <- ggplot(brasil, aes(x = pib_pc)) +
  geom_density()+
  ggtitle("Brasil")

cos_ri <- ggplot(costa_rica, aes(x = pib_pc)) +
  geom_density()+
  ggtitle("Costa Rica")

pan <- ggplot(panama, aes(x = pib_pc)) +
  geom_density()+
  ggtitle("Panama")

para <- ggplot(paraguay, aes(x = pib_pc)) +
  geom_density()+
  ggtitle("Paraguay")

urug <- ggplot(uruguay, aes(x = pib_pc)) +
  geom_density()+
  ggtitle("Uruguay")


cuadri <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9), nrow = 3, ncol = 3)

grid.arrange(argen, chil, colom, Mex, bras, cos_ri, pan, para, urug, layout_matrix = cuadri)

#------------------------------------------------

# Graficar de la siguiente forma como la esperanza de vida va evolucioanndo
# en cada pais
library(ggplot2)

x <- argentina$tiempo
y <- argentina$esperanza

ggplot(data.frame(x, y), aes(x = x, y = y)) +
  geom_line()



























