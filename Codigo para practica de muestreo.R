


library(readr)
hogares <- read_csv("D:/Beatriz/Ciclo II 2023/Analisis Estadistico con el Paquete R/Practica tamanio muestra - prueba de hipotesis/hogares_enigh.csv")
View(hogares)

#  Muestreo aleatorio simple

# Ejemplo para 10 viviendas
elementos <- sample(hogares_enigh$folioviv, 10)
elementos

# Muestreo sistematico

# Un numero aleatorio entre 1 y 10 y luego sumarle 10

library(devtools)
install_github("DFJL/SamplingUtil")

library(SamplingUtil)

N <- nrow(hogares)

sys.sample(N, n=10)



# ME QUEDE.   LUEGO PDEBO PREGUNTAR POR EL DOCUMENTO


vivienda <- seq(1:100)
sample(vivienda, 10)



# Muestreo aleatorio simplre

hombres <- subset(hogares, sexo_jefe == 1)
sample(hombres$folioviv, 5)


mujeres <- subset(hogares, sexo_jefe == 2)
sample(mujeres$folioviv, 5)

# Muestreo por Conglomerados


# Muestreo por conveniencia


# Muestreo de respuesta voluntaria


# Prevencion del sesgo





# Correlacion no implica causalidad

# el chocolate tiene correlacion con los grammys ganados,
# pero esto no tiene causalidad.










