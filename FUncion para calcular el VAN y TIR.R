

van_tir <- function(flujos_caja, tasa_descuento) {
  # Calcular el VAN
  van <- sum(flujos_caja / (1 + tasa_descuento)^(0:length(flujos_caja) - 1))

  # Calcular la TIR
  tir <- uniroot(function(x) van - flujos_caja / (1 + x)^(0:length(flujos_caja) - 1), lower = 0, upper = 100)$root


  # Devolver los resultados
  return(list(van = van, tir = tir))
}


# La función calcula el VAN usando la siguiente fórmula:
#
#   VAN = \sum_{i=0}^n \frac{CF_i}{(1+r)^i}


flujos_caja <- c(100, 50, 50, 50, 50)
tasa_descuento <- 0.1

resultados <- van_tir(flujos_caja, tasa_descuento)

print(resultados)

# Por ejemplo, si tenemos una base de datos con dos proyectos
# de inversión, podemos usar el siguiente código para calcular el
# VAN y el TIR de cada proyecto:

datos <- data.frame(
  flujos_caja = c(100, 50, 50, 50, 50, 100, 50, 50, 50, 50),
  tasa_descuento = c(0.1, 0.15)
)

resultados <- apply(datos, 1, van_tir)

print(resultados)

#-------------------------------------------------------------------------------

flujos_caja <- c(-2500, 2000, 1500)

tasa_descuento <- 0.05

resultados <- van_tir(flujos_caja, tasa_descuento)



datos <- data.frame(
  flujos_caja,
  tasa_descuento
)

nuevo_resultado <- apply(datos, 1, van_tir)

print(nuevo_resultado)


#------------------------------------------------------------------------------

#                         Funcion de VAN Y TIR


van_tir <- function(flujos_caja, tasa_descuento) {
  # Calcular el VAN
  van <- sum(flujos_caja / (1 + tasa_descuento)^(0:length(flujos_caja) - 1))

  # Calcular la TIR
  tir <- uniroot(function(x) van -
                   flujos_caja / (1 + x)^(0:length(flujos_caja) - 1),
                 interval = c(0,1))$root


  # Devolver los resultados
  return(list(van = van, tir = tir))
}



flujos_caja <- c(-2500, 2000, 1500)

tasa_descuento <- 0.05

resultados <- van_tir(flujos_caja, tasa_descuento)










