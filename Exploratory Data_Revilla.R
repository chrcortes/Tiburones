library(vegan)
library(gclus)
source("coldiss.R")
#setwd("C:/Users/corte/Dropbox/maestría/RESULTADOS/Coeficientes de similitud/")

## DATOS ==========================================================
datos.380 <- read.csv("datos_spp_380.csv")
datos.380$Mes <- factor(datos.380$Mes, levels = levels(datos.380$Mes)[ c(2,3,4,6,1,7,5)])
#datos.380[datos.380$Abundancia == 0, ] # Acceder a buceos sin tiburones
#datos.380 <- datos.380[ ! datos.380$Buceo %in% c(2,3,4,5,6,15,17,23,30,235,287,340,374,377,389), ]
REVILLA <- datos.380[ , 6:14]

## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

ab.REV <- table(unlist(REVILLA)) # cuenta los casos de cada abundancia de clase
barplot(ab.REV, las = 1, xlab = "Clases de abundancia", ylab = "Frecuencia", col = gray (5:0/5))
sum(REVILLA == 0) # numero de ausencias
sum(REVILLA == 0) / (nrow(REVILLA) * ncol(REVILLA)) # 0.7383
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# En cuantos sitios ocurre cada especie? Calcula las frecuencias relativas de las especies (proporcion de el numero de sitios) y grafica los histogramas
# calcula el numero de sitos donde cada espcie esta presente
REVILLA.pres <- apply(REVILLA > 0, 2, sum) # suma las presencias
sort(REVILLA.pres) # ordena los resultados en orden ascendente
REVILLA.relf <- 100*REVILLA.pres/nrow(REVILLA) # % de cada ocurrencia en 380
round(sort(REVILLA.relf), 1) # redondea a 1 digito
## - - - - - - - - - - - - - - - - 
par(mfrow = c(1, 2)) # dividir la ventana horizontalmente
hist(REVILLA.pres, main = "", right = F, las = 1, xlab = "Número de presencias", ylab = "Número de especies", breaks = seq(0, 380, by = 10), col= "bisque") # ocurrencias agrupadas en intervalos de clase de 10 ocurrencias 
hist(REVILLA.relf, main = "", right = F, las = 1, xlab = "Porcentaje de presencias (%)", ylab = "Número de especies", breaks = seq(0, 100, by = 5), col= "bisque") # frecuencias relativas de ocurrencias agrupadas en intervalos de clase de 10%
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## DIVERSITY ===============================================================
# library(vegan)
N0 <- rowSums(REVILLA > 0)     # RIQUEZA
H <- diversity(REVILLA)        # SHANNON
N1 <- exp(H)        # SHANNON diversity
N2 <- diversity(REVILLA, "inv")       # Simpson
J <- H / log(N0)
div <- data.frame(N0, H, N1, N2, J)

### TRANSFORMACION Y ESTANDARIZACION ========================================
## A- Transformaciones simples
REVILLA[1:10, ]
REVILLA.pa <- decostand(REVILLA, method = "pa") # Transformacion de abu. a P-A
REVILLA.pa[1:10, ]
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## B- Perfiles de especies: 2 metodos
# 1- Escalar las abundancias dividiendolas por el valor maximo de cada especie
REVILLA.scal <- decostand(REVILLA, "max")
REVILLA.scal[1:10, ]
apply(REVILLA.scal, 2, max)
# 2- Escalar las abundancias dividiendolas por los totales de la especie (abundancia relativa por especie)
REVILLA.relsp <- decostand(REVILLA, "total", MARGIN = 2)
REVILLA.relsp[1:10, ]
apply(REVILLA.relsp, 2, sum) # suma las columnas
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## C- Perfiles de sitios: 3 metodos
# 1- Escalar las abundancias dividiendolas por el valor total del buceo (abundancia relativa, o frecuencia relativa, por sitio)
REVILLA.rel <- decostand(REVILLA, "total")
REVILLA.rel[1:10, ]
apply(REVILLA.rel, 1, sum) # suma de las filas
# 2- Dar una longitud de 1 a cada fila del vector (Euclidean norm)
REVILLA.norm <- decostand(REVILLA, "normalize")
REVILLA.norm[1:10, ]
# verificar la normalidad de las filas del vector
norm <- function(x) sqrt(x%*%x)
apply(REVILLA.norm, 1, norm) # suma de las filas
# 3- Calcular las frecuencias relativas por filas y despúes aplicarles raiz cuadrada
REVILLA.hel <- decostand(REVILLA, "hellinger")
REVILLA.hel[1:10, ]
apply(REVILLA.hel, 1, norm) # suma de las filas
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## D- Estandarización por ambos, especies y sitios (perfiles dobles)
# 1- Transformación Chi-cuadrada
REVILLA.chi <- decostand(REVILLA, "chi.square")
REVILLA.chi[1:10, ]
# 2- Transformación Wisconsin
REVILLA.wis <- wisconsin(REVILLA)
REVILLA.wis[1:10, ]
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

### BOXPLOTS ===============================================================
par(mfrow = c(2, 2))

boxplot(REVILLA$tob, sqrt(REVILLA$tob), log1p(REVILLA$tob), 
        las = 1, main = "Transformacion simple", 
        names = c("raw data", "sqrt", "log"), col = "bisque")

boxplot(REVILLA.scal$tob, REVILLA.relsp$tob, 
        las = 1, main = "Estandarización por especies", 
        names = c("max", "total"), col = "Lightgreen")

boxplot(REVILLA.hel$tob, REVILLA.rel$tob, REVILLA.norm$tob, 
        las = 1, main = "Estandarización por sitios", 
        names = c("Hellinger", "total", "norm"), col = "Lightblue")

boxplot(REVILLA.chi$tob, REVILLA.wis$tob, 
        las = 1, main = "Estandarización doble", 
        names = c("Chi-cuadrada", "Wisconsin"), col = "orange")

# NUMERICAL ECOLOGY - SIMILITUD
REVILLA <- REVILLA[-8, ]
REVILLA.db <- vegdist(REVILLA)
head(REVILLA.db)