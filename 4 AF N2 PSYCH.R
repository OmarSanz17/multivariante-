
#_____________________Analisis Facotrial psych_________________
# Librerías 
library(psych)
library(polycor)
library(ggcorrplot)

# Extraccion de datos 
#Se encuentra dentro de la paquetería **psych**
x <- bfi

## Exploracion de la matriz 
dim(x)
#1. Tipos de variables 
str(x)
#2. Nombre de las variables 
colnames(x)
#3. Creación de una nueva base de datos donde se incluyen las variables de 1 a 25 y usamos 200 observaciones 
x1<-bfi[1:200,1:25]

# Matriz de correlaciones 
R<- hetcor(x1)$correlations

#4.Gráfico de correlaciones 
ggcorrplot(R,type="lower",hc.order= TRUE)

# Factorización de la matriz de correlaciones 

#Se utiliza la prueba de esfericidad  de Bartlett.
prueba_Bartlett<- cortest.bartlett(R)

#5. Visualización del p-valor
prueba_Bartlett$p.value

#Dado que el p-valor resulta significativo **NO** se rechaza H0  

## Criterio Kaiser-Meyer-Olkin
#Permite identificar si los datos que se van a analizar son adecuados para un análisis factorial.
#0.00 a 0.49 No adecuados 
#0.50 a 0.59 Poco adecuados 
#0.60 a 0.69 Aceptables 
#0.70 a 0.89 Buenos 
#0.90 a 1.00 Excelente 
KMO(R)
#Overall MSA =  0.76  son buenos para continuar con el análisis 


# Extracción de factores 

#minres : minimo residuo 
#mle : max verosimilitud
#pfa: ejesprincipales 
#alpha: alfa 
#minchi: minimos cuadrdos 
#minrank: rango minimo 

#6. Modelo varimax
modelo1<- fa(R,nfactor=3,rotate = "none",fm = "mle")

#7. Modelo dos 
modelo2<- fa(R,nfactor=3,rotate = "none",fm = "minres")

#Extraer el resultado de las Comunalidades, allí se encuentra la proporción de varianza explicada. Se interpreta de tal forma que números cercanos a 1 están bastante bien explicadas por los factores comunes, entre más comunalidades altas aya en el factor este explica mejor la variable y el análisis en consecuencia será mejor.
C1<-sort(modelo1$communality,decreasing = TRUE)
C2<-sort(modelo2$communality,decreasing = TRUE)

#combinar los resultados para comparar 
head(cbind(C1,C2))

#8. Unicidad del modelo 1
u1<- sort(modelo1$uniquenesses,decreasing = TRUE)

#9. Unicidad del modelo 2 
u2<- sort(modelo2$uniquenesses,decreasing = TRUE)

#10. Comparación 
head(cbind(u1,u2))

#Para elegir el numero de los factores
scree(R)

# Rotación de la matriz 
library(GPArotation)
rot<-c("None", "Varimax", "Quartimax", "Promax")
bi_mod<-function(tipo){
  biplot.psych(fa(x1, nfactors = 2,  
                  fm= "minres", rotate=tipo),
               main = paste("Biplot con rotación", tipo),
               col=c("#FFB6C1","#87CEFA","#87CEFA"), pch=c(21,18), group=bfi[,"gender"])
}
sapply(rot,bi_mod)


#Para esto utilizamos un gráfico de árbol  

modelo_varimax<-fa(R,nfactor = 5,
                   rotate = "varimax",
                   fm="minres")
fa.diagram(modelo_varimax)

#Visualización de la matriz de carga rotada. 
print(modelo_varimax$loadings,cut=0)

