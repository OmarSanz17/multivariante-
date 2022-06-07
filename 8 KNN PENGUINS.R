rm(list=ls())
#--------------------------------
# Ejercicio Penguins
#--------------------------------

#1.- Descargar la matriz penguins.
library(readxl)
ruta = "penguins.xlsx"
Z<-as.data.frame(read_excel(ruta))[,c(4,5,6,7,2)]
Z$especie = as.factor(Z$especie)
colnames(Z)

# Definir la matriz de datos y la variable respuesta
# Con las clasificaciones
x<-Z[,1:4]
y<-Z[,5]

# Se definen las variables y observaciones
n<-nrow(x)
p<-ncol(x)
dim(Z)

# Grafico scatter plot
# Creacion de un vector de colores
col.iris<-c("blue","green","orange")[y]
col.iris
pairs(x, main="Data set Pingüinos, Adelie (azul),Gentoo (verde), Chinstrap (naranja)", 
      pch=19,col=col.iris)

library(class)
# Se fija una "semilla" para tener valores iguales
set.seed(64)

# creacion de los ciclos
# para k=1 hasta k=20
# Selecciona el valor de k que tenga el error
# mas bajo.

# Inicialización de una lista vacia de tamaño 20
knn.class<-vector(mode="list",length=20)
knn.tables<-vector(mode="list", length=20)

# Clasificaciones erroneas
knn.mis<-matrix(NA, nrow=20, ncol=1)
knn.mis
# Se hace el ciclo
for(k in 1:20){
  knn.class[[k]]<-knn.cv(x,y,k=k)
  knn.tables[[k]]<-table(y,knn.class[[k]])
  # la suma de las clasificaciones menos las correctas
  knn.mis[k]<- n-sum(y==knn.class[[k]])
}
knn.mis

# Numero optimo de k-vecinos
which(knn.mis==min(knn.mis))
knn.tables[[1]]

# el mas eficiente es k=14
# se señala el k mas eficiente
k.opt<-1
knn.cv.opt<-knn.class[[k.opt]]
knn.cv.opt

# tabla de contingencia con las clasificaciones buenas y malas
knn.tables[[k.opt]]

# cantidad de observaciones mal clasificadas
knn.mis[k.opt]

# Error de clasificacion (MR)
knn.mis[k.opt]/n

# Grafico de clasificaciones correctas y erroneas
col.knn.iris<-c("indianred1","black")[1*(y==knn.cv.opt)+1]
pairs(x, main="Clasificación kNN de Iris",
      pch=19, col=col.knn.iris)


#5.1.- ¿Cual es numero optimo de k-vecinos cercanos?
#5.2.- ¿Cual es la cantidad de observaciones mal clasificadas?
#5.3.- ¿Cual es el ratio de mala clasificación (MR)?
