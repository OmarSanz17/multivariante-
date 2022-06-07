
#_______________Análisis Discriminante Lineal__________________

#Se carga la matriz de datos de iris
library(MASS)
Z<-as.data.frame(iris)
head(Z)

#Se hace una separación entre las variables numéricas y la variable categórica
x<-Z[,1:4]
y<-Z[,5]

#Se obtienen las dimensiones de la matriz de datos
n<-nrow(x)
p<-ncol(x)

#Se genera el modelo de discriminante lineal con validación cruzada (CV)
lda.iris<-lda(Z$Species~.,data=Z,CV=TRUE)

#Se observa el vector que contiene las clasificaciones que realiza el modelo
lda.iris$class

#Se realiza la matriz de confusión 
table.lda<-table(y,lda.iris$class)
table.lda
#Se obtiene su error de discriminación
mis.lda<- n-sum(y==lda.iris$class)
mis.lda/n

# Se localizan los sujetos que estan mal clasificados
col.lda.iris<-c("indianred1","black")[1*(y==lda.iris$class)+1]
pairs(x,main="Buena clasificación (negro), mala clasificación (rojo)",
      pch=19,col=col.lda.iris)
#Se observa la matriz de probabilidades con las que clasifica a cada sujeto el modelo
round(lda.iris$posterior,3)

#Gráfico de las probabilidades a posteriori calculadas
plot(1:n, lda.iris$posterior[,1],
     main="Probabilidades a posteriori",
     pch=20, col="#27408B",
     xlab="Número de observaciones", ylab="Probabilidades")
points(1:n,lda.iris$posterior[,2],
       pch=20, col="#8B4C39")
points(1:n,lda.iris$posterior[,3],
       pch=20, col="#2E8B57")
