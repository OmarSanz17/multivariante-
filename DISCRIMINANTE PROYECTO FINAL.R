# ______________ Analisis discriminante lineal ______________
#Librerias a utilizar 
library(ggplot2)
library(MVN)
library(readxl)
library(MASS)
library(Hotelling)
library(scatterplot3d)
library(klaR)
library(biotools)
library(scatterplot3d)
library(tidyverse)
#ANALISIS DISCRIMINANTE LINEAL
#EJERCICIO DE ANALISIS DISCRIMINANTE PARA LA BASE DE NUESTROS DATOS
#LIMPIO LA CONSOLA
rm(list = ls())
#CARGO LOS DATOS DE LOS CRANEOS
rut = "BASE DE CRANEOS DISCRIMINANTE LINEAL.xlsx"
datos <- as.data.frame(read_excel(rut))
attach(datos)
head(datos)
#CONVIERTO TIPO EN VARIABLE DE TIPO FACTOR
datos$TIPO = as.factor(datos$TIPO)
#DIMENSION DE LA MATRIZ DE DATOS
dim(datos)
#NOMBRE DE LAS VARIABLES
names(datos)
#TIPO DE VARIABLES
str(datos)
#DETECCION DE VALORES FALTANTES NA'S
anyNA(datos)
###########################

#Estadisticas descriptivas
## Medias y varianzas
#Media
m1 = tapply(datos$LONG , datos$TIPO , mean)
m2 = tapply(datos$ANCH , datos$TIPO , mean) 
m3 = tapply(datos$ALT , datos$TIPO , mean) 
m4 = tapply(datos$ALT.C , datos$TIPO , mean) 
m5 = tapply(datos$ANCH.C , datos$TIPO , mean) 
medias1 = cbind.data.frame(m1,m2,m3,m4,m5)
names(medias1) = c("Longitud","Ancho","Altura","Altura de cara","Ancho de Cara")
Grupo = c("Sikkim","Lhasa")
medias1 = cbind.data.frame(medias1,Grupo)
medias1
#Desviacion estandar
sd1 = tapply(datos$LONG , datos$TIPO , sd)
sd2 = tapply(datos$ANCH , datos$TIPO , sd) 
sd3 = tapply(datos$ALT , datos$TIPO , sd) 
sd4 = tapply(datos$ALT.C , datos$TIPO , sd) 
sd5 = tapply(datos$ANCH.C , datos$TIPO , sd) 
sds1 = cbind.data.frame(sd1,sd2,sd3,sd4,sd5)
names(sds1) = c("Longitud","Ancho","Altura","Altura de cara","Ancho de Cara")
Grupo = c("Sikkim","Lhasa")
sds1 = cbind.data.frame(sds1,Grupo)
sds1

###########################################
#Tratamiento de la matriz
#OBTENGO MI MUESTRA DE ENTRENAMIENTO
set.seed(2510)
#Sujetos por grupo
table(datos$TIPO)
#Se crea la muestra de entrenamiento del 80%
dentre = sample_frac(datos,.8)
dentre

#El resto de los datos forman la base de prueba
dpru = setdiff(datos,dentre)
dpru1 = data.frame(dpru[,1:5],clase=as.vector(dpru[,6]))
dpru1
ir_o = dentre[order(dentre$TIPO),]
ir_o

########################

# RESULTADOS DESCRIPTIVOS
#Hago el grafico de dispersion en pares de variables
pairs(x = datos[,1:5],
      col = c("red","blue")[datos$TIPO],pch = 19)
#Scatter plot en 3 dimensiones
scatterplot3d(x = datos$LONG, y = datos$ANCH, z = datos$ALT,
          xlab = "Longitud", ylab =  "Ancho", zlab= "Alto",
            color = datos$TIPO,angle = 430 )
#Scatter plot con otras variables en 3 dimensiones
scatterplot3d(x = datos$LONG, y = datos$ALT.C, z = datos$ANCH.C,
              xlab = "Longitud", ylab =  "Altura del cara", zlab= "Ancho del cara",
              color = datos$TIPO,angle = 2200 )

###############################

# RESULTADOS INFERENCIALES 

#Grafico qqplot para las distancias de mahalanobis de los sujetos
pruebaroyston = mvn(data = datos[,-6],mvnTest ="royston",
                    multivariatePlot = "qq")
#Normalidad univariada
pruebaroyston$univariateNormality

#Normalidad Multivariada
pruebaroyston$multivariateNormality

#Prueba de homocedasticidad MULTIVARIADA
boxM(data = datos[,1:5],grouping = datos[,6])

#################

# Resultados sobre el modelo

#Armo el modelo
ir2.lda = lda(TIPO~.,datos)
ir2.lda
#Para discriminar de las observaciones internas
#Armo la matriz de confusion
predicciones <- predict(object = ir2.lda, newdata = dentre[,-6])
table(dentre$TIPO,predicciones$class,dnn=c("Clase real","Clase predicha"))

#Error de discriminacion del modelo
errorclas <- mean(dentre$TIPO!= predicciones$class)*100
paste("Error de discriminacion = ",errorclas,"%")

#Probabilidades a a priori y a posteriori de la muestra de entrenamiento
probs = predict(ir2.lda,ir_o,type ="prob")
probs=data.frame(probs)
Posicion= 1:nrow(probs)
probs=data.frame(cbind(probs,Posicion))
head(probs)
attach(probs)
#Grafico de las probabilidades del discriminante para los datos de entrenamiento
ggplot(probs, aes(Posicion,LD1)) +
  geom_point(aes(color = class),size=2) +
  geom_line(aes(y=0), size=1) +
  geom_text(label=rownames(probs))

#Clafsificando con la prueba 

#Armo la matriz de confusion
predicciones1 <- predict(object = ir2.lda, newdata = dpru1[,-6])
table(dpru1$clase,predicciones1$class,dnn = c("Clase real","Clase predicha"))

# ERROR DE CLASIFICACION DE LOS DATOS DE PRUEBA
error1_entre = mean(dpru1$clase != predicciones1$class)*100
paste("error de clasificacion =",error1_entre, "%")

#PROBABILIDADES A PRIORI Y A POSTERIORI DE LOS DATOS DE PRUEBA
probs = predict(ir2.lda,dpru1 ,type ="prob")
probs=data.frame(probs)
probs
Posicion= 1:nrow(probs)
probs=data.frame(cbind(probs,Posicion))
head(probs)
attach(probs)
#Grafico de las probabilidades del discriminante para los datos de prueba
ggplot(probs, aes(Posicion,LD1)) +
  geom_point(aes(color = class),size=2) +
  geom_line(aes(y=0), size=1) +
  geom_text(label=rownames(probs))









