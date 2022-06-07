
#_____ Distancia de Mahalanobis____

# Cargar los datos
ventas= c( 1054, 1057, 1058, 1060, 1061, 1060, 1061, 
           1062, 1062, 1064, 1062, 1062, 1064, 1056, 
           1066, 1070)
clientes= c(63, 66, 68, 69, 68, 71, 70, 70, 71, 72, 72, 
            73, 73, 75, 76, 78)

# Utilizamos la función data.frame() para crear 
# un juego de datos en R
datos <- data.frame(ventas ,clientes)

dim(datos)
str(datos)
summary(datos)

#----------------------------------------------------
#    Calculo de la distancia
#----------------------------------------------------

# El método de distancia Mahalanobis mejora el 
# método clásico de distancia de Gauss 
# eliminando el efecto que pueden producir 
# la correlación entre las variables a analizar


# Determinar el número de outlier que queremos encontrar.
num.outliers <- 2

# Ordenar los datos de mayor a menor distancia, 
# según la métrica de Mahalanobis.
mah.ordenacion <- order(mahalanobis(datos, colMeans(datos), cov(datos)), decreasing=TRUE)
mah.ordenacion

# Generar un vector boleano los dos 
# valores más alejados segun la distancia Mahalanobis.
outlier2 <- rep(FALSE , nrow(datos))
outlier2[mah.ordenacion[1:num.outliers]] <- TRUE

# Resaltar con un punto relleno los 2 valores outliers.
colorear.outlier <- outlier2 *16

# Visualizar el gráfico con los datos destacando sus outlier.
plot(datos , pch=0)
points(datos , pch=colorear.outlier)

#----------------------------------
#   Ejercicio 2
#----------------------------------

require(graphics)

ma <- cbind(1:6, 1:3)
(S <-  var(ma))
mahalanobis(c(0, 0), 1:2, S)

x <- matrix(rnorm(100*3), ncol = 3)
stopifnot(mahalanobis(x, 0, 
                      diag(ncol(x))) == rowSums(x*x))

##- Here, D^2 = usual squared Euclidean distances

Sx <- cov(x)
D2 <- mahalanobis(x, colMeans(x), Sx)
plot(density(D2, bw = 0.5),
     main="Squared Mahalanobis distances, 
     n=100, p=3") ; rug(D2)
qqplot(qchisq(ppoints(100), df = 3), D2,
       main = expression("Q-Q plot of Mahalanobis" * ~D^2 *
                           " vs. quantiles of" * ~ chi[3]^2))
abline(0, 1, col = 'gray')

#------------------------------------------
#   Ejercicio 3
#-----------------------------------------

# Diseñar un ejercicio utilizando la distancia de Mahalanobis.

# 1.- Planteamiento del problema.
#Se desea encontrar si existen valores atipicos en la matriz de datos
#de la base de los palmer penguins a nivel multivariado, es por ello
#que se recurrirá a la distancia de Mahalanobis para encontrar
#los posibles valores atípicos y presentarlos en un gráfico 2d

# 2.- Simular los datos o utilizar una matriz precargada en R.

#Cargo la base de datos de los palmer penguins que esta incluida en el paquete "palmerpenguins"
library(palmerpenguins)
datos = as.data.frame(penguins)
head(datos)
#Quito valores faltantes
datos = datos[is.na(datos$bill_length_mm)==F,]
#Se extraen todos los pinwinos de la especie Gentoo debido a que los de la especie
#Chinstrap y Adelie presentan cierta homogeneidad entre si, mientras que los de Gentoo se
#Logran diferenciar muy bien de ellos.
datos = datos[datos$species!="Gentoo" ,]
dim(datos)
#Se cambian los nombres de las filas despues de remover a los pinguinos de la especie Gentoo
row.names(datos) = 1:nrow(datos)
#Extraigo unicamente las variables numericas continuas
datos = datos[,c(3,4,5,6)]
head(datos)
#Cambio el nombre de las columnas
names(datos) = c("L.Pico","P.Pico","Aleta","Masa")
#Reviso la dimension de la matriz procesada
dim(datos) #219 filas y 4 columnas
names(datos)
#Acercamiento Exploratorio en 3 dimensiones
#Se hace un gráfico de dispersion en 3d para observar el comportamiento de los sujetos
library(scatterplot3d)
zz <- scatterplot3d(x = datos[,1], y = datos[,2], z = datos[,3], 
                    xlab = "L.Pico", ylab = "P.Pico", zlab = "Aleta", 
                    pch = 1, color = as.numeric(datos$genotipo), grid = TRUE,
                    angle = 170)
zz.coords <- zz$xyz.convert(datos[,1], datos[,2], datos[,3]) 
text(zz.coords$x, 
     zz.coords$y,             
     labels = row.names(datos),               
     cex = .8, 
     pos = 2)  
head(datos)
#Deteccion de valores atipicos
outliers = mvn(data = datos[,-4] ,mvnTest= "hz",
               multivariateOutlierMethod = "quan")
outliers$univariateNormality


#Se alcanza a identificar en el gráfico de dispersion 3D que existen valores muy alejados del resto
#como el 14, el 28 y el 169

#Aplico el comando para obtener las distancias de mahalanobis
datosmahal = mahalanobis(datos,center = colMeans(datos), cov = cov(datos))
#Observo la distribución de las distancias mediante un histograma
hist(datosmahal,col = "Pink",main = "Histograma de las distancias de Mahalanobis",
     xlab = "Distancias de Mahalanobis") 
#El histograma revela la presencia de valores muy alejado del resto

#Realizo un gráfico de cajas para buscar los posibles valores outliers
boxplot(datosmahal,col = "pink") 
#Se encuentra que existen 4 observaciones que se alejan mucho de las demás.

#Se propone realizar un gráfico de las distancias de Mahalanobis 
plot(datosmahal) 
#Realizando un gráfico de dispersión de la variable se encuentra 1 dato muy alejado del resto.
#Procedemos a ordenar los datos y a quitar los primeros 4 que reportan los valores mas altos
dmsa = order(datosmahal,decreasing = T)[1:4]
#Quitamos los valores 169,81,39 y 14
limpios = datosmahal[-dmsa]

#Una vez quitadas las observaciones que aparecían como valores muy alejados,
#se procede a observar el comportamiento de los gráficos
#Revisamos el Histograma
hist(limpios,col = "Pink", main = "Histograma de los datos limpios",xlab =  "Distancias de Mahalanobis")
#Revisamos el gráfico de cajas
boxplot(limpios, col = "pink")
#El grafico de cajas ya no reporta ningun valor fuera de sus limites
#Revisamos el grafico de dispersion de las distancias de Mahalanobis
plot(limpios)
#Se puede observar que los datos ya tienen un comportamiento mas homogeneo

#Conclusion
#Al gráficar los datos para poder observar su comportamiento nos topamos con la limitante de que
#por mucho podemos gráficar 3 variables simultaneamente y suele suceder (como en este caso) que podemos
#llegar a tener 4 o mas variables que resultan de interes para nosotros y que no podemos alcanzar a representar
# al mismo tiempo por lo que la distancia de Mahalanobis resulta una opcion viable para condensar esa información
# multivaridada en un solo valor que resulta mas manejable como se pudo observar en el transcurso del ejemplo con
# la base de pinguinos en donde se pudo pasar de tener 3 dimensiones en el espacio para representar su comportamiento 
# a tener unicamente un solo valor que resumiera la información multivariante de los sujetos.
