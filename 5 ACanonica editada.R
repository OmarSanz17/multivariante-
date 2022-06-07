rm(list=ls())
#_________ Analisis Canonico _______________
# Instalar paqueterias
library(tidyverse)
library(readxl)
library(palmerpenguins)
#-------------------------------
#   Preparacion de la matriz
#-------------------------------
# Se utiliza la matriz penguins.xlsx
# Importar la matriz de datos.
# Mando a llamar a la base
penguins=as.data.frame(read_excel("penguins.xlsx"))
head(penguins)

# Exploracion de la matriz

dim(penguins)
colnames(penguins)
str(penguins)
anyNA(penguins)

# Escalamiento de la matriz

# Generacion de variables X
X <- penguins %>% 
  select(grosor_pico_mm, largo_pico_mm) %>%
  scale()
head(X)

# Generacion de variables Y
Y <- penguins %>%
  select(largo_aleta_mm,masa_corporal_g) %>%
  scale()
head(Y)

#----------------------------------
# Analisis canonico con un par de variables
#----------------------------------

# Libreria
library(CCA)

# Analisis
ac<-cancor(X,Y)

# Visualizacion de la matriz X
ac$xcoef

# Visualizacion de la matriz Y
ac$ycoef

# Visualizacion de la correlacion canonica
ac$cor

# Obtencion de la matriz de variables canonicas
# Se obtiene multiplicando los coeficientes por
# cada una de las variables (X1 y Y1)
ac1_X <- as.matrix(X) %*% ac$xcoef[, 1]
ac1_Y <- as.matrix(Y) %*% ac$ycoef[, 1]

#Visualizacion de los primeros 20 datos
ac1_X[1:20,]
ac1_Y[1:20,]


# Correlacion canonica entre variable X1 y Y1
cor(ac1_X,ac1_Y)

# Verificacion de la correlacion canonica
assertthat::are_equal(ac$cor[1], 
                      cor(ac1_X,ac1_Y)[1])

#--------------------------
# Analisis canonico con dos pares de variables
#-----------------------------

# Calculo de las variables X2 y Y2
ac2_X <- as.matrix(X) %*% ac$xcoef[, 2]
ac2_Y <- as.matrix(Y) %*% ac$ycoef[, 2]

# Agregamos las variables generadas a la matriz
# original de penguins

ac_df <- penguins %>% 
  mutate(ac1_X=ac1_X,
         ac1_Y=ac1_Y,
         ac2_X=ac2_X,
         ac2_Y=ac2_Y)

# Visualizacion de los nombres de las variables
colnames(ac_df)

# Generacion del grafico scater plot para la
# visualizacion de X1 y Y1
ac_df %>% 
  ggplot(aes(x=ac1_X,y=ac1_Y))+
  geom_point(color="#551A8B")

# Generacion de un boxplot
ac_df %>% 
  ggplot(aes(x=especie,y=ac1_X, color=especie))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)+
  ggtitle("Variable Canónica X1 contra Especie")

# Interpretación: se observa una correlacion entre
# la variable canónica X1 y la variable latente Especie

ac_df %>% 
  ggplot(aes(x=especie,y=ac1_Y, color=especie))+
  geom_boxplot(width=0.5)+
  geom_jitter(width=0.15)+
  ggtitle("Variable Canónica Y1 contra Especie")

ac_df %>% 
  ggplot(aes(x=ac1_X,y=ac1_Y, color=especie))+
  geom_point()+
  ggtitle("Variable Canónica X1 contra Y1")

# Scarter plot con las variables canonicas
# X2 y Y2 separadas por genero.

ac_df %>% 
  ggplot(aes(x=ac2_X,y=ac2_Y, color=genero))+
  geom_point()+
  ggtitle("Variable Canónica X2 contra Y2")

#Interpretacion: No de identifica correlacion
# entre el conjunto de variables X2 y Y2 separadas
# por genero.

