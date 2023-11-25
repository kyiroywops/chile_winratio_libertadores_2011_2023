#install.packages("MASS")
library(MASS)
library(tidyverse)
#install.packages("dplyr")
library(dplyr)
#install.packages("c50")
library(C50)
#install.packages("caret")
library(caret)
#install.packages("e1071")
library(e1071)
# cargar los datos a una variable 
ds_cangrejos <-crabs  
head(ds_cangrejos)
summary(ds_cangrejos)
glimpse(ds_cangrejos)

#eliminir atributo index del dataset ds_cangrejos
ds_cangrejos <- ds_cangrejos %>%
  dplyr::select(-index)
glimpse(ds_cangrejos)
# cambiar los nombres de atributos a español
ds_cangrejos <- ds_cangrejos %>%
  rename(sexo=sex,ancho_frontal=FL)
glimpse(ds_cangrejos)
ds_cangrejos <- ds_cangrejos %>%
  rename(ancho_posterior=RW,largo_carapacho=CL, 
         ancho_carapacho=CW,profundidad_cuerpo=BD)
glimpse(ds_cangrejos)
# especie --> B: Blue=1, O: orange=0
# sexo --> M: masculino=1, F: femenino=0

ds_cangrejos <- ds_cangrejos %>%
  mutate(especie= ifelse(especie=="B",1,0),
sexo=ifelse(sexo=="M",1,0))
glimpse(ds_cangrejos)
# ahora se crearan el conjunto de datos de entrenamiento y prueba.
# para estos casos usaremos un 70% y 30% respectivamente
# Para garantizar que los datos sean reproducibles usamos la siguiente condición
set.seed(1987)
# generar 70% de numeros aleatorios
id_instancia <- sample(1:nrow(ds_cangrejos),nrow(ds_cangrejos)*.70)

# creamos el conjunto de entrenamiento:
ds_entrenamiento <-ds_cangrejos[id_instancia,]
ds_entrenamiento <-ds_entrenamiento %>%
  mutate(sexo=factor(sexo,levels=c(1,0)))

# creamos el conjunto de prueba
ds_prueba <- ds_cangrejos[-id_instancia,]
ds_prueba <- ds_prueba %>%
  mutate(sexo=factor(sexo,levels=c(1,0)))

glimpse(ds_cangrejos)
glimpse(ds_entrenamiento)
glimpse(ds_prueba)
# como conocer el numero de instancias en cada clase
# resumir las instancias por clases

print(ds_entrenamiento %>% count(sexo))
print(ds_prueba %>% count(sexo))
print(ds_cangrejos %>% count(sexo))

# clasificación
# clasificacion datos aprendizaje --> entrenamiento
#datos clasificacion --> prueba

clasificador_entrenado <- C5.0(y=ds_entrenamiento$sexo,
                              x=ds_entrenamiento %>%  dplyr::select(-sexo))
print(clasificador_entrenado)
summary(clasificador_entrenado)
plot(clasificador_entrenado)




