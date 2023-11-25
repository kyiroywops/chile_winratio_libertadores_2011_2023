# Instalar y cargar los paquetes necesarios
library(MASS)
library(tidyverse)
library(dplyr)
library(C50)
library(caret)
library(e1071)

# 1. Identificar el conjunto de datos que se utilizará
# Ya has identificado el conjunto de datos: "libertadores-results-ds.csv"

# 2. Cargar los datos a una variable
data <- read.csv("libertadores-results-ds.csv")

# 3. Eliminar atributos que no sean necesarios utilizar
# Supongamos que deseamos eliminar las columnas "Date" y "Edition"
data <- data %>% select(-Date, -Edition)

# 4. Si tiene campos en inglés, cámbielos a español
# Supongamos que queremos cambiar el nombre de la columna "Home.Club" a "EquipoLocal"
data <- data %>% rename(EquipoLocal = Home.Club)

# 5. Seleccione un campo como clase y cambie valores 0 y 1 según corresponda
# Supongamos que deseamos predecir si el equipo local ganó o perdió
# Creamos una nueva columna "Resultado" donde 1 representa una victoria del equipo local y 0 representa una derrota
data <- data %>% mutate(Resultado = ifelse(Home.Score > AwayScore, 1, 0))

# 6. Definir datos de entrenamiento con un 70%
set.seed(123)  # Establecer una semilla para la reproducibilidad
indice_entrenamiento <- createDataPartition(data$Resultado, p = 0.7, list = FALSE)
datos_entrenamiento <- data[indice_entrenamiento, ]
datos_prueba <- data[-indice_entrenamiento, ]

# 7. Convertir la variable "Resultado" a factor
datos_entrenamiento$Resultado <- factor(datos_entrenamiento$Resultado)

# 8. Aplique el proceso de aprendizaje o entrenamiento de su clasificación
modelo_arbol <- C5.0(Resultado ~ ., data = datos_entrenamiento)

# 9. Visualice el árbol de decisiones usando plot
plot(modelo_arbol)

# 10. Una vez entrenado el algoritmo, realice la clasificación usando predict
predicciones <- predict(modelo_arbol, datos_prueba)

# 11. Genere una matriz de confusión a través del paquete caret
library(caret)
matriz_confusion <- confusionMatrix(predicciones, datos_prueba$Resultado)

# 12. Una vez obtenidos los datos, analice e interprete la información obtenida
print(matriz_confusion)
