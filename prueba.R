# 1. Cargar los paquetes necesarios
library(MASS)
library(tidyverse)
library(dplyr)
library(C50)
library(caret)
library(e1071)

# 2. Cargar el conjunto de datos con la codificación UTF-8
datosLibertadores <- read_csv("libertadores-results-ds.csv", locale = locale(encoding = "UTF-8"))

# Limpiar caracteres especiales en las columnas de texto
clean_text <- function(text) {
    text <- iconv(text, to = "ASCII//TRANSLIT")
    return(text)
}

datosLibertadores$Round <- clean_text(datosLibertadores$Round)
datosLibertadores$Date <- clean_text(datosLibertadores$Date)
datosLibertadores$`Home Club` <- clean_text(datosLibertadores$`Home Club`)
datosLibertadores$`Away Club` <- clean_text(datosLibertadores$`Away Club`)



# 5. Si tiene campos en ingles cámbielos a español
# Se renombran las columnas de inglés a español para mejorar la interpretación
datosLibertadores <- datosLibertadores %>% # nolint
  rename(
    Edición = Edition,
    Ronda = Round,
    Fecha = Date,
    Club_Local = `Home Club`,
    Club_Visitante = `Away Club`,
    Goles_Local = `Home Score`,
    Goles_Visitante = AwayScore
  )

# 4. Eliminar atributos que no sean necesarios utilizar
# Se elimina la columna 'Fecha' ya que no será utilizada en el modelo
datosLibertadores <- datosLibertadores %>% select(-Fecha)

# 6. Seleccione un campo como clase si tiene datos en texto cambiar valores 0 y 1 según corresponda.
# Crear una columna como clase y cambiar valores a 0 y 1
datosLibertadores$Resultado_Local <- ifelse(datosLibertadores$Goles_Local > datosLibertadores$Goles_Visitante, 1, 0)
datosLibertadores$Resultado_Local <- as.factor(datosLibertadores$Resultado_Local)

# Convertir variables categóricas en variables dummy
library(fastDummies)
datosLibertadores <- dummy_cols(datosLibertadores, select_columns = c("Club_Local", "Club_Visitante"), remove_selected_columns = TRUE)


# 6. Definir datos de entrenamiento con un 70%
# 7. Definir datos de prueba con un 30%
# Se divide el conjunto de datos en 70% para entrenamiento y 30% para pruebas
set.seed(42)
index <- createDataPartition(datosLibertadores$Resultado_Local, p = 0.7, list = FALSE)
train_data <- datosLibertadores[index,]
test_data <- datosLibertadores[-index,]

# 9. Aplicar el proceso de aprendizaje o entrenamiento de clasificación
model <- C50::C5.0(Resultado_Local ~ ., data = train_data)

# 10. Visualice el árbol de decisiones usando plot
# Visualizar el árbol de decisiones
library(partykit)
model_party <- as.party(model)
plot(model_party)


# 11. Una vez entrenado el algoritmo debe realizar la clasificación usando predict # nolint
# Realizar la clasificación usando predict y generar una matriz de confusión
predictions <- predict(model, test_data)
confusion_matrix <- confusionMatrix(predictions, test_data$Resultado_Local)
print(confusion_matrix)




# 12. Genere una matriz de confusión a través del paquete caret

# Imprimir la matriz de confusión para tener una vista detallada de los resultados
print(confusion_matrix)

# Calcular y mostrar métricas clave
exactitud <- sum(predictions == test_data$Resultado_Local) / length(predictions)
print(paste("Exactitud (Accuracy): ", exactitud))

# Calcular y mostrar la sensibilidad y especificidad
sensibilidad <- confusion_matrix$byClass["Sensitivity"]
especificidad <- confusion_matrix$byClass["Specificity"]
print(paste("Sensibilidad (Sensitivity): ", sensibilidad))
print(paste("Especificidad (Specificity): ", especificidad))

# Interpretación:
# - 'Exactitud' indica el porcentaje general de predicciones correctas.
# - 'Sensibilidad' indica la proporción de casos positivos reales que se identificaron correctamente.
# - 'Especificidad' indica la proporción de casos negativos reales que se identificaron correctamente.

