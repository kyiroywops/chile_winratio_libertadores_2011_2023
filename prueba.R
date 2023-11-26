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

# 4. Renombrar las columnas al español
datosLibertadores <- datosLibertadores %>%
    rename(
        Edición = Edition,
        Ronda = Round,
        Fecha = Date,
        Club_Local = `Home Club`,
        Club_Visitante = `Away Club`,
        Goles_Local = `Home Score`,
        Goles_Visitante = AwayScore
    )



# 4. Crear una columna como clase y cambiar valores a 0 y 1
datosLibertadores$Resultado_Local <- ifelse(datosLibertadores$Goles_Local > datosLibertadores$Goles_Visitante, 1, 0)
datosLibertadores$Resultado_Local <- as.factor(datosLibertadores$Resultado_Local)

# 5. Convertir variables categóricas en variables dummy
library(fastDummies)
datosLibertadores <- dummy_cols(datosLibertadores, select_columns = c("Club_Local", "Club_Visitante"), remove_selected_columns = TRUE)

# Eliminar la columna Fecha si aún no lo has hecho
datosLibertadores <- datosLibertadores %>% select(-Fecha)

# 6. Dividir los datos en entrenamiento (70%) y prueba (30%)
set.seed(42)
index <- createDataPartition(datosLibertadores$Resultado_Local, p = 0.7, list = FALSE)
train_data <- datosLibertadores[index,]
test_data <- datosLibertadores[-index,]

# 7. Aplicar el proceso de aprendizaje o entrenamiento de clasificación
model <- C50::C5.0(Resultado_Local ~ ., data = train_data)


# 7. Visualizar el árbol de decisiones
library(partykit)
model_party <- as.party(model)
plot(model_party)

# 8. Realizar la clasificación usando predict y generar una matriz de confusión
predictions <- predict(model, test_data)
confusion_matrix <- confusionMatrix(predictions, test_data$Resultado_Local)
print(confusion_matrix)




# 9. Analizar e interpretar la información obtenida

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

# Análisis adicional basado en los resultados:
# - ¿El modelo es eficaz para predecir victorias, empates o derrotas?
# - ¿Hay algún patrón en los errores que el modelo está cometiendo?
# - ¿Cómo podrían estos resultados ser útiles en un contexto real, como para predecir resultados de partidos futuros?
# - Consideraciones sobre el sobreajuste (overfitting) y cómo el modelo podría generalizarse a nuevos datos no vistos.

# Notas adicionales:
# Este análisis depende en gran medida de tus datos y resultados específicos. Adapta este esquema según lo que observes en tus datos y lo que consideres importante destacar.