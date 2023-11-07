# Cargar la librería dplyr
library(dplyr)
library(ggplot2)

# config para poder utilizar view

# Sys.setlocale(category = "LC_ALL", locale = "C")
# Sys.setenv(LANG = "es_ES.UTF-8")


# a. Importar el archivo CSV
data <- read.csv("libertadores-results-ds.csv")

# b. Aplicar funciones para analizar el archivo
# Número de filas
num_filas <- nrow(data)
print("b. Número de filas:")
print(num_filas)

# Número de columnas
num_columnas <- ncol(data)
print("b. Número de columnas:")
print(num_columnas)

# Primeras filas
head_data <- head(data, n = 6)
print("b. Primeras filas:")
print(head_data)

# Últimas filas
tail_data <- tail(data, n = 6)
print("b. Ultimas filas:")
print(tail_data)

# Visualización de la estructura del dataframe
str_data <- str(data)
print("b. Visualización de la estructura del dataframe:")
print(str_data)

# Resumen estadístico del dataframe
summary_data <- summary(data)
print("b. Resumen estadístico del dataframe:")
print(summary_data)

# c. Acceder a valores mediante posición índice numérico
primer_valor <- data[1, 1]
print("c. Primer valor:")
print(primer_valor)

# d. Acceder a valores mediante nombre de columna
primer_gol_local <- data$Home.Score[1]
print("d. Primer gol local (Acceder a valores mediante nombre de columna arreglo):") # nolint
print(primer_gol_local)


# e. Retornar un vector por columna mediante signo ($)
vector_goles_locales <- data$Home.Score
print("e. Vector de goles locales(Retornar un vector por columna mediaten signo peso):") # nolint
print(vector_goles_locales)

# f. Retornar un vector por columna y especificando una fila mediante signo ($) e índice # nolint
gol_local_segunda_fila <- data$Home.Score[2]
print("f. Gol local segunda fila (Retornar un vector por columna y especificando una fila mediante signo ($) e índice):") # nolint
print(gol_local_segunda_fila)

# g. Retornar filas específicas con todas las columnas
filas_especificas <- data[c(1, 3, 5), ]
print("g. Filas específicas con todas las columnas:")
print(filas_especificas)

# h. Realizar Operaciones Aritméticas en DataFrame (Añadir una nueva columna)
data$Total_Goles <- data$Home.Score + data$AwayScore
print("h. Realizar Operaciones Aritméticas en DataFrame (Añadir una nueva columna):")          # nolint
print(data$Total_Goles)

# i. Realizar una Eliminación de Columnas añadidas mediante NULL
data$Total_Goles <- NULL
print("i. Realizar una Eliminación de Columnas añadidas mediante NULL:")
print(data$Total_Goles)


# j. Filtrar registros mediante valor String (Character)
equipos_chilenos <- c("Colo Colo", "Ñublense", "Universidad de Chile", "Universidad Católica", "Unión Española", "Everton CD", "Palestino", "Cobresal", "Deportes Iquique", "Huachipato", "O'Higgins") # nolint
data_chilenos <- data[data$Home.Club %in% equipos_chilenos, ]

# Calcular puntos totales para cada equipo chileno
data_chilenos$PuntosTotales <- with(data_chilenos, {
  puntos_local <- ifelse(Home.Club %in% equipos_chilenos, ifelse(Home.Score > AwayScore, 3, ifelse(Home.Score == AwayScore, 1, 0)), 0)
  puntos_visitante <- ifelse(Away.Club %in% equipos_chilenos, ifelse(AwayScore > Home.Score, 3, ifelse(AwayScore == Home.Score, 1, 0)), 0)
  puntos_total <- puntos_local + puntos_visitante
  return(puntos_total)
})

print("Filtrar registros mediante valor String, calcular puntos totales para cada equipo chileno y agregar columna PuntosTotales:")
print(data_chilenos)


# k. Filtrar registros por más de un argumento mediante operador lógico
partidos_interesantes <- data_chilenos %>%
  filter(Home.Score > 2 & AwayScore > 2)

print("h. Filtrar registros por más de un argumento mediante operador lógico:")
print(partidos_interesantes)


# l. Filtrar registros mediante valor String (Character)
equipos_de_Chile <- data_chilenos # nolint

print("l. Filtrar registros mediante valor String (Character):")
print(equipos_de_Chile)


# m. Realizar 3 gráficos donde se definen los datos más relevantes
# Gráfico de Dispersión

# Calcular puntos totales para cada equipo chileno
data_chilenos$PuntosTotales <- with(data_chilenos, {
  puntos_local <- ifelse(Home.Club %in% equipos_chilenos, ifelse(Home.Score > AwayScore, 3, ifelse(Home.Score == AwayScore, 1, 0)), 0)
  puntos_visitante <- ifelse(Away.Club %in% equipos_chilenos, ifelse(AwayScore > Home.Score, 3, ifelse(AwayScore == Home.Score, 1, 0)), 0)
  puntos_total <- puntos_local + puntos_visitante
  return(puntos_total)
})

# Nuevo gráfico de dispersión relacionado con puntos totales
plot(data_chilenos$PuntosTotales, data_chilenos$Home.Score, main = "Puntos Totales vs. Goles de Equipos Chilenos en Libertadores", xlab = "Puntos Totales", ylab = "Goles de Equipos Chilenos como Local") # nolint: line_length_linter.

# Gráfico de Barras
barplot(table(data_chilenos$Home.Club), main = "Número de Partidos por Equipo Local en Libertadores", xlab = "Equipos", ylab = "Número de Partidos", cex.names = 0.29)

# Gráfico de Barras con filtro para equipos chilenos

# Filtrar los datos para incluir solo los tres equipos específicos
equipos_seleccionados <- c("Universidad de Chile", "Universidad Católica", "Colo Colo")
data_filtrada <- equipos_de_Chile[equipos_de_Chile$Home.Club %in% equipos_seleccionados, ]

# Crear el gráfico de barras con los datos filtrados
barplot(table(data_filtrada$Home.Club), 
        main = "Número de Partidos de Equipos Chilenos como Local en Libertadores",
        xlab = "Equipos",
        ylab = "Número de Partidos",
        cex.names = 0.8)


# n. Utilizar facet_wrap para dividir gráficos

# Crear un gráfico de densidad para cada equipo chileno
density_plot <- ggplot(data_chilenos, aes(x = PuntosTotales, fill = Home.Club)) +
  geom_density(alpha = 0.5) +
  labs(title = "Densidad de Puntos Totales de Equipos Chilenos en Libertadores",
       x = "Puntos Totales",
       y = "Densidad") +
  scale_fill_discrete(name = "Equipo") +
  facet_wrap(~ Home.Club, scales = "free")  # Divide por equipos

print(density_plot)




# o. Gráfico de densidad por equipo


density_plot <- ggplot(data_chilenos, aes(x = PuntosTotales, fill = Home.Club)) +
  geom_density(alpha = 0.5) +
  labs(title = "Densidad de Puntos Totales de Equipos Chilenos en Libertadores",
       x = "Puntos Totales",
       y = " distribución de los puntos totales de los equipos chilenos en la Copa Libertadores.") +
  scale_fill_discrete(name = "Equipo")

print(density_plot)

# Visualizar datos después de operaciones, filtrados, etc/ con view tambien
print(data_chilenos)
print(partidos_interesantes)
print(equipos_de_Chile)
