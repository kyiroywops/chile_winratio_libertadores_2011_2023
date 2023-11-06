# Cargar la librería dplyr
library(dplyr)

# config para view

Sys.setlocale(category = "LC_ALL", locale = "C")
Sys.setenv(LANG = "en_US.UTF-8")


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
print("d. Primer gol local (Acceder a valores mediante nombre de columna arreglo):")
print(primer_gol_local)
print("")

# e. Retornar un vector por columna mediante signo ($)
vector_goles_locales <- data$Home.Score
print("e. Vector de goles locales(Retornar un vector por columna mediaten signo peso):")
print(vector_goles_locales)

# f. Retornar un vector por columna y especificando una fila mediante signo ($) e índice
gol_local_segunda_fila <- data$Home.Score[2]
print("f. Gol local segunda fila (Retornar un vector por columna y especificando una fila mediante signo ($) e índice):")
print(gol_local_segunda_fila)

# g. Retornar filas específicas con todas las columnas
filas_especificas <- data[c(1, 3, 5), ]
print("g. Filas específicas con todas las columnas:")
print(filas_especificas)

# j. Filtrar registros mediante valor String (Character) y calcular puntos totales
equipos_chilenos <- c("Colo Colo", "Ñublense", "Universidad de Chile", "Universidad Católica", "Unión Española", "Everton CD", "Palestino", "Cobresal", "Deportes Iquique", "Huachipato", "O'Higgins")
data_chilenos <- data %>%
  filter(Home.Club %in% equipos_chilenos | Away.Club %in% equipos_chilenos) %>%
  mutate(PuntosTotales = case_when(
    Home.Club %in% equipos_chilenos & Home.Score > AwayScore ~ 3,
    Home.Club %in% equipos_chilenos & Home.Score == AwayScore ~ 1,
    Away.Club %in% equipos_chilenos & AwayScore > Home.Score ~ 3,
    Away.Club %in% equipos_chilenos & AwayScore == Home.Score ~ 1,
    TRUE ~ 0
  ))

print("Filtrar registros mediante valor String, calcular puntos totales para cada equipo chileno y agregar columna PuntosTotales:")
View(data_chilenos)
print("")

# k. Filtrar registros por más de un argumento mediante operador lógico
partidos_interesantes <- data_chilenos %>%
  filter(Home.Score > 2 & AwayScore > 2)

print("h. Filtrar registros por más de un argumento mediante operador lógico:")
View(partidos_interesantes)
print("")

# l. Filtrar registros mediante valor String (Character)
equipos_de_Chile <- data_chilenos

print("l. Filtrar registros mediante valor String (Character):")
View(equipos_de_Chile)
print("")

# m. Realizar 3 gráficos
# ... (tu código para gráficos, que no se ha modificado)

# Visualizar datos después de operaciones
View(data_chilenos)
View(partidos_interesantes)
View(equipos_de_Chile)
