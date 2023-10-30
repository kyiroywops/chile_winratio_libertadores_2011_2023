# Importar el archivo CSV
data <- read.csv("libertadores-results-ds.csv")

# a. Mostrar la cantidad de filas y columnas
num_filas <- nrow(data)
num_columnas <- ncol(data)

# b. Mostrar las primeras y últimas filas
head_data <- head(data, n = 6)
tail_data <- tail(data, n = 6)

# c. Acceder a valores mediante posición índice numérico
primer_valor <- data[1, 1]

# d. Acceder a valores mediante nombre de columna
primer_gol_local <- data$Home.Score[1]

# e. Retornar un vector por columna mediante signo ($)
vector_goles_locales <- data$Home.Score

# f. Retornar un vector por columna y especificando una fila mediante signo ($) e índice
gol_local_segunda_fila <- data$Home.Score[2]

# g. Retornar filas específicas con todas las columnas
filas_especificas <- data[c(1, 3, 5), ]

# h. Realizar Operaciones Aritméticas en DataFrame (Añadir una nueva columna)
data$Total_Goles <- data$Home.Score + data$AwayScore

# i. Realizar una Eliminación de Columnas añadidas mediante NULL
data$Total_Goles <- NULL

# j. Realizar Filtrado de valores de un dataframe añadiendo operador lógico
partidos_con_mas_de_3_goles <- data[data$Home.Score + data$AwayScore > 3, ]

# k. Filtrar registros por más de un argumento mediante operador lógico
partidos_interesantes <- data[data$Home.Score > 2 & data$AwayScore > 2, ]

# l. Filtrar registros mediante valor String (Character)
equipos_de_Chile <- data[data$Home.Club %in% c("Colo Colo", "Ñublense", "Universidad de Chile", "Universidad Católica", "Unión Española", "Everton CD", "Palestino", "Cobresal", "Deportes Iquique", "Huachipato", "O'Higgins"), ]

# m. Realizar 3 gráficos
# Gráfico de Dispersión
plot(data$Home.Score, data$AwayScore, main = "Goles de equipos chilenos en Libertadores", xlab = "Goles de local", ylab = "Goles de visitante")

# Gráfico de Barras
barplot(table(data$Home.Club), main = "Número de partidos por equipo local chileno", xlab = "Equipos", ylab = "Número de partidos")

# Gráfico de Barras con filtro
equipos_chilenos <- c("Colo Colo", "Ñublense", "Universidad de Chile", "Universidad Católica", "Unión Española", "Everton CD", "Palestino", "Cobresal", "Deportes Iquique", "Huachipato", "O'Higgins")
barplot(table(data[data$Home.Club %in% equipos_chilenos, ]$Home.Club), main = "Número de partidos por equipo chileno local", xlab = "Equipos", ylab = "Número de partidos")

# n. Utilizar facet_wrap para dividir gráficos
library(ggplot2)
ggplot(data, aes(x = Home.Score, y = AwayScore)) +
  geom_point() +
  facet_wrap(~Edition) +
  ggtitle("Goles de equipos chilenos en Libertadores por edición")

# o. Generar gráfico por densidad
density_plot <- ggplot(data, aes(x = Home.Score)) +
  geom_density() +
  ggtitle("Distribución de Goles de Equipos Chilenos de Local en Libertadores")
print(density_plot)
