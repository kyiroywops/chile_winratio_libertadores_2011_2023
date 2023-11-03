# a. Importar el archivo CSV
data <- read.csv("libertadores-results-ds.csv")

# b. Aplicar funciones para analizar el archivo
# Número de filas
num_filas <- nrow(data)

# Número de columnas
num_columnas <- ncol(data)

# Primeras filas
head_data <- head(data, n = 6)

# Últimas filas
tail_data <- tail(data, n = 6)

# Visualización de la estructura del dataframe
str_data <- str(data)

# Resumen estadístico del dataframe
summary_data <- summary(data)

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
equipos_chilenos <- c("Colo Colo", "Ñublense", "Universidad de Chile", "Universidad Católica", "Unión Española", "Everton CD", "Palestino", "Cobresal", "Deportes Iquique", "Huachipato", "O'Higgins")
data_chilenos <- data[data$Home.Club %in% equipos_chilenos, ]
partidos_con_mas_de_3_goles <- data_chilenos[data_chilenos$Home.Score + data_chilenos$AwayScore > 3, ]

# k. Filtrar registros por más de un argumento mediante operador lógico
partidos_interesantes <- data_chilenos[data_chilenos$Home.Score > 2 & data_chilenos$AwayScore > 2, ]

# l. Filtrar registros mediante valor String (Character)
equipos_de_Chile <- data_chilenos[data_chilenos$Home.Club %in% equipos_chilenos, ]

# m. Realizar 3 gráficos donde se definen los datos más relevantes
# Gráfico de Dispersión
plot(data_chilenos$Home.Score, data_chilenos$AwayScore, main = "Goles de Equipos Chilenos en Libertadores", xlab = "Goles de Equipos Chilenos como Local", ylab = "Goles de Equipos Chilenos como Visitante")

# Gráfico de Barras
barplot(table(data_chilenos$Home.Club), main = "Número de Partidos por Equipo Local en Libertadores", xlab = "Equipos", ylab = "Número de Partidos", cex.names = 0.2)

# Gráfico de Barras con filtro para equipos chilenos
barplot(table(equipos_de_Chile$Home.Club), main = "Número de Partidos de Equipos Chilenos como Local en Libertadores", xlab = "Equipos", ylab = "Número de Partidos", cex.names = 0.2)

# o. Generar gráfico por densidad
library(ggplot2)

# Generar gráfico por densidad
density_plot <- ggplot(data_chilenos, aes(x = Home.Score)) +
  geom_density() +
  labs(title = "Densidad de Goles de Equipos Chilenos de Local en Libertadores",
       x = "Goles de Equipos Chilenos como Local",
       y = "Densidad")  # Cambia "density" por "Densidad" aquí
print(density_plot)

