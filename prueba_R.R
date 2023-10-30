# Importar el archivo CSV

# a. Mostrar el archivo CSV
data <- read.csv("libertadores-results-ds.csv")

#  b.	Aplicar funciones para analizar el archivo, Nrow(),Ncol(), Head(), Tail(), Str() y Summary()
num_filas <- nrow(data)
num_columnas <- ncol(data)

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

# Filtrar equipos que pertenecen al fútbol chileno en la segunda gráfica
equipos_chilenos <- c("Colo Colo", "Ñublense", "Universidad de Chile", "Universidad Católica", "Unión Española", "Everton CD", "Palestino", "Cobresal", "Deportes Iquique", "Huachipato", "O'Higgins")

# Modificar el gráfico de barras para incluir equipos chilenos
barplot(table(equipos_de_Chile$Home.Club), main = "Número de partidos por equipo local chileno", xlab = "Equipos", ylab = "Número de partidos")

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
