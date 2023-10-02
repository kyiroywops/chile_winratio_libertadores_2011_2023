# Cargar el dataset desde el archivo CSV
data <- read.csv("libertadores-results-ds.csv")

# Calcular goles como local
home_goals <- aggregate(Home.Score ~ Home.Club, data=data, sum)

# Calcular goles como visitante
away_goals <- aggregate(AwayScore ~ Away.Club, data=data, sum)

# Fusionar los datos de goles como local y visitante
merged_goals <- merge(home_goals, away_goals, by.x="Home.Club", by.y="Away.Club", all=TRUE)

# Reemplazar NA con 0
merged_goals[is.na(merged_goals)] <- 0

# Calcular el total de goles
merged_goals$TotalGoals <- merged_goals$Home.Score + merged_goals$AwayScore

# Seleccionar solo las columnas relevantes y renombrar
merged_goals <- merged_goals[, c("Home.Club", "TotalGoals")]
colnames(merged_goals) <- c("Club", "TotalGoals")

# Ordenar el dataframe de mayor a menor según TotalGoals
merged_goals <- merged_goals[order(-merged_goals$TotalGoals), ]

# Imprimir el resultado
print(merged_goals)
