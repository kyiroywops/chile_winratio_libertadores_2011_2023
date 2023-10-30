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

# 1. Equipos chilenos
chilean_teams <- c("Colo Colo", "Ñublense", "Universidad de Chile", "Universidad Católica", "Unión Española", "Everton CD", "Palestino", "Cobresal", "Deportes Iquique", "Huachipato", "O'Higgins")

# 2. Cantidad de goles
chilean_goals <- merged_goals[merged_goals$Club %in% chilean_teams, ]

# 3. Participaciones en años
chilean_participations <- aggregate(Edition ~ Home.Club, data=data[data$Home.Club %in% chilean_teams, ], FUN=function(x) length(unique(x)))

# 4. Porcentaje de partidos ganados
chilean_data_home <- data[data$Home.Club %in% chilean_teams, ]
chilean_data_away <- data[data$Away.Club %in% chilean_teams, ]

chilean_wins_home <- aggregate(Home.Score > AwayScore ~ Home.Club, data=chilean_data_home, sum)
chilean_wins_away <- aggregate(AwayScore > Home.Score ~ Away.Club, data=chilean_data_away, sum)
chilean_matches_home <- aggregate(Edition ~ Home.Club, data=chilean_data_home, length)
chilean_matches_away <- aggregate(Edition ~ Away.Club, data=chilean_data_away, length)

# Fusionar datos de victorias y partidos jugados
chilean_stats <- merge(chilean_wins_home, chilean_wins_away, by.x="Home.Club", by.y="Away.Club", all=TRUE)
chilean_stats <- merge(chilean_stats, chilean_matches_home, by.x="Home.Club", by.y="Home.Club", all=TRUE)
chilean_stats <- merge(chilean_stats, chilean_matches_away, by.x="Home.Club", by.y="Away.Club", all=TRUE)

# Calcular victorias y partidos jugados manualmente
chilean_stats$TotalWins <- ifelse(chilean_stats$`Home.Score > AwayScore` > 0, 1, 0) + ifelse(chilean_stats$`AwayScore > Home.Score` > 0, 1, 0)
chilean_stats$TotalMatches <- chilean_stats$Edition.x + chilean_stats$Edition.y

# Calcular porcentaje de victorias
chilean_stats$WinPercentage <- (chilean_stats$TotalWins / chilean_stats$TotalMatches) * 100

# Resultados
print(chilean_goals)
print(chilean_participations)
print(chilean_stats[, c("Home.Club", "WinPercentage")])

