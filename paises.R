# Cargar el dataset desde el archivo CSV
data <- read.csv("datosDemograficos.csv")

head(data, n = 6)

dataFrameValores2 <- data.frame(
        Pais = Pais_dataset_p2,
        Codigo = Codigo_Pais_dataset_p2,
        Region = Region_dataset_p2
)       

head(dataFrameValores2)
head(valores[,c(1:5)])

dataframe3 <- merge(dataFrameValores2, valores, by.x="Codigo.Pais", by.y="Codigo")

head(dataframe3)
