setwd("~/Documents/cibpsi/dataAnalysis")

library(readr)
library(tidyverse)
library(markovchain)
library(stats)

numCadenas <- 6

data <- read_csv("files/data.csv", show_col_types = FALSE)


#filtroRT y valence!=NA
data$valence <- ifelse(!is.na(data$RT) & data$RT > 10, NA, data$valence)
data$valence <- ifelse(is.na(data$word), NA, data$valence)


#extraer participantes
participantes <- data["participant"] %>%
  distinct()

#dataframe para guardar valencias
valences <- data.frame(matrix(ncol = 0, nrow = 35))

#recorrer participantes
for (i in 1:nrow(participantes)){
  participantID <- participantes$participant[i]
  
  participantData <- data %>%
    filter(participant == participantID)
  
  #obtener todas las valencias de participante y guardarlas en dataframe valences
  for (i in 1:numCadenas){
    participantChainN <- participantData %>%
      filter(chainNumber == i & wordCount != 0)
    
    valences <- cbind(valences, participantChainN$valence)
  }
}

#transformar datos para hacer modelo de dos estados
datos_transformados <- as.data.frame(lapply(valences, function(x) ifelse(x>=5, "pos", "neg")))
lista_datos <- lapply(datos_transformados, as.character)

#ver modelo
ajuste_cadena <- markovchainFit(data = lista_datos)
matriz_transicion <- ajuste_cadena$estimate@transitionMatrix