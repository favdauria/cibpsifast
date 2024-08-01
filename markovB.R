setwd("~/Documents/cibpsi/dataAnalysis")

library(readr)
library(tidyverse)
library(markovchain)
library(stats)

numCadenas <- 6

data <- read_csv("files/data.csv", show_col_types = FALSE)

#extraer participantes
participantes <- data["participant"] %>%
  distinct()

#filtroRT y valence!=NA
data$valence <- ifelse(!is.na(data$RT) & data$RT > 10, NA, data$valence)
data$valence <- ifelse(is.na(data$word), NA, data$valence)
matrices_transicion <- list()

#matriz para guardar datos
markovTransitions <- data.frame(pos_pos = numeric(),
                                pos_neg = numeric(),
                                neg_pos = numeric(),
                                neg_neg = numeric())

#recorrer participantes para obtener probabilidades de transicion de cada uno
for (i in 1:nrow(participantes)){
  participantID <- participantes$participant[i]
  valences <- data.frame(matrix(ncol = 0, nrow = 35))
  
  participantData <- data %>%
    filter(participant == participantID)
  
  for (i in 1:numCadenas){ #recorrer cadenas y guardar las valencias
    participantChainN <- participantData %>%
      filter(chainNumber == i & wordCount != 0)
    
    valences <- cbind(valences, participantChainN$valence)
  }
  
  #transformar datos para hacer modelo de dos estados
  datos_transformados <- as.data.frame(lapply(valences, function(x) ifelse(x>=5, "pos", "neg")))
  
  #calcular las probabilidades de transicion por cadena
  transiciones_cadenas <- data.frame(pos_pos = numeric(),
                                     pos_neg = numeric(),
                                     neg_pos = numeric(),
                                     neg_neg = numeric())
  
  #obtener datos de transiciones para cada una de las seis cadenas
  for (t in 1:numCadenas){
    lista_datos <- lapply(datos_transformados[i], as.character)
    ajuste_cadena <- markovchainFit(data = lista_datos)
    matriz_transicion_i <- ajuste_cadena$estimate@transitionMatrix

    pos_pos_value <- matriz_transicion_i[2,2]
    pos_neg_value <- matriz_transicion_i[2,1]
    neg_pos_value <- matriz_transicion_i[1,2]
    neg_neg_value <- matriz_transicion_i[1,1]
    
    transition_cadena <- c(pos_pos_value, pos_neg_value, neg_pos_value, neg_neg_value)
    transiciones_cadenas <- rbind(transiciones_cadenas, transition_cadena)
  }
  
  #arreglar nombres de columnas porque se cambian mágicamente luego de rbind
  colnames(transiciones_cadenas) <- c("pos_pos_transition", "pos_neg_transition", "neg_pos_transition", "neg_neg_transition")

  #calcular probabilidades de transicion promedio por participante
  transicion_participante <- c(
    mean(transiciones_cadenas$pos_pos_transition),
    mean(transiciones_cadenas$pos_neg_transition),
    mean(transiciones_cadenas$neg_pos_transition),
    mean(transiciones_cadenas$neg_neg_transition)
  )
  #añadir valores anteriores a dataframe
  markovTransitions <- rbind(transicion_participante, markovTransitions)
  
}

#arreglar nombres que se cambian mágicamente
colnames(markovTransitions) <- c("pos_pos_transition", "pos_neg_transition", "neg_pos_transition", "neg_neg_transition")


#variables para tests
pospos <- markovTransitions$pos_pos_transition
posneg <- markovTransitions$pos_neg_transition
negpos <- markovTransitions$neg_pos_transition
negneg <- markovTransitions$neg_neg_transition

#ver probabilidades de transicion
mean(pospos)
mean(posneg)
mean(negpos)
mean(negneg)


###estadisticos
#one-sample t.test
t.test(pospos, mu=0.5)

#d cohen one-sample
mean_x <- mean(pospos)
sd_x <- sd(pospos)
d <- (mean_x - 0.5) / sd_x

#paired t.test
t.test(pospos, posneg, paired = TRUE)