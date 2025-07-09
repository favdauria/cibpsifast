source("scripts/cargar_libs.R")
source("scripts/cargar_constantes.R")
source("scripts/cargar_datos.R")
source("scripts/functions.R")


###To investigate dynamic shifts in affectivity during the generation of concepts in FAST, 
###we used Markov chain analyses and estimated the probability of transition between positive
###and negative states.
transitions_all <- data.frame(participant = numeric(),
                              pos_pos = numeric(),
                              pos_neg = numeric(),
                              neg_pos = numeric(),
                              neg_neg = numeric())
data <- na.omit(data)
data$valence <- ifelse(data$valence >= 5, "pos", "neg")

participant_transitions_probabilities <- function(participantID){
  #filtrar datos de participantes
  data_participant <-data %>%
    filter(participant == participantID)
  
  #definir matriz de estados
  unique_states <- c("pos", "neg")
  transitions <- matrix(0, nrow = length(unique_states), ncol = length(unique_states), dimnames = list(unique_states, unique_states))
  
  #contar transiciones
  for (i in 1:(length(data_participant$valence)-1)){
    from_state <- data_participant$valence[i]
    to_state <- data_participant$valence[i+1]
    
    chain_from_state <- data_participant$chainNumber[i]
    chain_to_state <- data_participant$chainNumber[i+1]
    if(chain_from_state == chain_to_state){
      transitions[from_state, to_state] <- transitions[from_state, to_state] + 1
    }
  }
  #Normalizar matriz de transiciones
  normalize_transition_matrix <- function(matrix) {
    # Calculate row sums
    row_sums <- rowSums(matrix)
    
    # Normalize each row by dividing by the row sum
    # Avoid division by zero by setting zero row sums to 1 (though should not be needed here)
    row_sums[row_sums == 0] <- 1
    
    # Normalize the matrix
    normalized_matrix <- sweep(matrix, 1, row_sums, FUN = "/")
    return(normalized_matrix)
  }
  transition_matrix_prob <- normalize_transition_matrix(transitions)
}


#recorrer participantes
for (i in 1:nrow(participants_list)){
  participantID <- participants_list$participant[i]
  probabilities_matrix_participant <- participant_transitions_probabilities(participantID)
  
  probabilities_pospos <- probabilities_matrix_participant[1,1]
  probabilities_posneg <- probabilities_matrix_participant[1,2]
  probabilities_negpos <- probabilities_matrix_participant[2,1]
  probabilities_negneg <- probabilities_matrix_participant[2,2]
  
  new_row <- data.frame(participant = participantID,
                        pos_pos = probabilities_pospos,
                        pos_neg = probabilities_posneg,
                        neg_pos = probabilities_negpos,
                        neg_neg = probabilities_negneg
  )
  transitions_all <- rbind(transitions_all, new_row)
}


##datos estadisticos
#matriz para guardar datos
markov_general <- data.frame(transition = character(),
                                 mean = numeric(),
                                 sd = numeric(),
                                 df = numeric(),
                                 t = numeric(),
                                 CI_1 = numeric(),
                                 CI_2 = numeric(),
                                 p_value = numeric(),
                                 d = numeric())

#guardar datos
datos_est_guardar <- function(transition){
  mean_transition <- mean(transitions_all[[transition]])
  sd_transition <- sd(transitions_all[[transition]])
  t_test <- t.test(transitions_all[[transition]], mu=0.5)
  p_value <- t_test$p.value
  t_statistic <- t_test$statistic[[1]]
  conf_int_1 <- t_test$conf.int[1]
  conf_int_2 <- t_test$conf.int[2]
  df <- t_test$parameter[[1]]
  d_value <- (mean_transition - 0.5) / sd_transition
  
  stats <- data.frame(transition = transition,
                      mean = mean_transition,
                      sd = sd_transition,
                      df = df,
                      t = t_statistic,
                      CI_1 = conf_int_1,
                      CI_2 = conf_int_2,
                      p_value = p_value,
                      d = d_value
  )
  return(stats)
}

markov_general <- rbind(markov_general, datos_est_guardar("pos_pos"))
markov_general <- rbind(markov_general, datos_est_guardar("pos_neg"))
markov_general <- rbind(markov_general, datos_est_guardar("neg_pos"))
markov_general <- rbind(markov_general, datos_est_guardar("neg_neg"))
print(markov_general)

###It was observed that SCRS rumination related to transition probabilities between valence states.
###Separate linear regressions showed that participants with higher SCRS rumination scores were more likely
###to transition from a positive conceptual state to a negative conceptual state and were more likely to
###remain in a negative conceptual states. 
scrs <- data %>% 
  group_by(participant, scrs) %>%
  summarise(scrs = mean(scrs, na.rm = TRUE))

markov_scrs <- merge(transitions_all, scrs, by="participant")
#positive to negative vs scrs
model_posneg_scrs <- lm(pos_neg~scrs, data = markov_scrs)
print(summary(model_posneg_scrs))
#negative to negative vs scrs
model_negneg_scrs <- lm(neg_neg~scrs, data = markov_scrs)
print(summary(model_negneg_scrs))

###Similar relationships were found for the SCRS rumination scores, as well as with depression,
###anxiety  and affect scores
markov_and("rrs")
markov_and("beck")
markov_and("stai_state")
markov_and("stai_trait")
markov_and("panas_pos")
markov_and("panas_neg")