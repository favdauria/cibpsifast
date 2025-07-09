source("scripts/cargar_libs.R")
source("scripts/cargar_constantes.R")
source("scripts/cargar_datos.R")
source("scripts/functions.R")

###We examined the semantic similarity between participants concept production during 
###FAST and the set of words derived from the psychopathology core beliefs.
###It was found that higher BDI-II depression scores... 
beckwords_similarity_data <- read_csv("files/simBECK_df.csv", show_col_types = FALSE) %>%
  drop_na()

data <- data %>% 
  select('participant', 'beck', 'rrs','scrs', 'stai_state', 'stai_trait', 'panas_pos', 'panas_neg') %>%
  distinct()

similarity_data <- merge(beckwords_similarity_data, data, by ="participant")

#cors
cor.test(similarity_data$similitud,similarity_data$scrs)
cor.test(similarity_data$similitud,similarity_data$beck)
cor.test(similarity_data$similitud,similarity_data$rrs)
cor.test(similarity_data$similitud,similarity_data$stai_state)
cor.test(similarity_data$similitud,similarity_data$stai_trait)
cor.test(similarity_data$similitud,similarity_data$panas_pos)
abs(cor(similarity_data$similitud, similarity_data$panas_pos))
cor.test(similarity_data$similitud,similarity_data$panas_neg)





#models
summary(lm(similitud~scrs, data = similarity_data))
summary(lm(similitud~beck, data = similarity_data))
summary(lm(similitud~rrs, data = similarity_data))
summary(lm(similitud~stai_state, data = similarity_data))
summary(lm(similitud~stai_trait, data = similarity_data))
summary(lm(similitud~panas_pos, data = similarity_data))
summary(lm(similitud~panas_neg, data = similarity_data))

#####
ggplot(similarity_data, aes(x = panas_pos, y = similitud)) +
  geom_point(alpha = 0.5, color = "steelblue", size = 2) +
  geom_smooth(method = "lm", color = "darkred", se = TRUE) +
  labs(
    x = "panas pos",
    y = "Similarity"
  ) +
  theme_classic(base_size = 16)

