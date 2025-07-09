source("scripts/cargar_libs.R")
source("scripts/cargar_constantes.R")
source("scripts/cargar_datos.R")
source("scripts/functions.R")

###higher SCRS rumination scores predicted more negatively valenced conceptual associations during FAST
valence_scrs <- data %>% 
  group_by(participant, scrs) %>%
  summarise(valence_mean = mean(valence))
model_valence_scrs <- lm(valence_mean~scrs, data = valence_scrs)
summary(model_valence_scrs)
confint(model_valence_scrs)


#añadir R² semiparcial (partR²) al reporte

###RRS scores and valence interacted so that higher RRS rumination was associated with higher self-reference for negative self-generated concepts but not positive
selfref_scrs <- data %>% 
  group_by(type_answer, scrs, participant) %>% 
  summarise(meanSR = mean(selfReference))
model_selfref_scrs <- lmer(meanSR~scrs:type_answer+type_answer+(1|participant), selfref_scrs)
summary(model_selfref_scrs)
#reporatar b de modelo asterisco estimate, scrs:typeanswer
#recordar cambiar en reporte R2 a R2part
#confint
confint(model_selfref_scrs)

model_selfref_scrs2 <- lmer(meanSR~scrs*type_answer+(1|participant), selfref_scrs)
summary(model_selfref_scrs2)
confint(model_selfref_scrs2)

#
#confint partr2

#1
resultado <- partR2(model_selfref_scrs,
                    partvars = c("type_answer", "scrs:type_answer"),
                    data = selfref_scrs,
                    nboot = 1000)  
print(resultado)
#2
resultado2 <- partR2(model_selfref_scrs2,
                    partvars = c("scrs", "type_answer", "scrs:type_answer"),
                    data = selfref_scrs,
                    nboot = 1000)  
print(resultado2)

###Similar findings were observed when using the SCRS rumination scores, as well as with the BDI-II depression, STAI anxiety and PANAS affect scores
#valence
valence_and("rrs")
valence_and("beck")
valence_and("stai_state")
valence_and("stai_trait")
valence_and("panas_pos")
valence_and("panas_neg")
#self reference
selfref_and("scrs")
selfref_and("rrs")
selfref_and("beck")
selfref_and("stai_state")
selfref_and("stai_trait")
selfref_and("panas_pos")
selfref_and("panas_neg")
