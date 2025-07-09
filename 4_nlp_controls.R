library(tidyverse)
library(ggplot2)


rrs_control <- read_csv("files/beck_control.csv", show_col_types = FALSE)
rrs_data <- read_csv("files/data.csv", show_col_types = FALSE) %>% 
  select('participant', 'rrs')


#extraer valores de rrs para los ids de las filas
rrs_control_t <- as.data.frame((t(rrs_control)))
rrs_values <- data.frame(
  rrs = numeric()
)
rho_values <- c()

#testing
rrs_data$participant <- as.character(rrs_data$participant)

rrs_control_t = rrs_control_t %>% rownames_to_column("participant")

r_h0 <- rrs_control_t %>% 
  pivot_longer(-participant, names_to = "replica", values_to = "similitud") %>%
  left_join(rrs_data, by="participant") %>% 
  group_by(replica) %>% 
  summarise(r=cor(similitud, rrs, method = "spearman")) %>% pull(r)


#rho for exp data
similarities_participant <- read_csv("files/simBECK_df.csv", show_col_types = FALSE)
exp_data <- data.frame(
  rrs = numeric(),
  similarity = numeric()
)

for(row in rrs_control_t$participant){
  rrs_participant <- rrs_data %>%
    filter(participant == row)
  similarity_participant <- similarities_participant %>%
    filter(participant == row)
  
  row_sim <- data.frame(
    rrs = rrs_participant$rrs,
    sim = similarity_participant$similitud
  )
  exp_data <- rbind(exp_data, row_sim)
}

rho_exp <- abs(cor(exp_data$rrs, exp_data$sim))
exp_data_reduced = exp_data %>% distinct() 
cor.test(exp_data_reduced$rrs,exp_data_reduced$sim)


hist(r_h0, xlab = "rho", ylab = "Frequency", col = "lightblue", border = "black",xlim=c(0,.9))
abline(v = rho_exp, col = "red", lwd = 2)  # Vertical line
text(10, 1, labels = "Value 10", pos = 4, col = "red")  # Label

# Calculate quartiles
#quartiles <- quantile(rho_values, probs = c(0.25, 0.5, 0.75))

##count
count_lower <- sum(abs(r_h0) < rho_exp)

total_count <- length(r_h0)

percentage_higher <- (count_lower / total_count) * 100

cat(rho_exp, "is higher than", round(percentage_higher, 2), "% of the values in the dataset.\n")



