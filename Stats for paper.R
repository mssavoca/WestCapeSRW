# Stats for SRW paper----

source("Util_SRW.R")

# Load necessary packages
library(MCMCglmm)
library(lme4)
library(lmerTest)


#Historic prey data plot----

# Calculate median by taxa for station type
median_by_taxa <- Historic_prey_dens %>%
  filter(station_type == "station") %>%
  group_by(taxa) %>%
  summarise(median_dens_by_m3 = median(dens_by_m3, na.rm = TRUE)) %>%
  arrange(desc(median_dens_by_m3))



#Modern prey data plot----

# Calculate median by taxa for station type
median_by_taxa_mod <- Modern_prey_dens %>%
  filter(station_type == "station") %>%
  group_by(taxa) %>%
  summarise(median_dens_by_m3 = median(dens_by_m3, na.rm = TRUE)) %>%
  arrange(desc(median_dens_by_m3))




# Convert "month" column to factor with desired order
All_prey_density <- All_prey_density %>%
  mutate(month = factor(month, levels = c("October", "November", "December", "January")))


# Make sure your factors are properly set
All_prey_density$station_type <- as.factor(All_prey_density$station_type)
All_prey_density$taxa <- as.factor(All_prey_density$taxa)
All_prey_density$month <- as.factor(All_prey_density$month)
All_prey_density$year <- as.factor(All_prey_density$year)



All_prey_density_filtered <- All_prey_density %>%
  filter(month != "September")

All_prey_density_filtered$taxa <- relevel(All_prey_density_filtered$taxa, 
                                          ref = "Candaciidae")


# Define priors for the random effect
prior <- list(
  R = list(V = 1, nu = 0.002),  # residual variance
  G = list(G1 = list(V = 1, nu = 0.002))  # random effect variance
)

# Fit the MCMCglmm model
mcmc_model <- MCMCglmm(
  fixed = dens_by_m3 ~ station_type + taxa,  # interaction term
  random = ~ year,
  data = All_prey_density_filtered,
  family = "gaussian",
  prior = prior,
  nitt = 110000, burnin = 10000, thin = 100
)

# Check model summary
summary(mcmc_model)






