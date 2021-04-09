# Prep ####

# load libraries 
library(tidyverse)
library(lmtest)
library(sandwich)
library(ggeffects)
library(modelsummary)

# load models
mods_un <- read_rds("output/mods_un.rds")
datos <- read_rds("output/datos.rds")

# tables ####

# balance
datos %>% 
  select(age, ed, ideology, state_sum, propeace_sum, prox_cd, presence_insurgents_cum_log, presence_paramilitaries_cum_log, desplazados_recepcion_cum_log, desplazados_expulsion_cum_log, vote1_anti) %>% 
  datasummary_balance(~ vote1_anti, data = .)

# regression results
msummary(
  mods_un,
  stars = TRUE
)

# predicted probabilities ####

mods_un %>%
  pluck(3) %>%
  ggpredict(terms = "victim") %>%
  plot()
