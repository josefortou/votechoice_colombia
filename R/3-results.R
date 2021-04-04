# librerias
library(tidyverse)
library(lmtest)
library(sandwich)
library(ggeffects)
library(modelsummary)

datos %>% 
  select(age, ed, ideology, state_sum, propeace_sum, prox_cd, presence_insurgents_cum_log, presence_paramilitaries_cum_log, desplazados_recepcion_cum_log, desplazados_expulsion_cum_log, vote1_anti) %>% 
  datasummary_balance(~ vote1_anti, data = .)

coefs <- c("factor(victim_num)" = "Victim")

vcovmat <- vcovCL(mod, cluster = ~ region + municipio_size + urban)

msummary(
  mods,
  stars = TRUE,
  vcov = "HC"
)

mods %>%
  pluck(4) %>%
  ggpredict(terms = "victim_num",
            vcov.fun = "HC") %>%
  plot()
