# librerias
library(tidyverse)
library(lmtest)
library(sandwich)

# datos
datos <- read_rds("output/datos.rds")
datos_match <- read_rds("output/datos_match.rds")

# regression models ####

# run regression on unmatched data
mod_un <- glm(
  vote1_anti ~ factor(victim_num),
  family = "binomial",
  data = datos
)

mod_un_cov <- glm(
  vote1_anti ~ factor(victim_num) + region + urban + municipio_size + gender + age + ed +
    ideology + state_sum + propeace_sum + party_left +
    presence_insurgents_cum_log + presence_paramilitaries_cum_log +
    desplazados_expulsion_cum_log,
  family = "binomial",
  data = datos_match
)

# run regression with matched data
mod_match <- glm(
  vote1_anti ~ factor(victim_num),
  family = "binomial",
  weights = weights,
  data = datos_match
)

mod_match_cov <- glm(
  vote1_anti ~ factor(victim_num) + region + urban + municipio_size + gender + age + ed +
    ideology + state_sum + propeace_sum + party_left +
    presence_insurgents_cum_log + presence_paramilitaries_cum_log +
    desplazados_expulsion_cum_log,
  family = "binomial",
  weights = weights,
  data = datos_match
)

# make list
mods <- list(
  `Unmatched, no covariates` = mod_un,
  `Unmatched, w covariates` = mod_un_cov,
  `Matched, no covariates` = mod_match,
  `Matched, w covariates` = mod_match_cov
)

# save ####

write_rds(mods, "output/mods.rds")
