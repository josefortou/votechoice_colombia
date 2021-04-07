# Prep ####

# load libraries
library(tidyverse)
library(lmtest)
library(sandwich)
library(fixest)
library(lme4)

# load data (matched and unmatched)
datos <- read_rds("output/datos.rds")
datos_match <- read_rds("output/datos_match.rds")

# regression models: unmatched data ####

# naive
mod_un <- glm(
  vote1_anti ~ victim*propeace_sum_re,
  family = "binomial",
  data = datos
)

# w covariates
mod_un_cov <- glm(
  vote1_anti ~ victim + urban + gender + age_re + ed_re + region +
    ideology_re + state_sum_re + propeace_sum_re + party_left,
  family = "binomial",
  data = datos
)

# fixed effects
mod_un_fe <- feglm(
  vote1_anti_num ~ victim + urban + gender + age_re + ed_re +
    ideology_re + state_sum_re + propeace_sum_re + party_left | 
    municipio_code,
  family = "binomial",
  data = datos
)

# multilevel
mod_un_re <- glmer(
  vote1_anti ~ victim + urban + gender + age_re + ed_re +
    ideology_re + state_sum_re + propeace_sum_re + party_left + 
    (1|municipio_code),
  family = "binomial",
  data = datos
)

# lsit of models
mods_un <- list(
  `Unmatched, no covariates` = mod_un,
  `Unmatched, w covariates` = mod_un_cov,
  `Unmatched, w FE` = mod_un_fe,
  `Unmatched, w RE` = mod_un_re
)

# save
write_rds(mods_un, "output/mods_un.rds")

# regression models: matched data ####

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
