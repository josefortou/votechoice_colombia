# Prep ####

# load libraries
library(tidyverse)
library(lmtest)
library(sandwich)
library(fixest)
library(lme4)

# load data (matched and unmatched)
datos <- read_rds("output/datos.rds")
match_data <- read_rds("output/match_data.rds")

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
  family = "quasibinomial",
  weights = weights,
  data = match_data
)

mod_match_cov <- glm(
  vote1_anti ~ victim_num + urban + gender + age + ed + income_family +
    ideology + democracy_best + support_agree,
  family = "quasibinomial",
  weights = weights,
  data = match_data
)

# make list
mods <- list(
  # `Unmatched, no covariates` = mod_un,
  # `Unmatched, w covariates` = mod_un_cov,
  `Matched, no covariates` = mod_match,
  `Matched, w covariates` = mod_match_cov
)

# save ####

write_rds(mods, "output/mods.rds")
