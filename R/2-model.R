# librerias
library(tidyverse)
library(lmtest)
library(sandwich)
library(ggeffects)
library(modelsummary)

# datos
datos_clean <- read_rds("output/datos.rds")

# regression models ####

# run regression with matched data
mod <- glm(
  vote1_anti ~ factor(victim_num) + region + municipio_size + gender + age + ed_cat +
    ideology + state_sum + propeace_sum + party_left +
    presence_insurgents_cum_log + presence_paramilitaries_cum_log +
    desplazados_recepcion_cum_log + desplazados_expulsion_cum_log,
  family = "binomial",
  weights = weights,
  data = m.data
)

summary(mod)

mod_s <- coeftest(mod, vcov. = vcovCL(mod, cluster = ~region))

mod %>%
  ggpredict(terms = "victim_num",
            vcov.fun = "vcovHC") %>%
  plot()

