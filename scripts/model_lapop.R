# librerias
library(tidyverse)
library(broom)
library(texreg)
library(sjPlot)
library(stargazer)

# datos
datos_clean <- read_rds("output/datos_lapop_2018_clean.rds")

# turnout

glm(
  vb2 ~ 
    wc_any + collt5 + a4_sec + aoj11 + vic1ext +
    vb10 + l1_num + clien1na + colvb27b + colvb27h +
    colpaz1a + colpropaz_sum + colpact20 + colespa2a +
    q1 + q2 + ed_sup + etid_white +
    colideol_sum + redist_sum + ros_sum +
    pol1_3c + gi0n_3c + conocim_3c, 
  family = "binomial",
  data = datos_clean
) %>% 
  plot_model()

# vote for duque (runoff)

glm(
  vb3n_peace ~
    wc_any + collt5 + a4_sec + aoj11 + vic1ext +
    vb10 + l1_num + clien1na + colvb27b + colvb27h +
    colpaz1a + colpropaz_sum + colpact20 + colespa2a +
    q1 + q2 + ed_sup + etid_white +
    colideol_sum + redist_sum + ros_sum +
    pol1_3c + gi0n_3c + conocim_3c,
  family = "binomial",
  data = datos_clean
) %>%
  plot_model()

glm(
  vb3n2_duque ~
    wc_any + collt5 + a4_sec + aoj11 + vic1ext +
    vb10 + l1_num + clien1na + colvb27b + colvb27h +
    colpaz1a + colpropaz_sum + colpact20 + colespa2a +
    q1 + q2 + ed_sup + etid_white +
    colideol_sum + redist_sum + ros_sum +
    pol1_3c + gi0n_3c + conocim_3c,
  family = "binomial",
  data = datos_clean
) %>%
  plot_model()

glm(
  vb3n2_petro ~
    wc_any + collt5 + a4_sec + aoj11 + vic1ext +
    vb10 + l1_num + clien1na + colvb27b + colvb27h +
    colpaz1a + colpropaz_sum + colpact20 + colespa2a +
    q1 + q2 + ed_sup + etid_white +
    colideol_sum + redist_sum + ros_sum +
    pol1_3c + gi0n_3c + conocim_3c,
  family = "binomial",
  data = datos_clean
) %>%
  plot_model()

