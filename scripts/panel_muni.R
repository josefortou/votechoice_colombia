library(tidyverse)
library(readxl)

# Cargar datos####

# Panel CEDE

cede <- read_rds("data/panel_cede_completo.rds")

# Electorales

elect <- read_excel("data/electorales_2014.xlsx")

# Unir dataframes

data <- cede %>%
  left_join(elect, by = "codmpio")

# Crear nuevas variables

data <- data %>%
  group_by(codmpio) %>%
  mutate(
    sgr_acum = sum(sgr_total, na.rm = TRUE),
    desplazados_recepcion_total = sum(desplazados_recepcion, na.rm = TRUE),
    desplazados_expulsion_total = sum(desplazados_expulsion, na.rm = TRUE),
    incup_farc_total = sum(incup_farc, na.rm = TRUE),
    incup_eln_total = sum(incup_eln, na.rm = TRUE),
    incup_auc_total = sum(incup_auc, na.rm = TRUE)
  )

# Seleccionar 2018

data_2018 <- data %>%
  filter(ano == 2018)

# Guardar

write_rds(data_2018, "output/datos_muni_2018.rds")
