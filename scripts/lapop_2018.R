# librerias
library(tidyverse)
library(haven)
library(broom)

# datos ####

# cargar datos

datos <- read_dta("https://obsdemocracia.org/uploads/data_file/Colombia2018PUB.dta") %>%
  as_factor()

# limpieza ####

# crear dummy
datos <- datos %>%
  mutate(vb3n2 = na_if(vb3n2, "Inaplicable"),
         vb3n2_lump = fct_lump(vb3n2, 1)) 

# analisis exploratorio ####

theme_set(theme_light())

# voto en 1a
datos %>%
  count(vb2)

# por quien voto en 2a
datos %>%
  mutate(vb3n = na_if(vb3n, "Inaplicable")) %>%
  ggplot(aes(fct_infreq(vb3n))) +
  geom_bar() +
  coord_flip()

# voto en 2a
datos %>%
  count(vb2v)
datos %>%
  count(vb3n2)

# por quien voto en 2a
datos %>%
  mutate(vb3n2 = na_if(vb3n2, "Inaplicable")) %>%
  ggplot(aes(fct_infreq(vb3n2))) +
  geom_bar() +
  coord_flip()

# modelos ####

modelo <- glm(vb3n2_lump ~ soct2 + idio2, data = datos, family = "binomial")
tidy(modelo)

