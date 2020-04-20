library(tidyverse)
library(haven)

# load data
lapop0418 <- read_dta("data/lapop/Colombia LAPOP merge 2004-2018 v1.0_W.dta") %>%
  mutate_if(is.labelled, as_factor)

# colnames(lapop0418)
# 
# lapop0418 %>%
#   pivot_longer(
#     cols = starts_with("vb3_"),
#     names_to = "ano_eleccion",
#     names_prefix = "vb3_",
#     values_to = "vb3"
#   ) %>%
#   count(vb3, ano_eleccion) %>% View()

# select some vars
lapop_sub <- lapop0418 %>%
  filter(year == "2018") %>%
  select(wave, year, estratopri, estratosec, prov, municipio, 
         q1, q2, a4, vb2, vb2v, vb3n_18, vb3n2_18)
rm(lapop0418)

# plot
lapop_sub %>%
  ggplot(aes(y = fct_infreq(vb3n2_18))) +
  geom_bar()
