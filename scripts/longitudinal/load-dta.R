# libraries
library(tidyverse)

# list files
dta_files <- list.files("data/lapop/downloaded_files", "*.dta", 
                        full.names = TRUE)

# dta_files %>%
#   map(read_dta, encoding = "latin1") %>%
#   map(janitor::clean_names) %>%
#   map_dfr(bind_rows)

# pluck one dataset
# read it with correct encoding
# convert labelled data into factors
lap18 <- dta_files %>%
  purrr::pluck(21) %>%
  read_dta(encoding = "utf-8") %>%
  mutate_if(is.labelled, as_factor) 

lap18 <- lap18 %>%
  mutate(dummy_voto = fct_lump(vb2, 1))

model <- glm(dummy_voto ~ q1, data = lap18, family = "binomial")
tidy(model)

library(nnet)
model <- multinom(vb2 ~ q1, data = lap18)
tidy(model)
