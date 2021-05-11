# Prep ####

# Load libraries
library(tidyverse)
library(here)
library(janitor)

# Load raw survey data
ab <- read_delim(here("data", "Colombia2018PUBv2_IsFad1S.csv"), 
                    delim = ";")

# Data cleaning ####

# recode NA
# .b = "no responde"
# .c = "no sabe"
ab <- ab %>%
   mutate(
      across(everything(), ~na_if(.x, ".b")),
      across(everything(), ~na_if(.x, ".c"))
   )

# Recode variables
ab <- ab %>%
  transmute(
   ## geographic
   # municipality code
   sbjnum,
   municipio_code = as.numeric(str_remove(municipio, "8")),
   # urban
   urban = fct_relevel(
     case_when(
        ur == 1 ~ "Urban",
        ur == 2 ~ "Rural"
     ), "Rural"
   ),
   # region
   region = fct_relevel( 
     case_when(
       estratopri == 811 ~ "Atlantic",
       estratopri == 812 ~ "Bogotá",
       estratopri == 813 ~ "Central",
       estratopri == 814 ~ "East",
       estratopri == 815 ~ "Pacific",
       estratopri == 816 ~ "Amazon/Plains"
     ), "Bogotá"),
   # size of municipality
   municipio_size = fct_relevel( 
     case_when(
       estratosec == 1 ~ "Large",
       estratosec == 2 ~ "Medium",
       estratosec == 3 ~ "Small"
     ), "Small", "Medium"),
   ## socio-demographic
   # gender
   gender = fct_relevel(
      if_else(q1 == 1, "Male", "Female"), "Male"
   ),
   # age in years
   age = q2,
   # education in years
   ed = as.numeric(ed),
   # education in categories
   ed_cat = fct_relevel(
      case_when(
         ed == 0 ~ "None",
         between(ed, 1, 5) ~ "Primary",
         between(ed, 6, 11) ~ "Secondary",
         ed > 11 ~ "Tertiary"
      ), "None", "Primary", "Secondary"
   ),
   # education level
   ed_sup = fct_relevel(
      if_else(ed > 11, "Tertiary", "Secondary or lower"), 
      "Secondary or lower"),
   # working
   employment = fct_relevel(
      case_when(
         colocup4a %in% c(1, 2) ~ "Employed",
         colocup4a %in% c(3:8) ~ "Unemployed"
      ), "Unemployed"
   ),
   # family income
   income_family = as.numeric(q10new),
   # ethnic identity
   etid = fct_relevel(
      case_when(
         etid == 1 ~ "White",
         etid == 2 ~ "Mestizo",
         etid == 3 ~ "Indigenous",
         etid == 4 ~ "Black",
         etid == 5 ~ "Mulato",
         etid %in% c(".a", 7) ~ "Other/don't know",
      ), "Other/don't know"
   ),
   # ethnic identity: minority
   etid_min = fct_relevel(
      case_when(
         etid %in% c(".a", 3, 4, 5, 7) ~ "Minority",
         etid %in% c(1, 2) ~ "White/mestizo",
      ), "White/mestizo"
   ),
   ## victimization
   # reports family member victim of conflict
   victim_family = fct_relevel(
      case_when(
         wc1 == 1 | wc3 == 1 | wc2 == 1 | colwc8 == 1 | colwc9 == 1 ~ "Yes",
         wc1 == 2 & wc3 == 2 & wc2 == 2 & colwc8 == 2 & colwc9 == 2 ~ "No",
         TRUE ~ NA_character_
      ), "No"
   ),
   # registered in government victim registry RUV
   ruv = fct_relevel(
      case_when(
         collt5 == "1" ~ "Yes",
         collt5 == "2" ~ "No"
      ), "No"
   ),
   # victim dummy (either family or registered in RUV)
   victim = fct_relevel(
      case_when(
         victim_family == "Yes" | ruv == "Yes" ~ "Yes",
         victim_family == "No" & ruv == "No" ~ "No"
      ), "No"
   ),
   victim_num = if_else(victim == "Yes", 1, 0),
   # received reparation from government
   reparation = fct_relevel(
      case_when(
         collt6 == 1 ~ "Yes",
         collt6 == 2 ~ "No"
      ), "No"
   ),
   ## democracy and ideology
   democracy_best = as.numeric(ing4),
   democracy_satisfied = fct_relevel(
      case_when(
         pn4 %in% c(1:2) ~ "Yes",
         pn4 %in% c(3:4) ~ "No"
      ), "Yes"
   ),
   # left-right placement, 1-10 scale
   ideology = as.numeric(l1),
   # left-right placement, 2 cat
   ideology_2c = fct_relevel(
      case_when(
         l1 %in% c(1:5) ~ "Left",
         l1 %in% c(6:10) ~ "Right"
      ), "Left"
   ),
   # left-right placement, 3 cat
   ideology_3c = fct_relevel(
      case_when(
         l1 %in% c(1:3) ~ "Left",
         l1 %in% c(4:7) ~ "Center",
         l1 %in% c(8:10) ~ "Right"
      ), "Center", "Left"
   ),
   # left-right placement, 5 cat
   ideology_5c = fct_relevel(
      case_when(
         l1 %in% c(1:2) ~ "Left",
         l1 %in% c(3:4) ~ "Center-left",
         l1 %in% c(5:6) ~ "Center",
         l1 %in% c(7:8) ~ "Center-right",
         l1 %in% c(9:10) ~ "Right"
      ), "Left", "Center-left", "Center", "Center-right"
   ),
   # proximity to Centro Democratico
   prox_cd = as.numeric(colvb27b),
   # proximity to Colombia Humana
   prox_ch = as.numeric(colvb27h),
   ## conflict, peace and agreements
   # negotiate or use force
   negotiation = fct_relevel(
      case_when(
         colpaz1a == 1 ~ "Negotiation",
         colpaz1a %in% c("2" ,"3", ".a") ~ "Other"
      ), "Other"
   ),
   # support santos agreement w farc
   support_agree = as.numeric(colpropaz1b),
   # in favor of duque modifying agreements
   reform_agree = as.numeric(colpact20),
   ## electoral behavior
   # protest
   protest = fct_relevel(
      case_when(
         prot3 == 1 ~ "Yes",
         prot3 == 2 ~ "No",
         prot3 == ".a" ~ NA_character_
      ), "No"
   ),
   # voted in first round
   vote1 = fct_relevel(
      case_when(
         vb2 == 1 ~ "Yes",
         vb2 == 2 ~ "No",
      ), "No"
   ),
   # vote anti-agreement in first round
   vote1_anti = fct_relevel(
     case_when(
        vb3n == 801 ~ "Yes",
        vb3n %in% c(0, 97, 802, 803, 804, 805, 877, 899) ~ "No",
        vb3n == ".a" ~ NA_character_
     ), "No" 
   ),
   vote1_anti_num = if_else(vote1_anti == "No", 0, 1),
   # voted in second round
   vote2 = fct_relevel(
      case_when(
         vb2v == 1 ~ "Yes",
         vb2v == 2 ~ "No"
      ), "No"
   ),
   # vote anti-agreement in first round
   vote2_anti = fct_relevel(
      case_when(
         vb3n == 801 ~ "Yes",
         vb3n %in% c(0, 97, 802, 899) ~ "No",
         vb3n == ".a" ~ NA_character_
      ), "No" 
   ),
   vote2_anti_num = if_else(vote2_anti == "No", 0, 1),
   # vote anti-agreement post-elections (if elections where today)
   votep_anti = fct_relevel(
     case_when(
        vb20 == 2 ~ "Yes",
        vb20 %in% c(1, 3, 4) ~ "No",
        vb20 == ".a" ~ NA_character_
     ), "No"
   ),
   votep_anti_num = if_else(votep_anti == "No", 0, 1)
  )

# Municipal-level data ####

# load violent presence data (vippa)
vipaa <- read_delim(here("data", "ViPPA_v2.csv"), 
                    "\t", escape_double = FALSE, trim_ws = TRUE)

# aggregate to municipality-year
vipaa <- vipaa %>%
   # rename for merging ahead
   rename(codmpio = mun) %>%
   # count number of events per municipality-year-actor type
   count(codmpio, year, actor_main) %>%
   # pivot so that every row is a municipality-year
   pivot_wider(names_from = actor_main, names_prefix = "actions_",
               values_from = n, values_fill = 0) %>%
   # clean up variable names
   clean_names() %>% 
   # create total column
   rowwise(codmpio, year) %>% 
   mutate(
      actions_total = sum(c(actions_insurgents, actions_criminal_organizations,
                            actions_government, actions_paramilitaries,
                            actions_farc_dissidents))
   ) %>% 
   ungroup() %>% 
   # get all existen codmpio-year combinations
   complete(codmpio, year) %>% 
   # replace NA with 0
   mutate(actions_total = replace_na(actions_total, 0)) %>%
   # select relevant variables
   select(codmpio, year, actions_total)

# calculate cumulative variable
vipaa <- vipaa %>%
   # group
   group_by(codmpio) %>% 
   # apply function to select variables and rename columns
   mutate(across(starts_with("actions_"), cumsum, .names = "{.col}_cum")) %>%
   ungroup() %>% 
   # keep only 2018
   filter(year == 2018)

# calculate logs
vipaa <- vipaa %>%
   mutate(
      actions_total_cum_log = log(actions_total_cum + 0.1)
   ) 

# select variables
vipaa <- vipaa %>%
   select(codmpio, actions_total_cum, actions_total_cum_log)

# merge
dataset <- ab %>%
   left_join(vipaa, by = c("municipio_code" = "codmpio")) %>%
   mutate(
      across(starts_with("actions"), ~replace_na(.x, 0))
   )

# remove vipaa dataset
rm(vipaa)

# Save data ####

# unmatched dataset
write_rds(dataset, here("output", "dataset.rds"))
