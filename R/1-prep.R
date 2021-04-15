# Prep ####

# Load libraries
library(tidyverse)
library(here)
library(dagitty)
library(ggdag)
library(MatchIt)
library(WeightIt)
library(cobalt)

# Load raw survey data
ab <- read_delim(here("data", "Colombia2018PUBv2_IsFad1S.csv"), 
                    delim = ";")

# Data cleaning ####

# recode NA
# .b = "no responde"
# .c = "no sabe"
ab_clean <- ab %>%
   mutate(
      across(everything(), ~na_if(.x, ".b")),
      across(everything(), ~na_if(.x, ".c"))
   )

# Recode variables
ab_clean <- ab_clean %>%
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
   upm,
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
   # economic informality
   formal = fct_relevel(
      case_when(
         formal == 1 ~ "Formal",
         formal == 2 ~ "Informal"
      ), "Informal"
   ),
   # socio-economic group (estrato)
   estrato = as.factor(colestsoc_col),
   # socio-economic group 3 categories
   estrato_3c = fct_relevel(
      case_when(
         colestsoc_col %in% c(1:3) ~ "Lower",
         colestsoc_col %in% c(4:6) ~ "Upper",
         colestsoc_col == 7 ~ "Rural",
      ), "Rural", "Lower", "Upper"
   ),
   # socio-economic group 3 categories v2
   estrato_3c_v2 = fct_relevel(
      case_when(
         colestsoc_col %in% c(1:2) ~ "Lower",
         colestsoc_col %in% c(3:6) ~ "Middle/upper",
         colestsoc_col == 7 ~ "Rural",
      ), "Rural", "Lower", "Middle/upper"
   ),
   # socio-economic group 4 categories
   estrato_4c = fct_relevel(
      case_when(
         colestsoc_col %in% c(1:2) ~ "Lower",
         colestsoc_col %in% c(3:4) ~ "Middle",
         colestsoc_col %in% c(5:6) ~ "Upper",
         colestsoc_col == 7 ~ "Rural",
         ), "Rural", "Lower", "Middle"
   ),
   # family income
   income_family = as.numeric(q10new),
   # family income above minimum wage
   income_family_min = fct_relevel(
     if_else(income_family > 8, "Above", "Below"), "Below" 
   ),
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
   # feels secure/insecure in neighborhood
   secure = fct_relevel(
      case_when(
         aoj11 %in% c(1, 2) ~ "Secure",
         aoj11 %in% c(3, 4) ~ "Insecure",
         TRUE ~ NA_character_
      ), "Insecure"
   ),
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
   ## evaluations and perceptions
   # most important issue is security-related
   security = fct_relevel(
      case_when(
         a4 %in% c(30, 05, 56, 22, 17, 862, 863, 864, 12, 14, 06, 27, 31, 
                   33, 57) ~ "Security", 
         a4 == ".b" ~ NA_character_,
         TRUE ~ "Other"
      ), "Other issue" 
   ),
   # sociotropic evaluation of the economy, last year
   socio = fct_relevel(
     case_when(
       soct2 == 1 ~ "Better",
       soct2 == 2 ~ "Same",
       soct2 == 3 ~ "Worse",
       soct2 == ".a" ~ NA_character_
     ), "Worse", "Same"
   ),
   # idiosyncratic evaluation of the economy, last year
   idio = fct_relevel(
      case_when(
         idio2 == 1 ~ "Better",
         idio2 == 2 ~ "Same",
         idio2 == 3 ~ "Worse",
         idio2 == ".a" ~ NA_character_
      ), "Worse", "Same"
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
         l1 %in% c(1:4) ~ "Left",
         l1 %in% c(5:6) ~ "Center",
         l1 %in% c(7:10) ~ "Right"
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
   # role of state in intervention sum index
   ros_sum = na_if(
      replace_na(as.numeric(ros1), 0) + 
         replace_na(as.numeric(ros4), 0), 0
   ),
   # state redistribution sum index
   redist_sum = na_if(
      replace_na(as.numeric(redist1), 0) + 
         replace_na(as.numeric(redist2), 0) +
         replace_na(as.numeric(redist3), 0), 0
   ),
   # state intervention and redistribution sum index
   state_sum = na_if(replace_na(ros_sum, 0) + replace_na(redist_sum, 0), 0),
   ## conflict, peace and agreements
   # negotiate or use force
   negotiation = fct_relevel(
      case_when(
         colpaz1a == 1 ~ "Negotiation",
         colpaz1a %in% c("2" ,"3", ".a") ~ "Other"
      ), "Other"
   ),
   # reconciliation with farc or eln possible
   reconciliation = fct_relevel(
      case_when(
         colpaz6a == 1 | colpaz6c == 1 ~ "Yes",
         colpaz6a %in% c("2", ".a") | colpaz6c %in% c("2", ".a") ~ "No/Don't know"
      ), "No/Don't know"
   ),
   # pro-agreement attitudes
   support_agree = as.numeric(colpropaz1b),
   # in favor of modifying agreements
   reform_agree = as.numeric(colpact20),
   # approve of FARC participating in elections
   farc_part = as.numeric(colespa2a),
   ## party id
   # sympathizes with a (any) party
   party_any = fct_relevel(
      case_when(
         vb10 == 1 ~ "Yes", 
         vb10 == 2 ~ "No"
      ), "No"
   ),
   # proximity to individual parties
   prox_cd = as.numeric(colvb27b),
   prox_pu = as.numeric(colvb27c),
   prox_av = as.numeric(colvb27g),
   prox_ch = as.numeric(colvb27h),
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
   votep_anti_num = if_else(votep_anti == "No", 0, 1),
   ## interest, political knowledge and media
   # interest in politics, 3 cat
   interest_cat = fct_relevel(
      case_when(
         pol1 %in% c(1, 2) ~ "High/some",
         pol1 == 3 ~ "Little",
         pol1 == 4 ~ "None",
         pol1 == ".a" ~ NA_character_
      ), "None", "Little"
   ),
   # political knowledge, 3 cat
   know_cat = fct_relevel(
      case_when(
         conocim %in% c(1, 2) ~ "High",
         conocim == 3 ~ "Medium",
         conocim %in% c(4, 5) ~ "Low"
      ), "Low", "Medium"
   )
  )

# Municipal-level data ####

# load violent presence data (vippa)
vippa <- read_delim(here("data", "ViPPA_v2.csv"), 
                    "\t", escape_double = FALSE, trim_ws = TRUE)

# aggregate to municipality-year
vippa <- vippa %>%
   # rename for merging ahead
   rename(codmpio = mun) %>%
   # count number of events per municipality-year-actor type
   count(codmpio, year, actor_main) %>%
   # pivot so that every row is a municipality-year
   pivot_wider(names_from = actor_main, names_prefix = "presence_",
               values_from = n, values_fill = 0) %>%
   # clean up variable names
   clean_names() %>%
   # complete cases 
   complete(codmpio, year) %>%
   # replace NA with 0 and change format to numeric
   mutate(
      across(presence_insurgents:presence_farc_dissidents, ~replace_na(.x, 0)),
      across(presence_insurgents:presence_farc_dissidents, as.numeric)
   )

# define function to calculate cumulative sums with NA values
cumsum_narm <- function(x) {
   cumsum(replace_na(x, 0))
}

# calculate cumulative variables
vippa <- vippa %>%
   # apply function to select variables and rename columns
   mutate(across(starts_with("presence_"), cumsum_narm, .names = "{.col}_cum")) %>%
   # keep only 2018
   filter(year == 2018)

# calculate logs
vippa <- vippa %>%
   mutate(
      across(
         presence_insurgents:presence_farc_dissidents_cum, 
         # add 1 to calculate log on 0s
         ~log(.x + 1), 
         .names = "{.col}_log"
      )
   )

# select variables
vippa <- vippa %>%
   select(codmpio, ends_with("cum_log"))

# merge
datos <- ab_clean %>%
   left_join(vippa, by = c("municipio_code" = "codmpio"))

# remove vippa dataset
rm(vippa)

# load CEDE municipality-year panel 
cede <- read_rds(here("data", "panel_cede_completo.rds"))

# select variables
cede <- cede %>%
   select(ano, municipio_code = codmpio, 
          desplazados_recepcion, desplazados_expulsion)

# calculate cumulative variables
cede <- cede %>%
   # apply function to select variables and rename columns
   mutate(across(starts_with("desplazados_"), cumsum_narm, .names = "{.col}_cum")) %>%
   # keep only 2018
   filter(ano == 2018)

# calculate logs
cede <- cede %>%
   mutate(
      across(
         starts_with("desplazados_"), 
         # add 1 to calculate log on 0s
         ~log(.x + 1), 
         .names = "{.col}_log"
      )
   )

# select variables
cede <- cede %>%
   select(municipio_code, desplazados_recepcion_cum_log, desplazados_expulsion_cum_log)

# merge
datos <- datos %>%
   left_join(cede, by = "municipio_code")

# remove CEDE dataset
rm(cede)

# some data recoding for different kinds of analysis
ab <- ab %>%
   mutate(
      across(
         c(age, ed, ideology, state_sum, propeace_sum, ends_with("cum_log")), 
         scale, 
         .names = "{.col}_re"
      )
   )

# DAG ####

node_details <- tribble(
   ~name, ~label, ~x, ~y,
   "vic", "Victimization", 1, 1,
   "vote_choice", "Vote choice", 3, 1,
   "ses", "SES", 1.5, 2,
   "pol_att", "Political \nattitudes", 2, 2,
   # "urban", "Urban ", 1.5, 3,
   # "gender", "Gender", 2, 3,
   # "age", "Age", 2.5, 3,
   # "edu", "Education", 3, 3,
   # "ethid", "Ethinicity", 3.5, 3,
   # "relig", "Religious", 4, 3,
   # "ideol", "Ideology", 4.5, 3,
   # "party_id", "Party ID", 5, 3,
   # "pro_peace", "Pro-peace attitudes", 5.5, 3,
   # "pro_state", "Pro-intervention attitudes", 6, 3,
   "muni", "Municipality \ncharacteristics", 2.5, 2
)

node_labels <- node_details$label
names(node_labels) <- node_details$name

vote_dag <- dagify(
   # vote_choice ~ vic + muni + urban + age + edu + age + edu + ethid + relig +
   #    ideol + party_id + pro_peace + pro_state,
   # vic ~ muni + urban + age + edu + age + edu + ethid + relig +
   #    ideol + party_id + pro_peace + pro_state,
   vote_choice ~ vic + muni + ses + pol_att,
   vic ~ muni + ses + pol_att,
   exposure = "vic",
   outcome = "vote_choice",
   # latent = "Local-lev",
   coords = node_details,
   labels = node_labels
)

vote_dag

# Turn DAG into a tidy data frame for plotting
vote_dag_tidy <- vote_dag %>% 
   tidy_dagitty() %>%
   node_status()   # Add column for exposure/outcome/latent

status_colors <- c(exposure = "#0074D9", outcome = "#FF4136", latent = "grey50")

# Fancier graph
ggplot(vote_dag_tidy, aes(x = x, y = y, xend = xend, yend = yend)) +
   geom_dag_edges() +
   geom_dag_point(aes(color = status)) +
   geom_dag_label_repel(aes(label = label, fill = status), seed = 1234,
                        color = "white", fontface = "bold") +
   scale_color_manual(values = status_colors, na.value = "grey20") +
   scale_fill_manual(values = status_colors, na.value = "grey20") +
   guides(color = FALSE, fill = FALSE) + 
   theme_dag()

# find backdoor paths
paths(vote_dag)

# find necessary adjustments
adjustmentSets(vote_dag)

# save
vote_dag <- write_rds("output/vote_dag.rds")

# Matching ####

# # data
# ab_clean <- read_rds(here("output", "ab_clean.rds"))
# dataset <- read_rds(here("output", "datos.rds"))

# select covariates that are:
# measured prior to treatment (or otherwise not be affected by treatment)
# confounding vars: cause variation in the outcome and selection into treatment
datos_m <- ab_clean %>%
   select(vote1_anti, victim_num, urban, gender, age, ed, income_family,
          ideology, democracy_best, support_agree)

# drop NA
datos_m <- datos_m %>%
   drop_na()

# make list of matching methods
match_methods <- list(
   `Nearest Neighbor` = "nearest",
   `Optimal` = "optimal",
   `Full` = "full",
   # `Exact` = "exact",
   `Coarsened Exact` = "cem"
)

# apply matching methods
match_out <- match_methods %>%
   map(~ matchit(
      victim_num ~ urban + gender + age + ed + income_family +
         ideology + democracy_best + support_agree,
      data = datos_m, method = .
   ))

# genetic matching
match_gen <- matchit(
   victim_num ~ urban + gender + age + ed + income_family +
      ideology + democracy_best + support_agree,
   data = datos_m, method = "genetic", pop.size = 1000
)

match_out[["Genetic"]] <- match_gen

# check imbalance before and after

match_out %>%
   map(~ bal.tab(., thresholds = c(m = 0.05), un = TRUE))

# love plot to check imbalance graphically
match_out %>%
   # pick "full optimal" matching
   pluck(3) %>%
   love.plot(
   stats = "mean.diffs",
   thresholds = c(m = 0.05, v = 2), 
   abs = TRUE, binary = "std", var.order = "unadjusted"
)

# extract matched data
match_data <- match_out %>%
   pluck(3) %>%
   match.data()

# Save data ####

# unmatched
write_rds(datos, "output/datos.rds")

# matched
write_rds(match_data, "output/match_data.rds")
