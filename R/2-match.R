# Prep ####

# Load libraries
library(tidyverse)
library(here)
library(dagitty)
library(ggdag)
library(MatchIt)
library(WeightIt)
library(cobalt)

# load clean survey data with municipality-level variables
dataset <- read_rds(here("output", "dataset.rds"))

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
vote_dag <- write_rds(vote_dag, here("output", "vote_dag.rds"))

# Matching ####

# # data
# ab_clean <- read_rds(here("output", "ab_clean.rds"))
# dataset <- read_rds(here("output", "datos.rds"))

# select covariates that are:
# measured prior to treatment (or otherwise not be affected by treatment)
# confounding vars: cause variation in the outcome and selection into treatment
data_m <- dataset %>%
  select(
    # dv and treatment
    vote1_anti, victim_num,
    # socio-demographic
    urban, gender, age, ed, income_family, etid,
    # ideology
    ideology, democracy_best, democracy_satisfied, protest, prox_cd,
    # pro-peace/agreement attitudes
    negotiation, support_agree, reform_agree
  ) %>%
  # drop NA
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
    data = data_m, method = .
  ))

# genetic matching
match_gen <- matchit(
  victim_num ~ urban + gender + age + ed + income_family +
    ideology + democracy_best + support_agree,
  data = data_m, method = "genetic", pop.size = 1000
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

# matched dataset
write_rds(match_data, here("output", "match_data.rds"))
