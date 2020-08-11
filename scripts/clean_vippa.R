library(tidyverse)

vippa <- read_delim("data/ViPPA_v2.csv", "\t", 
                    escape_double = FALSE, trim_ws = TRUE)

vippa %>%
  count(mun, year) %>%
  group_by(mun, year) %>%
  mutate(cum_actions = cumsum(n)) %>%
  filter(mun == 5001) %>%
  ggplot(aes(year, cum_actions)) +
  geom_col() +
  facet_wrap(~ actor_sub)

vippa %>%
  group_by(mun, actor_sub, year) %>%
  summarise(casos = n()) %>%
  ungroup() %>%
  complete(
    year = seq(
      min(year), max(year), by = 1
    )
  ) %>%
  group_by(mun, actor_sub) %>%
  mutate(
    casos = replace_na(casos, 0),
    casos_cum = cumsum(casos)
  ) %>% 
  filter(mun==5001) %>%
  ggplot(aes(year, casos_cum)) +
  geom_col() +
  facet_wrap(~ actor_sub)
