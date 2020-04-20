# load libraries
library(tidyverse)
library(rvest)

# make list of urls to download from
pages <- list(
  "https://obsdemocracia.org/temas-de-estudio/datos/?page=1",
  "https://obsdemocracia.org/temas-de-estudio/datos/?page=2",
  "https://obsdemocracia.org/temas-de-estudio/datos/?page=3"
)

# make tibble with urls/links to csv, dta and pdf files
links <- pages %>%
  map(read_html) %>% 
  map(html_nodes, "a.link") %>%
  map(html_attr, "href") %>%
  map_dfr(as_tibble) %>%
  filter(str_detect(value, ".dta|.csv|.pdf")) %>%
  mutate(url = paste0("https://obsdemocracia.org", value),
         file_type = str_sub(url, -3))

# create directory to store downloaded files
dir.create("data/lapop/downloaded_files")

# make full path for files
file_path <- paste0("data/lapop/downloaded_files/", basename(links$url))

# download all files to folder
walk2(.x = links$url, .y = file_path, .f = download.file)

# clean enviroment and console
rm(list = ls())
cat("\14")
         