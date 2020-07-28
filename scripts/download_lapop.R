# librerias
library(tidyverse)
library(xml)

# descargas ####

# datos
url_datos <- "https://obsdemocracia.org/uploads/data_file/Colombia2018PUBv2_IsFad1S.csv"
download.file(url_datos, "data/datos_lapop_2018.csv")

# cuestionario
url_cuest <- "https://obsdemocracia.org/uploads/data_file/Bar%C3%B3metro_de_las_Am%C3%A9ricas_2018PUB_5UCtdqH_Hj9rXwk.pdf"
download.file(url_cuest, "docs/cuestionario_lapop_2018.pdf")
