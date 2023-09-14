library(tidyverse)


# rstudioapi::jobRunScript("retrieve_targeting_data.R")
# rstudioapi::jobRunScript("fbadlibrary.R")

# Sys.sleep(60*7)

dir("_site_hessen", full.names = T) %>% keep(~str_detect(.x, "qmd")) %>% walk(quarto::quarto_render)

dir("_site_bayern", full.names = T) %>% keep(~str_detect(.x, "qmd")) %>% walk(quarto::quarto_render)
