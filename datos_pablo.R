pacman::p_load(tidyverse, googlesheets4)
pablo <- gs4_get(gs4_find("rindes_trigo_2020_JMF")$id)
load(here::here("data/data.Rdata"))

dat %>% 
  filter(cultivo_de_cosecha == "Trigo") %>% 
  select(campana, Zona, variedad, fecha_siembra = fecha_de_siembra_dia_mes, rinde) %>% 
  write_sheet(ss=pablo, sheet = "rindes")
