# Importacion {#import}


```r
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)
pacman::p_load(tidyverse, googlesheets4)
# gs4_auth(email = "edwardsmolina@gmail.com")
# gs4_find("JMF_2020")
# jmf <- gs4_get("1b3Zu9CT-B1lHVwkl7wLS_njDQyvwRFYnw_H-OjyO7LA")
# gs4_browse(jmf)
load(here::here("data/data.Rdata"))
```



```r
raw <- read_sheet(jmf, sheet = "raw", guess_max = 10000, skip=1) %>% 
  janitor::clean_names() #%>% 
  # mutate_if(is.character, as.factor) %>% 
  # mutate(fsiembra = lubridate::ymd(fsiembra)) 
# glimpse(raw)
# summary(raw$superficie)
```


```r
wiki <- read_sheet(jmf, sheet = "wiki", guess_max = 10000, skip=0) 
# wiki %>%  view
```



```r
raw %>% 
  mutate(Zona = case_when(
    str_detect(zona_0, "Madariaga|Maipú") ~ "Madariaga", 
    str_detect(zona_0, "Balcarce") ~ "Sierra", 
    TRUE ~ "Costa"
  )) %>%  
   mutate(
    year = factor(lubridate::year(fecha_de_siembra_dia_mes)),     
    date = update(fecha_de_siembra_dia_mes, year = 1), 
    temprano_tardio= case_when(
    date<as.Date("0001-07-15") ~ "Temprano", 
    date>as.Date("0001-07-15") ~ "Tardio")) %>% 
  mutate(across(c("localidad", "variedad", "antecesor_estival","fungicida1", "fungicida2", 
                  "enfermedad_1", "enfermedad_2"), 
                #sacar acentos
                ~stringi::stri_trans_general(
                #pasar a mayusculas y sacar puntos
                str_to_upper(gsub(',', '\\.', 
                # sacar espacios antes-desp
                str_trim(
                  str_replace_all(., fixed(" "), "_")
                ))), "Latin-ASCII"))) %>% 
  rowwise() %>%
  mutate(dano_tot = sum(dano_1,dano_2, na.rm = T)) %>% 
  ungroup-> dat  

# glimpse(dat)
# raw %>% 
#   slice(292:295) %>% 
#   select(contains("dano"))
# save(raw,dat, file = "data/data.Rdata")
```


```r
all_images <- list.files(path = here::here("plots"), 
                         pattern = ".png", 
                         all.files = TRUE, full.names = TRUE)
all_images %>% 
  map(~ drive_upload(.,
                     path = as_dribble("juanchi_guille/JMF_fina_2020"),
                     overwrite = TRUE)
  )
```


* Cantidad de datos/celdas vacias por variable


```r
dat %>%
  summarise_all(funs(sum(!is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "datos") %>% 
  left_join(by="variable", 
            dat %>%
    summarise_all(funs(sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "datos_faltantes") 
  ) #%>% 
  # knitr::kable() %>% 
  # write_sheet(ss=jmf, sheet = "faltantes")
```
