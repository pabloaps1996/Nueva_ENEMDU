rm(list = ls())

library(tidyverse)
library(rio)
library(magrittr)

# Apertura de las poblaciones nacionales, por area, 5
# ciudades principales y region natural
censo = readRDS("./Censo/poblacion.rds")

censo<-censo %<>%
  mutate(canton = paste0(provincia, canton),
         dom10=case_when(canton=="0101" ~ "30",
                         canton=="0601" ~ "31",
                         canton=="1101" ~ "32",
                         canton=="1701" ~ "33",
                         canton=="1801" ~ "34",
                         canton=="2301" ~ "35",
                         canton=="0701"& area_2000==1 ~ "40",
                         canton=="0801"& area_2000==1 ~ "41",
                         canton=="0901"& area_2000==1 ~ "42",
                         canton=="1308"& area_2000==1 ~ "43",
                         T ~ "NA"),
         provin = provincia)

table(censo$dom10, useNA = "ifany")

# # de personas por dominios

censo<-censo %>% mutate(N_15=ifelse(p03>=15, 1,0))

pob_nac = data.frame(
  dominio = "Nacional",
  poblacion = nrow(censo),
  N_15= sum(censo$N_15)
) %>%
  mutate(cod_dominio = "00") %>%
  select( cod_dominio, poblacion,N_15)

pob_area = censo %>%
  group_by(area_2000) %>%
  summarise(poblacion = n(),
            N_15=sum(N_15)) %>%
  select(cod_dominio = area_2000,poblacion,N_15)


pob_provin = censo %>%
  group_by(provincia) %>%
  summarise(poblacion = n(),
            N_15=sum(N_15)) %>%
  relocate(dominio=provincia) %>% rename(cod_dominio = dominio)

pob_dom10 = censo %>%
  group_by(dom10) %>%
  summarise(poblacion = n(),
            N_15=sum(N_15)) %>%
  relocate(dominio=dom10) %>% rename(cod_dominio = dominio) %>%
  filter(!cod_dominio=="NA")

# Unir las poblaciones

resumen = rbind(pob_nac, pob_area, pob_provin, pob_dom10)

nombre<- import("./insumos/dominios.xlsx") %>% rename(cod_dominio=provin)



resumen<- resumen %>%
  left_join(nombre, by = "cod_dominio") %>%
  mutate(nprovin=case_when(cod_dominio=="00"~"NACIONAL",
                           cod_dominio=="1"~"URBANO",
                           cod_dominio=="2"~"RURAL",
                           T~nprovin ),
         cod_dominio=nprovin) %>% select(-nprovin)

# Guardar los resultados
export(resumen, "./Tratadas/resumen_poblaciones.rds")
