
#

rm(list = ls())
#Librarias
{library(readxl)
  library(haven)
  library(stringr)
  library(tidyverse)
  library(srvyr)
  library(samplesize4surveys)
  library(openxlsx)
  library(rio)}


marco = readRDS("./insumos/2. tamaño_de_muestra/20240502_marco_upm.rds")

upm_nac = marco %>%
  group_by(id_upm) %>%
  summarise(n = n()) %>%
  mutate(cod_dominio = "01",
         dominio = "Nacional") %>%
  group_by(dominio, cod_dominio) %>%
  summarise(n_upm = sum(n))


upm_area = marco %>%
  group_by(area) %>%
  summarise(n = n()) %>%
  mutate(cod_dominio = area,
         dominio = area) %>%
  group_by(dominio, cod_dominio) %>%
  summarise(n_upm = sum(n))



marco<- marco %>%
  mutate(canton= substr(id_upm,1,4),
         prov= substr(id_upm,1,2),
         dom10=case_when(canton=="0101" ~ "30",
                         canton=="0601" ~ "31",
                         canton=="1101" ~ "32",
                         canton=="1701" ~ "33",
                         canton=="1801" ~ "34",
                         canton=="2301" ~ "35",
                         canton=="0701"& area==1 ~ "40",
                         canton=="0801"& area==1 ~ "41",
                         canton=="0901"& area==1 ~ "42",
                         canton=="1308"& area==1 ~ "43",
                         T ~ "NA"))


upm_prov = marco %>%
  group_by(prov) %>%
  summarise(n = n()) %>%
  mutate(cod_dominio = prov,
         dominio = prov) %>%
  group_by(dominio, cod_dominio) %>%
  summarise(n_upm = sum(n))



upm_dom10 = marco %>%
  group_by(dom10) %>%
  summarise(n = n()) %>%
  mutate(cod_dominio = dom10,
         dominio = dom10) %>%
  group_by(dominio, cod_dominio) %>%
  summarise(n_upm = sum(n)) %>% filter(!dominio=="NA")


marco_upm<- rbind(upm_nac,upm_area,upm_prov,upm_dom10)

export(marco_upm, "./Tratadas/2. tamaño_de_muestra/marco_upm.xlsx")

