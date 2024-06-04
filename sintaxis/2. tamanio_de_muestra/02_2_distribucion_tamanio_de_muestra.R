## Distribución de la muestra por upms
rm(list = ls())
#
# carga de librerias
#
library(rio)
library(tidyverse)

# Lectura marco
marco <- import("./insumos/2. tamanio_de_muestra/marco_upm.rds")

#lectura tamaño distribución desocupación

load(file = "productos/2. tamanio_de_muestra/tamaño_distribucion.RData")


marco1 <- marco %>%
  # filter(!Mi<40) %>%
  group_by( pro, estrato) %>%
  summarise(Nh = n_distinct(id_upm),
            Mh = sum(Mi))  %>%
  mutate(dom10 = str_sub(estrato,1,2))

aux <- marco1 %>%
  left_join(obj1 %>% rename(pro = dom, tam_pro = upm_max), by="pro") %>%
  left_join(obj4 %>% rename(dom10 = dom, tam_can = upm_max), by="dom10") %>%
  mutate(tam_can = ifelse(is.na(tam_can),0,tam_can))

resumen <- aux %>%
  group_by(pro, estrato) %>%
  summarise(viv = sum(Mh)) %>%
  # ungroup() %>%
  group_by(pro) %>%
  mutate(prop_p = viv/sum(viv)) %>%
  ungroup() %>%
  select(estrato, prop_p)

resumen2 <- aux %>%
  mutate(cant = substr(estrato,1,2)) %>%
  filter(cant>=30) %>%
  group_by(cant, estrato) %>%
  summarise(viv = sum(Mh)) %>%
  # ungroup() %>%
  group_by(cant) %>%
  mutate(prop_c = viv/sum(viv)) %>%
  ungroup() %>%
  select(estrato,prop_c)


base <- aux %>%
  left_join(resumen, by="estrato") %>%
  left_join(resumen2, by="estrato") %>%
  mutate(dis_p = tam_pro*prop_p,
         dis_c = tam_can*prop_c) %>%
  group_by(estrato) %>%
  mutate(dis_f = pmax(dis_p, dis_c, na.rm = T),
         dis_f1 = ifelse(dis_f<4, 4, dis_f),
         dis_f2 = ifelse(dis_f - floor(dis_f1/4)*4 < 2, floor(dis_f1/4)*4, ceiling(dis_f1/4)*4),
         # fraccion de muestreo
         frac_n = dis_f2/Nh)

base[is.na(base)] <- 0

# comparacion con tamaños de la enemdu actual

tam_upm_ac <- import(".\\informacion\\muestra_viviendas_junio_20240601.rds")

tam_upm_ac1 <- tam_upm_ac %>%
  group_by(dom = provin) %>%
  summarise(n = n_distinct(id_conglomerado))

control <- base %>%
  group_by(pro) %>%
  summarise(tam_new = sum(dis_f2)) %>%
  ungroup() %>%
  left_join(tam_upm_ac1 %>%
              rename(pro = dom), by = "pro") %>%
  filter(pro != "20") %>%
  mutate(diff=tam_new-n)

sum(control$tam_new); sum(control$n)



