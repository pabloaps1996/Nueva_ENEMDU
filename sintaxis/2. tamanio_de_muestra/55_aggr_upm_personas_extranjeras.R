#
rm(list = ls())
#
# carga de librerias
#
library(samplesize4surveys)
library(rio)
library(data.table)
library(tidyverse)
# carga de upm
upm <- import("insumos\\2. tamanio_de_muestra\\man_sec_upm_final_dmq.rds")

# variable area 2000
a2000 <- import("insumos\\2. tamanio_de_muestra\\parroquia_amadis_area.rds")

#
pob <- import("insumos/2. tamanio_de_muestra/base proyecciones/poblacion2022_proyec_3011.csv")
names(pob) <- toupper(names(pob))

#
viv <- import("insumos/2. tamanio_de_muestra/nogit/CPV_Vivienda_2022_Nacional_05_30_2024_SA_DINEM.csv")
names(viv) <- toupper(names(viv))

viv1 <- viv %>%
  mutate(pro = str_pad(I01, 2, "left", pad = "0"),
         can = str_pad(I02, 2, "left", pad = "0"),
         par = str_pad(I03, 2, "left", pad = "0"),
         zon = str_pad(I04, 3, "left", pad = "0"),
         sec = str_pad(I05, 3, "left", pad = "0"),
         man = str_pad(I06, 3, "left", pad = "0"),
         loc = str_pad(I07, 3, "left", pad = "0"),
         edi = str_pad(I08, 3, "left", pad = "0"),
         viv = str_pad(I10, 3, "left", pad = "0"),
         id_man_loc = ifelse(zon == "999",
                             paste0(pro, can, par, zon, sec, loc),
                             paste0(pro, can, par, zon, sec, man)),
         id_viv = paste0(id_man_loc, edi, viv),
         particular = ifelse(V01 %in% c(1:8), 1, 0),
         # creacion de variable para emparejar area 2000
         id_par = paste0(pro, can, par),
         amadis = ifelse(zon == "999", "dis", "ama")) %>%
  left_join(a2000 %>%
              select(-per),
            by = c("id_par", "amadis")) %>%
  # filtramos las viviendas particulares y seleccionamos variables necesarias
  filter(particular == 1) %>%
  select(id_viv, area, particular)

poblacion2022 <- pob %>%
  mutate(# creacion de mansec
    pro = str_pad(I01, 2, "left", pad = "0"),
    can = str_pad(I02, 2, "left", pad = "0"),
    par = str_pad(I03, 2, "left", pad = "0"),
    zon = str_pad(I04, 3, "left", pad = "0"),
    sec = str_pad(I05, 3, "left", pad = "0"),
    man = str_pad(I06, 3, "left", pad = "0"),
    loc = str_pad(I07, 3, "left", pad = "0"),
    edi = str_pad(I08, 3, "left", pad = "0"),
    viv = str_pad(I10, 3, "left", pad = "0"),
    man_sec = ifelse(zon == "999",
                     paste0(pro, can, par, zon, sec),
                     paste0(pro, can, par, zon, sec, man)),
    id_man_loc = ifelse(zon == "999",
                        paste0(pro, can, par, zon, sec, loc),
                        paste0(pro, can, par, zon, sec, man)),
    id_viv = paste0(id_man_loc, edi, viv)) %>%
  # pet = ifelse(P03>=15, 1, 0),
  # pea = ifelse(P03<15 , NA, ifelse(P22==7 & P25==2, 0, 1))) %>%
  # emparejamos man_sec
  left_join(upm %>%
              select(man_sec, id_upm),
            by = "man_sec") %>%
  left_join(viv1,
            by = "id_viv") %>%
  # filtramos personas en viviendas particulares
  filter(!is.na(particular)) %>%
  # seleccionamos variables necesarias
  select(pro, can, par, area, id_upm, id_viv, hog = INH, per = P00,
         P08)

aggr_prov <- poblacion2022 %>%
  group_by(pro, id_upm, id_viv) %>%
  summarise(N_per = n(),
            N_per_ext = sum(P08 == 3, na.rm = T)) %>%
  mutate(viv_ext = ifelse(N_per_ext == 0, 0, 1)) %>%
  group_by(pro, id_upm) %>%
  summarise(N_per = n(),
            tiene_ext = sum(viv_ext))

saveRDS(aggr_prov, "intermedios/2. tamanio_de_muestra/per_ext_upm.rds")




