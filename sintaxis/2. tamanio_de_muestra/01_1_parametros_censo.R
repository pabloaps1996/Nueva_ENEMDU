#
rm(list = ls())
#
# carga de librerias
#
library(samplesize4surveys)
library(rio)
library(data.table)
library(tidyverse)
#carga de upm
upm <- import("insumos\\2. tamanio_de_muestra\\man_sec_upm_final_dmq.rds")

#
pob <- import("insumos/2. tamanio_de_muestra/nogit/CPV_Poblacion_2022_Nacional_05_30_2024_SA_DINEM.csv")
names(pob) <- toupper(names(pob))

#
viv <- import("insumos/2. tamanio_de_muestra/nogit/viv_2022.csv")
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
         particular = ifelse(V01 %in% c(1:8), 1, 0)) %>%
  # filtramos las viviendas particulares
  filter(particular == 1) %>%
  #agregamos a nivel de provincia
  group_by(pro) %>%
  summarise(promedio_per = mean(TOTPER))

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
            by = "man_sec")

sum(is.na(poblacion2022$id_upm))

## 5. Calcular indicadores-----------------------------------------------------
poblacion2022 <- as.data.table(poblacion2022)

### CONDICIÓN DE ACTIVIDAD-----------------------------------------------------

# Poblaciones CIET-13

# Población en edad de trabajar (PET)
poblacion2022[P03 >= 15, PET:=1]
# Población empleada (E_CIET13)
poblacion2022[(PET == 1 & P22 %between% c(1,6)), E_CIET13:=1]
# Población desempleada (DE_CIET13)
poblacion2022[(PET == 1 & P22 == 7 & P25 == 1), DE_CIET13:=1]
# Población Económicamente Activa (PEA_CIET13)
poblacion2022[(E_CIET13 ==1 | DE_CIET13 == 1), PEA_CIET13:=1]
# Población Económicamente Inactiva (PEI_CIET13)
poblacion2022[PET == 1 & P22 == 7 & P25 == 2, PEI_CIET13:=1]

# Autoconsumo

# Población en trabajo en la producción de autoconsumo (AUT)
poblacion2022[((PET == 1 & P22 %between% c(2,5) & P23 == 1 & P24 %between% c(3,4))|
                 (PET == 1 & P22 == 6 & P24 %between% c(3,4))), AUT := 1]
# Población en trabajo en la producción de autoconsumo (AUT) que ha buscado trabajo y está disponible para trabajar (AUT_SBD)
poblacion2022[AUT == 1 & P25 == 1, AUT_SBD := 1]
# Población en trabajo en la producción de autoconsumo (AUT) que no ha buscado trabajo ni está disponible para trabajar (AUT_NBD)
poblacion2022[AUT == 1 & P25 == 2, AUT_NBD := 1]

# Poblaciones CIET-19

# Población ocupada (O_CIET19)
poblacion2022[E_CIET13 == 1 & is.na(AUT), O_CIET19:=1]
# Población desocupada (DO_CIET19)
poblacion2022[DE_CIET13 == 1 | AUT_SBD == 1, DO_CIET19:=1]
# Población en la Fuerza de Trabajo (FT_CIET19)
poblacion2022[PEA_CIET13 ==1, FT_CIET19:=1]
poblacion2022[(PEA_CIET13== 1 & AUT_NBD== 1) , FT_CIET19:=NA]
# Población fuera de la Fuerza de Trabajo (FFT_CIET19)
poblacion2022[(PEI_CIET13 | AUT_NBD == 1) , FFT_CIET19:=1]


# calculo de la correlacion intraclase desde el censo
pob1 <- poblacion2022 %>%
  as.data.frame() %>%
  mutate(DO_CIET19 = ifelse(FT_CIET19 == 1 & is.na(DO_CIET19), 0, DO_CIET19)) %>%
  filter(!is.na(DO_CIET19)) %>%
  filter(pro == "08")

ICC(pob1$DO_CIET19, pob1$id_upm)


### agregado de la base a viviendas
cen_viv <-
  mutate()

aggr_viv <- poblacion2022 %>%
  as.data.frame() %>%
  group_by(pro, id_viv) %>%
  summarise(n_per = n()) %>%
  group_by_(pro) %>%
  summarise(promedio_per = mean(n_per))










# samplesize4surveys::ICC()
# function (y, cl)
# {
#   cl <- as.factor(cl)
#   N <- length(y)
#   Ni <- table(cl)
#   t1 <- (tapply(y, cl, mean) - mean(y))^2
#   TSS <- (N - 1) * var(y)
#   BSS <- sum(Ni * t1)
#   WSS <- TSS - BSS
#   ICC <- 1 - (sum(Ni)/sum(Ni - 1)) * (WSS/TSS)
#   res <- list(TSS = TSS, BSS = BSS, WSS = WSS, ICC = ICC)
#   res
# }

