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
pob <- import("insumos/2. tamanio_de_muestra/nogit/CPV_Poblacion_2022_Nacional_05_30_2024_SA_DINEM.csv")
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
         P03, P22, P23, P24, P25)

# eliminamos bases que ya no usamos
rm(pob, viv, viv1, a2000, upm)

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

# Creación de la variable dominio 10



poblacion2022<-poblacion2022 %>%
  mutate( canton = paste0(pro, can),
         dom10=case_when(canton=="0101" ~ "30",
                         canton=="0601" ~ "31",
                         canton=="1101" ~ "32",
                         canton=="1701" ~ "33",
                         canton=="1801" ~ "34",
                         canton=="2301"& area==1 ~ "35",
                         canton=="0701"& area==1 ~ "40",
                         canton=="0801"& area==1 ~ "41",
                         canton=="0901"& area==1 ~ "42",
                         canton=="1308"& area==1 ~ "43",
                         T ~ "NA"))





# calculo de la correlacion intraclase desde el censo
dom10_rho_do <- poblacion2022 %>%
  as.data.frame() %>%
  mutate(DO_CIET19 = ifelse(FT_CIET19 == 1 & is.na(DO_CIET19), 0, DO_CIET19)) %>%
  filter(!is.na(DO_CIET19)) %>%
  group_by(dom10) %>%
  summarise(rho_do = ICC(DO_CIET19, id_upm)$ICC)

dom10_rho_de <- poblacion2022 %>%
  as.data.frame() %>%
  mutate(DE_CIET13 = ifelse(PEA_CIET13 == 1 & is.na(DE_CIET13), 0, DE_CIET13)) %>%
  filter(!is.na(DE_CIET13)) %>%
  group_by(dom10) %>%
  summarise(rho_de = ICC(DE_CIET13, id_upm)$ICC)

### parametros necesrios a nivel de provincia
aggr_dom10 <- poblacion2022 %>%
  as.data.frame() %>%
  group_by(dom10, id_viv) %>%
  summarise(N_per = n(),
            N_per_pea = sum(PEA_CIET13, na.rm = T),
            N_per_de = sum(DE_CIET13, na.rm = T),
            N_per_ft = sum(FT_CIET19, na.rm = T),
            N_per_do = sum(DO_CIET19, na.rm = T)) %>%
  group_by(dom10) %>%
  summarise(promedio_per_viv = mean(N_per),
            N_per = sum(N_per),
            N_per_pea = sum(N_per_pea),
            N_per_de = sum(N_per_de),
            N_per_ft = sum(N_per_ft),
            N_per_do = sum(N_per_do))

parametros_dom10 <- aggr_dom10 %>%
  mutate(tasa_de = N_per_de/N_per_pea,
         tasa_do = N_per_do/N_per_ft,
         r_pea = N_per_pea/N_per,
         r_ft = N_per_ft/N_per) %>%
  full_join(dom10_rho_de,
            by = "dom10") %>%
  full_join(dom10_rho_do,
            by = "dom10") %>%
  select(dom10, N_per, b = promedio_per_viv,
         N_per_pea, N_per_de, tasa_de, r_pea, rho_de,
         N_per_ft, N_per_do, tasa_do, r_ft, rho_do)


saveRDS(parametros_dom10, "intermedios/2. tamanio_de_muestra/parametros_cantones.rds")




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

