
# CALCULO DEL TAMAÑO DE MUESTRA POR DOMINIOS

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

d <- 0.10

# lectura de parametros provinciales
parametros_prov <- readRDS("intermedios/2. tamanio_de_muestra/parametros_prov.rds")

index_pro <- unique(parametros_prov$pro)

########## DESOCUPACION
for(i in 1:length(index_pro)){

  apoyo <- ss4HHSp(N = parametros_prov$N_per[parametros_prov$pro == index_pro[i]],
                   r = parametros_prov$r_ft[parametros_prov$pro == index_pro[i]],
                   b = parametros_prov$b[parametros_prov$pro == index_pro[i]],
                   rho = parametros_prov$rho_do[parametros_prov$pro == index_pro[i]],
                   P = parametros_prov$tasa_do[parametros_prov$pro == index_pro[i]],
                   delta = d,
                   conf = 0.95,
                   m = c(1:20)) %>%
    mutate(pro = index_pro[i],
           variable = "desocupacion") %>%
    select(7, 8, 1:6)

  if(i == 1){
    muestra_prov_do <- apoyo
  }else{
    muestra_prov_do <- rbind(muestra_prov_do, apoyo)
  }

}

dores7 <- muestra_prov_do %>%
  filter(HouseholdsPerPSU == 7)

sum(dores7$PSUinSample)/12


############# DESEMPLEO
for(i in 1:length(index_pro)){

  apoyo <- ss4HHSp(N = parametros_prov$N_per[parametros_prov$pro == index_pro[i]],
                   r = parametros_prov$r_pea[parametros_prov$pro == index_pro[i]],
                   b = parametros_prov$b[parametros_prov$pro == index_pro[i]],
                   rho = parametros_prov$rho_de[parametros_prov$pro == index_pro[i]],
                   P = parametros_prov$tasa_de[parametros_prov$pro == index_pro[i]],
                   delta = d,
                   conf = 0.95,
                   m = c(1:20)) %>%
    mutate(pro = index_pro[i],
           variable = "desempleo") %>%
    select(7, 8, 1:6)

  if(i == 1){
    muestra_prov_de <- apoyo
  }else{
    muestra_prov_de <- rbind(muestra_prov_de, apoyo)
  }

}

deres7 <- muestra_prov_de %>%
  filter(HouseholdsPerPSU == 7)

sum(deres7$PSUinSample)/12

names(dores7)[2:8] <- paste0(names(dores7)[2:8], "_do")

muestra_do_de_prov <- left_join(dores7,deres7, by="pro")

save(muestra_do_de_prov, file = "productos/2. tamanio_de_muestra/muestra_do_de_prov.RData")


# # lectura de parametros nac
# parametros_nac <- readRDS("intermedios/2. tamanio_de_muestra/parametros_nac.rds")
#
# index_nac <- unique(parametros_nac$nac)
#
# for(i in 1:length(index_nac)){
#
#   apoyo <- ss4HHSp(N = parametros_nac$N_per[parametros_nac$nac == index_nac[i]],
#                    r = parametros_nac$r_ft[parametros_nac$nac == index_nac[i]],
#                    b = parametros_nac$b[parametros_nac$nac == index_nac[i]],
#                    rho = parametros_nac$rho_do[parametros_nac$nac == index_nac[i]],
#                    P = parametros_nac$tasa_do[parametros_nac$nac == index_nac[i]],
#                    delta = d,
#                    conf = 0.95,
#                    m = c(1:20)) %>%
#     mutate(nac = index_nac[i],
#            variable = "desocupacion") %>%
#     select(7, 8, 1:6)
#
#   if(i == 1){
#     muestra_nac_do <- apoyo
#   }else{
#     muestra_nac_do <- rbind(muestra_nac_do, apoyo)
#   }
#
# }
#
# dores7 <- muestra_nac_do %>%
#   filter(HouseholdsPerPSU == 7)
#
# sum(dores7$PSUinSample)/12
#
#
# ##### DESEMPLEO
# for(i in 1:length(index_nac)){
#
#   apoyo <- ss4HHSp(N = parametros_nac$N_per[parametros_nac$nac == index_nac[i]],
#                    r = parametros_nac$r_pea[parametros_nac$nac == index_nac[i]],
#                    b = parametros_nac$b[parametros_nac$nac == index_nac[i]],
#                    rho = parametros_nac$rho_de[parametros_nac$nac == index_nac[i]],
#                    P = parametros_nac$tasa_de[parametros_nac$nac == index_nac[i]],
#                    delta = d,
#                    conf = 0.95,
#                    m = c(1:20)) %>%
#     mutate(nac = index_nac[i],
#            variable = "desempleo") %>%
#     select(7, 8, 1:6)
#
#   if(i == 1){
#     muestra_nac_de <- apoyo
#   }else{
#     muestra_nac_de <- rbind(muestra_nac_de, apoyo)
#   }
#
# }
#
# deres7 <- muestra_nac_de %>%
#   filter(HouseholdsPerPSU == 7)
#
# sum(deres7$PSUinSample)/12
#
# names(dores7)[2:8] <- paste0(names(dores7)[2:8], "_do")
#
# muestra_do_de_nac <- left_join(dores7,deres7, by="nac")
#
# save(muestra_do_de_nac, file = "productos/2. tamanio_de_muestra/muestra_do_de_nac.RData")
#
# # lectura de parametros area
# parametros_area <- readRDS("intermedios/2. tamanio_de_muestra/parametros_area.rds")
#
# index_area <- unique(parametros_area$area)
#
# for(i in 1:length(index_area)){
#
#   apoyo <- ss4HHSp(N = parametros_area$N_per[parametros_area$area == index_area[i]],
#                    r = parametros_area$r_ft[parametros_area$area == index_area[i]],
#                    b = parametros_area$b[parametros_area$area == index_area[i]],
#                    rho = parametros_area$rho_do[parametros_area$area == index_area[i]],
#                    P = parametros_area$tasa_do[parametros_area$area == index_area[i]],
#                    delta = d,
#                    conf = 0.95,
#                    m = c(1:20)) %>%
#     mutate(area = index_area[i],
#            variable = "desocupacion") %>%
#     select(7, 8, 1:6)
#
#   if(i == 1){
#     muestra_area_do <- apoyo
#   }else{
#     muestra_area_do <- rbind(muestra_area_do, apoyo)
#   }
#
# }
#
# dores7 <- muestra_area_do %>%
#   filter(HouseholdsPerPSU == 7)
#
# sum(dores7$PSUinSample)/12
#
# ### desempleo
# for(i in 1:length(index_area)){
#
#   apoyo <- ss4HHSp(N = parametros_area$N_per[parametros_area$area == index_area[i]],
#                    r = parametros_area$r_pea[parametros_area$area == index_area[i]],
#                    b = parametros_area$b[parametros_area$area == index_area[i]],
#                    rho = parametros_area$rho_de[parametros_area$area == index_area[i]],
#                    P = parametros_area$tasa_de[parametros_area$area == index_area[i]],
#                    delta = d,
#                    conf = 0.95,
#                    m = c(1:20)) %>%
#     mutate(area = index_area[i],
#            variable = "desempleo") %>%
#     select(7, 8, 1:6)
#
#   if(i == 1){
#     muestra_area_de <- apoyo
#   }else{
#     muestra_area_de <- rbind(muestra_area_de, apoyo)
#   }
#
# }
#
# de_res7 <- muestra_area_de %>%
#   filter(HouseholdsPerPSU == 7)
#
# sum(de_res7$PSUinSample)/12
#
#
# names(dores7)[2:8] <- paste0(names(dores7)[2:8], "_do")
#
# muestra_do_de_area <- left_join(dores7,de_res7, by="area")
#
# save(muestra_do_de_area, file = "productos/2. tamanio_de_muestra/muestra_do_de_area.RData")
#
#
# lectura de parametros dom10
parametros_dom10 <- readRDS("intermedios/2. tamanio_de_muestra/parametros_cantones.rds") %>%
  filter(!dom10=="NA")

index_dom10 <- unique(parametros_dom10$dom10)

for(i in 1:length(index_dom10)){

  apoyo <- ss4HHSp(N = parametros_dom10$N_per[parametros_dom10$dom10 == index_dom10[i]],
                   r = parametros_dom10$r_ft[parametros_dom10$dom10 == index_dom10[i]],
                   b = parametros_dom10$b[parametros_dom10$dom10 == index_dom10[i]],
                   rho = parametros_dom10$rho_do[parametros_dom10$dom10 == index_dom10[i]],
                   P = parametros_dom10$tasa_do[parametros_dom10$dom10 == index_dom10[i]],
                   delta = d,
                   conf = 0.95,
                   m = c(1:20)) %>%
    mutate(dom10 = index_dom10[i],
           variable = "desocupacion") %>%
    select(7, 8, 1:6)

  if(i == 1){
    muestra_dom10_do <- apoyo
  }else{
    muestra_dom10_do <- rbind(muestra_dom10_do, apoyo)
  }

}

dores7 <- muestra_dom10_do %>%
  filter(HouseholdsPerPSU == 7)

sum(dores7$PSUinSample)/12

# DESEMPLEO
for(i in 1:length(index_dom10)){

  apoyo <- ss4HHSp(N = parametros_dom10$N_per[parametros_dom10$dom10 == index_dom10[i]],
                   r = parametros_dom10$r_pea[parametros_dom10$dom10 == index_dom10[i]],
                   b = parametros_dom10$b[parametros_dom10$dom10 == index_dom10[i]],
                   rho = parametros_dom10$rho_de[parametros_dom10$dom10 == index_dom10[i]],
                   P = parametros_dom10$tasa_de[parametros_dom10$dom10 == index_dom10[i]],
                   delta = d,
                   conf = 0.95,
                   m = c(1:20)) %>%
    mutate(dom10 = index_dom10[i],
           variable = "desempleo") %>%
    select(7, 8, 1:6)

  if(i == 1){
    muestra_dom10_de <- apoyo
  }else{
    muestra_dom10_de <- rbind(muestra_dom10_de, apoyo)
  }

}

deres7 <- muestra_dom10_de %>%
  filter(HouseholdsPerPSU == 7)

sum(deres7$PSUinSample)/12


names(dores7)[2:8] <- paste0(names(dores7)[2:8], "_do")

muestra_do_de_dom10 <- left_join(dores7,deres7, by="dom10")

save(muestra_do_de_dom10, file = "productos/2. tamanio_de_muestra/muestra_do_de_dom10.RData")

obj1 <- muestra_do_de_prov  %>%
  mutate(max = pmax(PSUinSample_do,PSUinSample)/12) %>%
  select(dom=pro, upm_max=max)
    # %>%
  # mutate(upm = round(upm/12, 0))

# obj2 <- muestra_do_de_nac  %>%
#   mutate(max= pmax(PSUinSample_do,PSUinSample)) %>%
#   select(dom=nac, upm_max= max)
#
# obj3 <- muestra_do_de_area  %>%
#   mutate(max= pmax(PSUinSample_do,PSUinSample)) %>%
#   select(dom=area, upm_max= max)

obj4 <- muestra_do_de_dom10  %>%
  mutate(max= pmax(PSUinSample_do,PSUinSample)/6) %>%
  select(dom=dom10, upm_max = max)

tam_upm <- rbind(obj1, obj4)

save(obj1, obj4, file = "productos/2. tamanio_de_muestra/tamaño_distribucion.RData")

tam_upm_ac <- import(".\\informacion\\muestra_viviendas_junio_20240601.rds")

tam_upm_ac1 <- tam_upm_ac %>%
  group_by(dom = provin) %>%
  summarise(n= n_distinct(id_conglomerado)) %>%
  mutate(n=n*12)

at <- left_join(tam_upm,tam_upm_ac1, by= "dom")


# mutate(upm = round(upm/12, 0))
# %>%
# mutate(upm = round(upm/12, 0))





# ob1_1<- muestra_do_de_prov %>%
#   filter(variable == "desempleo") %>%
#   select(pro, upm = PSUinSample) %>%
#   mutate(upm = round(upm/12, 0))

# obj2 <- muestra_do_de_dom10 %>%
#   filter(variable == "desocupacion") %>%
#   select(dom10, upm = PSUinSample) %>%
#   mutate(upm = round(upm/6, 0))




# deff = 1+((v*promedio)-1)*rho

# muestra_desem <- resultado_desemp %>%
#   mutate(# definicio de deltas
#     # delta = 0.15,
#     # delta = 0.016/(2*tasa),
#     rango_deseado = case_when(cod == "02" ~ 0.01,
#                               cod == "06" ~ 0.012,
#                               cod == "08" ~ 0.025,
#                               cod == "14" ~ 0.01,
#                               cod == "35" ~ 0.0125, #sto domingo_c
#                               cod == "41" ~ 0.035, # esmeraldas_c
#                               cod == "42" ~ 0.015,
#                               cod == "43" ~ 0.01, # manta_c
#                               cod == "13" ~ 0.01, # manabi
#                               cod == "13" ~ 0.01,
#                               T~ 0.02),
#     delta = rango_deseado/(2*tasa))
