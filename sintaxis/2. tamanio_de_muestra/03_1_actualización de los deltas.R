## Actualización de los deltas


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


parametros_prov <- readRDS("intermedios/2. tamanio_de_muestra/parametros_prov.rds") %>%
  rename(dominio = pro)

parametros_dom10 <- readRDS("intermedios/2. tamanio_de_muestra/parametros_cantones.rds") %>%
  filter(!dom10=="NA") %>%
  mutate(dom10 = ifelse(dom10 == 35 , 44 , dom10)) %>%
  rename(dominio = dom10)


parametros <- rbind(parametros_prov,parametros_dom10) %>%
  mutate(# rango del intervalo a obtener
    delta0= 0.10, # delta del calculo original
    rango_de = 2*delta0*tasa_de,
    LI_de = round(tasa_de - delta0*tasa_de, 4),
    LS_de = round(tasa_de + delta0*tasa_de, 4),
    rango_do = 2*delta0*tasa_do,
    LI_do = round(tasa_do - delta0*tasa_do, 4),
    LS_do = round(tasa_do + delta0*tasa_do, 4))

# %>%
#     select(dominio,tasa_de,LI_de,LS_de,rango_de,
#            tasa_do, LI_do,LS_do,rango_do,delta0)


# Cambios de los deltas

parametros_1 <-  parametros %>%
  mutate(delta1= case_when(dominio == "41" ~ 0.07,
                           dominio == "08" ~ 0.07,
                           dominio == "24" ~ 0.08,
                           dominio == "22" ~ 0.08,
                           #dominio == "14" ~ 0.10,
                           dominio == "04" ~ 0.10,
                           dominio == "15" ~ 0.09,
                           dominio == "21" ~ 0.08,
                           dominio == "43" ~ 0.10,
                           dominio == "13" ~ 0.10,
                           dominio == "02" ~ 0.10,
                           dominio == "19" ~ 0.10,
                           dominio == "12" ~ 0.10,
                           dominio == "17" ~ 0.06,
                           dominio == "09" ~ 0.06,
                           dominio == "15" ~ 0.10,
                           T ~ 0.11),
                           rango_de = 2*delta1*tasa_de,
                           LI_de = round(tasa_de - delta1*tasa_de, 4),
                           LS_de = round(tasa_de + delta1*tasa_de, 4),
                           rango_do = 2*delta1*tasa_do,
                           LI_do = round(tasa_do - delta1*tasa_do, 4),
                           LS_do = round(tasa_do + delta1*tasa_do, 4))



parametros_prov <-  parametros_1 %>% filter(dominio <= 24)
parametros_dom10 <-  parametros_1 %>% filter(dominio > 24)

### Nuevo tamanio provincias

index_pro <- unique(parametros_prov$dominio)

########## DESOCUPACION
for(i in 1:length(index_pro)){

  apoyo <- ss4HHSp(N = parametros_prov$N_per[parametros_prov$dominio == index_pro[i]],
                   r = parametros_prov$r_ft[parametros_prov$dominio == index_pro[i]],
                   b = parametros_prov$b[parametros_prov$dominio == index_pro[i]],
                   rho = parametros_prov$rho_do[parametros_prov$dominio == index_pro[i]],
                   P = parametros_prov$tasa_do[parametros_prov$dominio == index_pro[i]],
                   delta = parametros_prov$delta1[parametros_prov$dominio == index_pro[i]],
                   conf = 0.95,
                   m = 7) %>%
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



############# DESEMPLEO
for(i in 1:length(index_pro)){

  apoyo <- ss4HHSp(N = parametros_prov$N_per[parametros_prov$dominio == index_pro[i]],
                   r = parametros_prov$r_pea[parametros_prov$dominio == index_pro[i]],
                   b = parametros_prov$b[parametros_prov$dominio == index_pro[i]],
                   rho = parametros_prov$rho_de[parametros_prov$dominio == index_pro[i]],
                   P = parametros_prov$tasa_de[parametros_prov$dominio == index_pro[i]],
                   delta = parametros_prov$delta1[parametros_prov$dominio == index_pro[i]],
                   conf = 0.95,
                   m = 7) %>%
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

names(dores7)[2:8] <- paste0(names(dores7)[2:8], "_do")

muestra_do_de_prov <- left_join(dores7,deres7, by="pro")


save(muestra_do_de_prov, file = "productos/2. tamanio_de_muestra/muestra_do_de_prov_deltas.RData")


### Nuevo tamanio dominios
index_dom10 <- unique(parametros_dom10$dominio)

for(i in 1:length(index_dom10)){

  apoyo <- ss4HHSp(N = parametros_dom10$N_per[parametros_dom10$dominio == index_dom10[i]],
                   r = parametros_dom10$r_ft[parametros_dom10$dominio == index_dom10[i]],
                   b = parametros_dom10$b[parametros_dom10$dominio== index_dom10[i]],
                   rho = parametros_dom10$rho_do[parametros_dom10$dominio == index_dom10[i]],
                   P = parametros_dom10$tasa_do[parametros_dom10$dominio == index_dom10[i]],
                   delta = parametros_dom10$delta1[parametros_dom10$dominio == index_dom10[i]],
                   conf = 0.95,
                   m = 7) %>%
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


# DESEMPLEO
for(i in 1:length(index_dom10)){

  apoyo <- ss4HHSp(N = parametros_dom10$N_per[parametros_dom10$dominio == index_dom10[i]],
                   r = parametros_dom10$r_pea[parametros_dom10$dominio == index_dom10[i]],
                   b = parametros_dom10$b[parametros_dom10$dominio == index_dom10[i]],
                   rho = parametros_dom10$rho_de[parametros_dom10$dominio == index_dom10[i]],
                   P = parametros_dom10$tasa_de[parametros_dom10$dominio == index_dom10[i]],
                   delta =  parametros_dom10$delta1[parametros_dom10$dominio == index_dom10[i]],
                   conf = 0.95,
                   m = 7) %>%
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


names(dores7)[2:8] <- paste0(names(dores7)[2:8], "_do")

muestra_do_de_dom10 <- left_join(dores7,deres7, by="dom10")

save(muestra_do_de_dom10, file = "productos/2. tamanio_de_muestra/muestra_do_de_dom10_deltas.RData")




 ### Distribución

obj1 <- muestra_do_de_prov  %>%
  mutate(max = pmax(PSUinSample_do,PSUinSample)/12) %>%
  select(dom=pro, upm_max=max)


obj4 <- muestra_do_de_dom10  %>%
  mutate(max= pmax(PSUinSample_do,PSUinSample)/6) %>%
  select(dom=dom10, upm_max = max)


#tam_upm <- rbind(obj1, obj4)


save(obj1, obj4, file = "productos/2. tamanio_de_muestra/tamaño_distribucion_deltas.RData")

tam_upm_ac <- import(".\\informacion\\muestra_viviendas_junio_20240601.rds")

tam_upm_ac1 <- tam_upm_ac %>%
  group_by(dom = provin) %>%
  summarise(n= n_distinct(id_conglomerado)) %>%
  mutate(n=n*12)

