
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


# lectura de parametros provinciales
parametros_prov <- readRDS("intermedios/2. tamanio_de_muestra/parametros_prov.rds")

index_pro <- unique(parametros_prov$pro)

for(i in 1:length(index_pro)){

  apoyo <- ss4HHSp(N = parametros_prov$N_per[parametros_prov$pro == index_pro[i]],
                   r = parametros_prov$r_ft[parametros_prov$pro == index_pro[i]],
                   b = parametros_prov$b[parametros_prov$pro == index_pro[i]],
                   rho = parametros_prov$rho_do[parametros_prov$pro == index_pro[i]],
                   P = parametros_prov$tasa_do[parametros_prov$pro == index_pro[i]],
                   delta = 0.10,
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

res7 <- muestra_prov_do %>%
  filter(HouseholdsPerPSU == 7)

sum(res7$PSUinSample)/12


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









































