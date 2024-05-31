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




# CARGA DE BASES  ---------------------------------------------------------
## CENSO_POBLACION
censo = readRDS("./insumos/2. tamaño_de_muestra/poblacion.rds")

censo1<-censo %>%
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
         provin = provincia,
         pet= ifelse(p03>=15, 1, 0),
         pea= ifelse(pet==1 & p22 %in% c(1:6),1,0),
         pea= ifelse(pet==1 & p22==7 & p25==1, 1, 0))

# base de la ENEMDU (anual personas)
base<- readRDS("insumos/2. tamaño_de_muestra/ENEMDU_Anual_Enero_Diciembre_2023_hog_integr.rds")

table(base$pean)

# Construcción de los dominios10
base<- base %>%
  mutate(canton= substr(ciudad,1,4),
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
                         T ~ "NA"),
         one="NACIONAL",
         estrato_mes= paste0(estrato, mes))

  b0<- base %>%
  select(id_upm, id_viv, id_hog, upm_mes,mes,
         estrato,estrato_mes,area,dom10,pean,
         provin,tdesem,tpobre,edad,
         fexp_hog= fexp_cal_hog,
         fexp_upm= fexp_cal_upm,one)

# validaciones
table(b0$tdesem, useNA = "ifany")
table(b0$tpobre, useNA = "ifany")
sum(b0$fexp_hog)
sum(b0$fexp_upm)
summary(b0$fexp_hog)
summary(b0$fexp_upm)



# DISEÑO DE MUESTREO  -----------------------------------------------------
# plan <- b0 %>% as_survey_design(ids = id_upm,
#                                 strata = estrato,
#                                 weights = fexp_hog,
#                                 nest = T)
# options(survey.lonely.psu = "certainty")


plan <- b0 %>% as_survey_design(ids = upm_mes,
                                strata = estrato_mes,
                                weights = fexp_hog,
                                nest = F)
options(survey.lonely.psu = "certainty")

# CALCULO DE LOS TOTALES POBLACIONELES NECESARIOS PARA EL CALCULO (CENSO)---------------
pob_nac = data.frame(
  dominio = "NACIONAL",
  N_p = nrow(censo1),    ### población total
  N_pea= sum(censo1$pea)) ### pea??

pob_area = censo1 %>%
  group_by(area_2000) %>%
  summarise(N_p = n(),
            N_pea=sum(pea)) %>%
  select(dominio = area_2000,N_p,N_pea)

pob_provin = censo1 %>%
  group_by(provincia) %>%
  summarise(N_p = n(),
            N_pea=sum(pea)) %>%
  relocate(dominio=provincia)

pob_dom10 = censo1 %>%
  group_by(dom10) %>%
  summarise(N_p = n(),
            N_pea=sum(pea)) %>%
  relocate(dominio=dom10) %>%
  filter(!dominio=="NA")

resumen = rbind(pob_nac, pob_area, pob_provin, pob_dom10)
rm(pob_nac, pob_area, pob_provin, pob_dom10)



# CALCULO DE PERSONAS POR UPM (VARIABLES DE INTERES) ----------------------
b0_1<- b0 %>% mutate(pean=ifelse(is.na(pean) , 0, pean))
## VARIABLE DESEMPLEO
pro_per_nac = b0_1  %>%
  group_by(one, id_upm,mes, id_viv, provin) %>%
  summarise(n_pea = sum(pean)) %>%
  group_by(one, id_upm, mes,provin) %>%
  summarise(n_viv = n(),
            n_pea = sum(n_pea)) %>%
  mutate(dominio = "NACIONAL") %>%
  group_by(one) %>%
  summarise(n_viv_total = sum(n_viv),
            n_viv_promedio= mean(n_viv),
            n_pea = sum(n_pea)) %>%
  mutate(promedio = n_pea/n_viv_total) %>%
  rename(dominio=one)

pro_per_area = b0_1  %>%
  group_by(area, id_upm,mes, id_viv) %>%
  summarise(n_pea = sum(pean)) %>%
  group_by(area, id_upm, mes) %>%
  summarise(n_viv = n(),
            n_pea = sum(n_pea)) %>%
  group_by(area) %>%
summarise(n_viv_total = sum(n_viv),
            n_viv_promedio= mean(n_viv),
          n_pea = sum(n_pea)) %>%
  mutate(promedio = n_pea/n_viv_total)%>%
  rename(dominio=area)


pro_per_prov = b0_1  %>%
  group_by(provin, id_upm,mes, id_viv) %>%
  summarise(n_pea = sum(pean)) %>%
  group_by(provin, id_upm, mes) %>%
  summarise(n_viv = n(),
            n_pea = sum(n_pea)) %>%
  group_by(provin) %>%
  summarise(n_viv_total = sum(n_viv),
            n_viv_promedio= mean(n_viv),
            n_pea = sum(n_pea)) %>%
  mutate(promedio = n_pea/n_viv_total)%>%
  rename(dominio=provin)


pro_per_dom10 = b0_1  %>%
  group_by(dom10, id_upm,mes, id_viv) %>%
  summarise(n_pea = sum(pean)) %>%
  group_by(dom10, id_upm, mes) %>%
  summarise(n_viv = n(),
            n_pea = sum(n_pea)) %>%
  group_by(dom10) %>%
  summarise(n_viv_total = sum(n_viv),
            n_viv_promedio= mean(n_viv),
            n_pea = sum(n_pea)) %>%
  mutate(promedio = n_pea/n_viv_total)%>%
  rename(dominio=dom10)



pro_per_desemp<- rbind(pro_per_nac,pro_per_area,pro_per_prov,pro_per_dom10)

# ## VARIABLE POBREZA
#
# pro_per_nac<-b0  %>%
#   group_by(one, id_upm,mes, id_viv) %>%
#   summarise(total = n())%>%
#   group_by(one, id_upm, mes) %>%
#   summarise(n_viv = n(),
#             total = sum(total)) %>%
#   mutate(dominio = "NACIONAL") %>%
#   group_by(one) %>%
#   summarise(n_viv_total = sum(n_viv),
#             n_viv_promedio= mean(n_viv),
#             total = sum(total)) %>%
#   mutate(promedio = total/n_viv_total) %>%
#   rename(dominio=one)
#
#
# pro_per_area = b0  %>%
#   group_by(area, id_upm,mes, id_viv) %>%
#   summarise(total = n())%>%
#   group_by(area, id_upm, mes) %>%
#   summarise(n_viv = n(),
#             total = sum(total)) %>%
#   mutate(dominio = "NACIONAL") %>%
#   group_by(area) %>%
#   summarise(n_viv_total = sum(n_viv),
#             n_viv_promedio= mean(n_viv),
#             total = sum(total)) %>%
#   mutate(promedio = total/n_viv_total) %>%
#   rename(dominio=area)
#
# pro_per_prov = b0  %>%
#   group_by(provin, id_upm,mes, id_viv) %>%
#   summarise(total = n())%>%
#   group_by(provin, id_upm, mes) %>%
#   summarise(n_viv = n(),
#             total = sum(total)) %>%
#   mutate(dominio = "NACIONAL") %>%
#   group_by(provin) %>%
#   summarise(n_viv_total = sum(n_viv),
#             n_viv_promedio= mean(n_viv),
#             total = sum(total)) %>%
#   mutate(promedio = total/n_viv_total) %>%
#   rename(dominio=provin)
#
# pro_per_dom10 =b0  %>%
#   group_by(dom10, id_upm,mes, id_viv) %>%
#   summarise(total = n())%>%
#   group_by(dom10, id_upm, mes) %>%
#   summarise(n_viv = n(),
#             total = sum(total)) %>%
#   mutate(dominio = "NACIONAL") %>%
#   group_by(dom10) %>%
#   summarise(n_viv_total = sum(n_viv),
#             n_viv_promedio= mean(n_viv),
#             total = sum(total)) %>%
#   mutate(promedio = total/n_viv_total) %>%
#   rename(dominio=dom10)
#
#
# pro_per_pobre<- rbind(pro_per_nac,pro_per_area,pro_per_prov,pro_per_dom10)


# ESTIMACION DE LA VARIABLE DE INTERES ------------------------------------

  var<-c("tdesem")

grupos <- list(plan$variables$one,
               plan$variables$area,
               plan$variables$provin,
               plan$variables$dom10)

for (j in 1:length(grupos)){

  for(i in 1:length(var)){

    v0 <- as.name(var[i])
    v0 <- enquo(v0)

    v1 <- plan %>%
      group_by(grupos[[j]]) %>%
      summarise(tasa = survey_mean(!!v0, na.rm=T,
                                   vartype = c("cv", "se", "ci"),
                                   deff = T),
                n_per = unweighted(n()),
                pea = survey_total(pean, na.rm = T)) %>%
      mutate(variable = var[i]) %>%
      select(1, 8, 2:7,n_per,pea)

    if(i==1){
      resultado  <- v1
    }else{
      resultado <- rbind(resultado, v1)
    }
  }
  if(j==1){
    rf  <- resultado
  }else{
    rf <- rbind(rf, resultado)
  }
}

resultado=rf
resultado <- resultado %>%
  mutate_if(is.numeric, round, 3)%>%
  rename(dominio=`grupos[[j]]`) %>%
  filter(dominio!="NA")

rm(v0, v1, i)



# UNIR LAS CALCULOS DE CADA VARIABLE --------------------------------------

nombre<- import("./insumos/2. tamaño_de_muestra/dominios.xlsx") %>% rename(dominio=provin)

N_pea<-resumen %>% select(dominio,N_pea,N_p) # Población en edad de trabajar (desempleo)

N_p<-resumen %>% select(dominio,N_p) # Población total




resultado_desemp<- resultado %>%
  left_join(N_pea, by="dominio") %>%
  left_join(pro_per_desemp, by="dominio")


resultado_pobreza<- resultado %>%
  left_join(N_p, by="dominio") %>%
  left_join(pro_per_pobre, by="dominio")


resultado_desemp<- resultado_desemp %>%
  left_join(nombre, by = "dominio") %>%
  mutate(nprovin=case_when(dominio=="NACIONAL" ~ "NACIONAL",
                           dominio=="1" ~ "URBANO",
                           dominio=="2" ~ "RURAL",
                           T ~ nprovin),
         dominio=nprovin) %>% select(-nprovin)



rm(list = setdiff(ls(), c("resultado_desemp", "resultado_pobreza")))

export(resultado_desemp, "Tratadas/2. tamaño_de_muestra/resultado_desemp.xlsx")
export(resultado_pobreza, "Tratadas/2. tamaño_de_muestra/resultado_pobreza.xlsx")






