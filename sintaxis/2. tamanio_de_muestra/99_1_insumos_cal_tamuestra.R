{library(rio)
library(tidyverse)
library(srvyr)
library(readr)}


anual_23<- import("./insumos/2. tamaño_de_muestra/ENEMDU_Anual_Enero_Diciembre_2023_hog_integr.rds") %>% mutate(one=1)



sum(anual_23$fexp_cal_hog)
sum(anual_23$fexp_cal_upm)
summary(anual_23$fexp_acum_aju)
summary(anual_23$fexp_rec45)
table(anual_23$tdesem, useNA = "ifany")
table(anual_23$tpobre, useNA = "ifany")


# Construccion de los 10 dominios de analisis

anual_23<- anual_23 %>%
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
         id_upm_mes= paste0(id_upm, mes) )



anual_23<- anual_23 %>% group_by(id_upm,id_upm_mes,estrato,area,dom10,provin,tdesem,tpobre, id_hog) %>%
  summarise(fexp_cal_hog=mean(fexp_cal_hog)) %>%
  ungroup()
unique(anual_23$tpobre)


# Estimación Diseño de Muestreo
plan = anual_23 %>%
  as_survey_design(ids = id_upm_mes,
                   strata = estrato,
                   weights = fexp_cal_hog,
                   nest = T)
options(survey.lonely.psu = "certainty")

# Nacional
est_nac = plan %>%
  summarise(desempleo = survey_mean(tdesem,
                                    vartype = c("se","ci","cv"),
                                    deff = T,
                                    na.rm = T),
          pobreza = survey_mean(tpobre,
                                    vartype = c("se","ci","cv"),
                                    deff = T,
                                    na.rm = T),
          n_upm=n_distinct(id_upm_mes)) %>%
          #pea = survey_total(pean, na.rm = T)) %>%
  mutate(cod_dominio="00") %>%
   relocate(cod_dominio)


# Area

est_area = plan %>%
  group_by(area) %>%
  summarise(desempleo = survey_mean(tdesem,
                                    vartype = c("se","ci","cv"),
                                    deff = T,
                                    na.rm = T),
            pobreza = survey_mean(tpobre,
                                  vartype = c("se","ci","cv"),
                                  deff = T,
                                  na.rm = T),
            n_upm=n_distinct(id_upm_mes)) %>%
            #pea = survey_total(pean, na.rm = T))%>%
  rename(cod_dominio = area)


# Provincias
est_prov = plan %>%
  group_by(provin) %>%
  summarise(desempleo = survey_mean(tdesem,
                                    vartype = c("se","ci","cv"),
                                    deff = T,
                                    na.rm = T),
            pobreza = survey_mean(tpobre,
                                  vartype = c("se","ci","cv"),
                                  deff = T,
                                  na.rm = T),
            n_upm=n_distinct(id_upm_mes)) %>%
            #pea = survey_total(pean, na.rm = T)) %>%
  mutate(cod_dominio = provin) %>%
  relocate(cod_dominio) %>% select(-provin)


# dominios 10
est_dom10 = plan %>%
  group_by(dom10) %>%
  summarise(desempleo = survey_mean(tdesem,
                                    vartype = c("se","ci","cv"),
                                    deff = T,
                                    na.rm = T),
            pobreza = survey_mean(tpobre,
                                  vartype = c("se","ci","cv"),
                                  deff = T,
                                  na.rm = T),
            n_upm=n_distinct(id_upm_mes)) %>%
            #pea = survey_total(pean, na.rm = T)) %>%
  mutate(cod_dominio = dom10) %>%
  relocate(cod_dominio) %>% select(-dom10) %>% filter(!cod_dominio=="NA")





resultados_hog<- rbind(est_nac,est_area,est_prov,est_dom10)


nombre<- import("./insumos/dominios.xlsx") %>% rename(cod_dominio=provin)



resultados_hog<- resultados_hog %>%
            left_join(nombre, by = "cod_dominio") %>%
            mutate(nprovin=case_when(cod_dominio=="00"~"NACIONAL",
                                     cod_dominio=="1"~"URBANO",
                                     cod_dominio=="2"~"RURAL",
                                     T~nprovin ),
                   cod_dominio=nprovin) %>% select(-nprovin)


export(resultados_hog, "resultados/estimaciones_dese_pobr_ump_mes.xlsx")




