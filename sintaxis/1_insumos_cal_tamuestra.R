library(rio)
library(tidyverse)
library(srvyr)


anual_23<- import("./insumos/ENEMDU_Anual_Enero_Diciembre_2023_hog_integrado_ciuu_completa.rds")


anual_23_1<- anual_23 %>%
  mutate(dom_ciu=case_when(ciudad=="170150" ~ "25",
                      ciudad=="090150" ~ "26",
                      ciudad=="010150" ~ "27",
                      ciudad=="070150" ~ "28",
                      ciudad=="180150" ~ "29",
                      ciudad=="080150" ~ "30",
                      ciudad=="230150" ~ "31",
                      ciudad=="130850" ~ "32",
                      ciudad=="110150" ~ "33",
                      T ~ NA),
         id_upm_mes= paste0(id_upm, mes),
         canton= substr(ciudad,1,4),
         dmq=ifelse(canton=="1701","34", NA))



base = anual_23_1 %>%
  mutate(one = 1)


enemdu_hog <- base %>% as_survey_design(ids = id_upm_mes,
                                        strata = estrato,
                                        weights = fexp_cal_hog,
                                        nest = T)

options(survey.lonely.psu = "certainty")


enemdu_hog_2 <- base %>% as_survey_design(ids = id_upm,
                                        strata = estrato,
                                        weights = fexp_cal_hog,
                                        nest = T)
# Indicadores por hogar

grupos <- list(enemdu_hog$variables$one,
               enemdu_hog$variables$area,
               enemdu_hog$variables$prov,
               enemdu_hog$variables$dom_ciu,
               enemdu_hog$variables$dmq)

for (i in 1:length(grupos)){
  tabla <- enemdu_hog %>%
    group_by(grupos[[i]]) %>%
    summarise(desem = survey_mean(tdesem, vartype = c("se", "ci", "cv"), na.rm = T, deff = T),
              pobre = survey_mean(tpobre, vartype = c("se", "ci", "cv"), na.rm = T, deff = T))
  if (i==1){
    resultados_hog <- tabla
  } else {
    resultados_hog <- rbind(resultados_hog,tabla)
  }
}
export(resultados_hog, "resultados/resultados_hog_dom_33_mes_2023.xlsx")




