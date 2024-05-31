
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


#resultado_desemp <- import("Tratadas/2. tamaño_de_muestra/resultado_desemp.xlsx")

#resultado_pobreza<-import("Tratadas/2. tamaño_de_muestra/resultado_pobreza.xlsx")
resultado_marco <- import("Tratadas/2. tamaño_de_muestra/marco_upm.xlsx")

resultado_desemp <- import("Tratadas/2. tamaño_de_muestra/resultado_desemp.xlsx") %>%
  left_join(resultado_marco,
            by=c("dominio")) %>%
  relocate(cod) %>%
  mutate(tasa = tasa/100,
         v = 7,
         # cambiando el deff de bolivar a 1,5 porque el rho
         # esta saliendo negativo
         tasa_deff = ifelse(cod == "02", 1.5, tasa_deff),
         # calculo de la correlacion intraclase
         rho = (tasa_deff-1)/((n_viv_promedio*promedio)-1),
         deff = 1+((v*promedio)-1)*rho,
         tnr = 0.20) %>%
  select(-n_upm)


muestra_desem <- resultado_desemp %>%
  mutate(# definicio de deltas
    # delta = 0.15,
    # delta = 0.016/(2*tasa),
    rango_deseado = case_when(cod == "02" ~ 0.01,
                              cod == "06" ~ 0.012,
                              cod == "08" ~ 0.025,
                              cod == "14" ~ 0.01,
                              cod == "35" ~ 0.0125, #sto domingo_c
                              cod == "41" ~ 0.035, # esmeraldas_c
                              cod == "42" ~ 0.015,
                              cod == "43" ~ 0.01, # manta_c
                              cod == "13" ~ 0.01, # manabi
                              cod == "13" ~ 0.01,
                              T~ 0.02),
    delta = rango_deseado/(2*tasa),
    CV = round(delta/1.96, 2),
    m_per = ss4p(N = pea,
                 P = tasa,
                 DEFF = deff,
                 conf = 0.95,
                 error = "rme",
                 delta = delta),
    # nro de viviendas en la muestra
    m_viv = ceiling(m_per/promedio),
    m_upm = ceiling(m_viv/v),
    # rango del intervalo a obtener
    rango_obj = 2*delta*tasa,
    LI_obj = round(tasa - delta*tasa, 4),
    LS_obj = round(tasa + delta*tasa, 4)) %>%
  select(cod, dominio, tasa, prom_pea_viv = promedio,
         rho, deff, delta, CV,
         m_per, m_viv, m_upm,
         rango_obj, LI_obj, LS_obj) %>%
  filter(!cod %in% c("0", "1", "2")) %>%
  mutate(m_upm_mes = ifelse(as.numeric(cod) <= 24, ceiling(m_upm/12),
                            ceiling(m_upm/6)))

sum(muestra_desem$m_upm_mes)

View(muestra_desem)












n_nac<-ss4HHSp(N = resultado_desemp[resultado_desemp$dominio == "NACIONAL",which(names(resultado_desemp)=="pea")],
              #M = resultado_desemp[resultado_desemp$dominio == "NACIONAL",which(names(resultado_desemp)=="n_upm")],
              #r = resultado_desemp[resultado_desemp$dominio == "NACIONAL",which(names(resultado_desemp)=="r")],
                 b = resultado_desemp[resultado_desemp$dominio == "NACIONAL",which(names(resultado_desemp)=="b")],
                 rho = resultado_desemp[resultado_desemp$dominio == "NACIONAL",which(names(resultado_desemp)=="rho")],
                 P = resultado_desemp[resultado_desemp$dominio == "NACIONAL",which(names(resultado_desemp)=="tasa")],
                 delta = resultado_desemp[resultado_desemp$dominio == "NACIONAL",which(names(resultado_desemp)=="delta")],
                 conf = 0.95,
                 m = c(1:20))














# Tamaño de muestra de personas

resultado_desemp<- resultado_desemp %>%
                  mutate(margen_error=0.15,
                         conf = 0.95,
                         tnr= 0.20,
                         tasa=tasa/100,
                         numerador= (tasa*(1-tasa)*tasa_deff),
                         denominador= (1-tnr)*((((margen_error)^2)*(tasa^2))/(conf^2))+(((tasa*(1-tasa)*tasa_deff))/N_d),
                         n_d=numerador/denominador)
































tabla<-ss4HHSp(N = resultado_desemp[resultado_desemp$dominio == "NACIONAL",which(names(resultado_desemp)=="N_d")],
        M = n_distinct(marco$id_upm),
        r = resultado_desemp[resultado_desemp$dominio == "NACIONAL",which(names(resultado_desemp)=="r")],
        b = 3.2,
        rho = 0.06,
        P = resultado_desemp[resultado_desemp$dominio == "NACIONAL",which(names(resultado_desemp)=="tasa")],
        delta = 0.15,
        conf = 0.95,
        m = c(1:20))

