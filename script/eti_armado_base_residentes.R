

#-------------------activamos paquetes y librerias------------#

library(readxl)
library(tidyverse)
library(magrittr)
library(writexl)
library(dplyr)
library(haven)
library(janitor)
library(data.table)
library(lubridate)
library(glue)
# 
# ##Para actualizar la base histórica:
# 
# eti_a_2014_2022 <- read_sav("entradas/eti_a_2014_2022.sav") %>%
#   zap_label() %>%
#   zap_labels()
# 
# saveRDS(eti_a_2014_2022,"entradas/eti_a_2014_2022.rds")
# 

#Se levanta la base histórica:

eti_a_2014_2022 <- readRDS("entradas/eti_a_2014_2022.rds")


#Se levanta la base del añp:


eti_a_2023 <- read_sav("entradas/ETI A 2023.sav") %>% 
  zap_label() %>% 
  zap_labels() %>% 
  mutate(viajeros=1) %>% 
  mutate(P16=as.character(P16))

# Se unifican las dos bases

eti_a_2014_2023 <- bind_rows(eti_a_2014_2022,eti_a_2023)

rm(eti_a_2014_2022,eti_a_2023)


eti_a_2014_2023 <- eti_a_2014_2023 %>% 
  select(IDCUESTIONARIO,
         SUBTIPO,
         P3_2,
         P3_3,
         P5,
         P17_1,
         P15_1,
         P15_2,
         P16,
         P18_1,
         P19_1,
         P19_2,
         P22B,
         P21_DMP,
         P21_TA,
         PAQUETE,
         WPF,
         Pasaje,
         Alojamiento,
         Gastronomia,
         Transporte,
         Compras,
         Otros,
         gtotal,
         paque,
         gtotalp,
         ggdpn,
         trim,
         noch1,
         noch2,
         noch3,
         noch4,
         noch5,
         noch6,
         noch7,
         noch8,
         noch9,
         noch10,
         noch11,
         noch12,
         noch13,
         noch14,
         noch15,
         noch16,
         noch17,
         noch18,
         noch19,
         noch20,
         noch21,
         noch22,
         noch23,
         noch24,
         totaln,
         p17nuev,
         p5nuev,
         trimnue,
         destino,
         destino_2,
         destino_cv,
         estadia,
         paq,
         paso_agrup,
         viaj,
         vis,
         p_1,
         p_2,
         p_3,
         p_4,
         p_5,
         p_6,
         p_7,
         p_8,
         p_9,
         p_10,
         p_11,
         p_12,
         p_13,
         p21_ta2,
         p21_ta_publ,
         viajeros,
         gastoest2) %>% 
  clean_names

#Se genera la variable indice de tiempo

eti_a_2014_2023 <- eti_a_2014_2023 %>% 
  mutate(indice_tiempo=as.Date(glue("{p3_3}-{p3_2}-01")))

#Se genera la variable anio_trim


eti_a_2014_2023 <- eti_a_2014_2023 %>% 
  mutate(trimestre=case_when(p3_2 %in% seq(from=1,to=3)~1,
                             p3_2 %in% seq(from=4,to=6)~4,
                             p3_2 %in% seq(from=7,to=9)~7,
                             p3_2 %in% seq(from=10,to=12)~10),
         anio_trim=as.Date(glue("{p3_3}-{trimestre}-01"))) 

eti_a_2014_2023$trimestre=NULL

#Se genera la variable destino Ezeiza y Aeroparque

eti_a_2014_2023 <- eti_a_2014_2023 %>%
  mutate(dest_eya=case_when(destino_cv==1~"Bolivia",
                            destino_cv==2~"Brasil",
                            destino_cv==3~"Uruguay",
                            destino_cv==4~"Paraguay",
                            destino_cv==5~"Chile",
                            destino_cv==6~"EE.UU y Canadá",
                            destino_cv==7~"Resto América",
                            destino_cv==8~"Europa",
                            destino_cv==9~"Resto del Mundo"),
         dest_cor=case_when(destino_cv==2~"Brasil",
                            destino_cv==5~"Chile",
                            destino_cv==6~"EE.UU y Canadá",
                            destino_cv==1 & p21_dmp!=6~"Resto América",
                            destino_cv==3 & p21_dmp!=6~"Resto América",
                            destino_cv==4 & p21_dmp!=6~"Resto América",
                            destino_cv==7 & p21_dmp!=6~"Resto América",
                            destino_cv==8~"Europa y resto del mundo",
                            destino_cv==9~"Europa y resto del mundo",
                            p21_dmp==6~"Caribe"),
         dest_mdza=case_when(destino_cv==2~"Brasil",
                             destino_cv==5~"Chile",
                             destino_cv==6~"EE.UU y Canadá",
                             destino_cv==1 & (p21_dmp!=6 & p21_dmp!=24)~"Resto América",
                             destino_cv==3 & (p21_dmp!=6 & p21_dmp!=24)~"Resto América",
                             destino_cv==4 & (p21_dmp!=6 & p21_dmp!=24)~"Resto América",
                             destino_cv==7 & (p21_dmp!=6 & p21_dmp!=24)~"Resto América",
                             destino_cv==8~"Europa y resto del mundo",
                             destino_cv==9~"Europa y resto del mundo",
                             p21_dmp==6 | p21_dmp==24~"México y Caribe"),
         dest_puerto=case_when(p21_dmp==5~"Uruguay",
                               TRUE~"Resto del mundo"),
         dest_cristo=case_when(p21_dmp==3~"Chile",
                               TRUE~"Resto del mundo"),
         paso_final=case_when(paso_agrup==1~""))

#Se genera la variable Paso

eti_a_2014_2023 <- eti_a_2014_2023 %>%
  mutate(paso_final=case_when(paso_agrup==1~"Ezeiza y Aeroparque",
                              paso_agrup==4~"Aep. Córdoba",
                              paso_agrup==6~"Cristo Redentor",
                              paso_agrup==10~"Puerto de Buenos Aires",
                              paso_agrup==12~"Aep. Mendoza"))


#Se salva la base como rds

saveRDS(eti_a_2014_2023,"salidas/eti_a_2014_2023.rds")

