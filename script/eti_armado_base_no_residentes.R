

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

##Se levanta la serie histórica 


eti_nr_2014_2022 <- read_sav("entradas/eti_2014_2022.zip") %>% 
  zap_label() %>% 
  zap_labels()

#Filtramos la variables de interes

eti_nr_2014_2022 <-eti_nr_2014_2022 %>% 
  select(P3_2,
         P3_3,
         P5,
         P13,
         P17_1,
         P15_1,
         P19_1,
         P39_1,
         P39_4,
         P39_7,
         P39_9,
         P39_10,
         P39_111,
         P39_13,
         P39_14,
         P39_15,
         P39_16,
         P39_17,
         P39_18,
         P39_19,
         P39_20,
         PASAJE,
         ALOJAMIENTO,
         GASTRONOMIA,
         TRANSPORTE,
         COMPRAS,
         OTROS,
         EXCURSIONES,
         CABOTAJE,
         PASO,
         GTOTAL,
         GTOTALP,
         GGDPN,
         gastoest2,
         p21_ta2,
         p21_ta_publ,
         p17nuev,
         trimnue,
         paq,
         p29_1r,
         p29_2r,
         p29_3r,
         p29_5r,
         p29_7r,
         p29_8r,
         p29_9r,
         p29_11r,
         p39_1r,
         p39_4r,
         p39_7r,
         p39_9r,
         p39_10r,
         p39_111r,
         p39_13r,
         p39_14r,
         p39_15r,
         p39_16r,
         p39_17r,
         p39_18r,
         p39_19r,
         p39_20r,
         p41a_1r,
         p41a_2r,
         p41a_3r,
         p41a_4r,
         p41a_5r,
         p41a_6r,
         p41a_71r,
         p41a_8r,
         p41a_9r,
         p41b_1r,
         p41b_2r,
         p41b_3r,
         p41b_4r,
         p41b_5r,
         p41b_6r,
         p41b_71r,
         p41b_8r,
         p42_1,
         viaj,
         vis,
         estadia,
         totaln,
         paso_agrup,
         origen_final,
         viajeros,
         WPF,
         P18_1,
         P40_1,
         P40_2,
         P40_3,
         P40_4,
         P40_5,
         P40_6,
         P40_7,
         P40_8,P21_1_1A,
         P21_1_1B,
         P21_1_2A,
         P21_1_2B,
         P21_1_3,
         P21_1_4,
         P21_1_5,
         P21_1_6,
         P21_1_7,
         P21_1_8,
         P21_1_9,
         P21_2_1A,
         P21_2_1B,
         P21_2_2A,
         P21_2_2B,
         P21_2_3,
         P21_2_4,
         P21_2_5,
         P21_2_6,
         P21_2_7,
         P21_2_8,
         P21_2_9,
         P21_3_1A,
         P21_3_1B,
         P21_3_2A,
         P21_3_2B,
         P21_3_3,
         P21_3_4,
         P21_3_5,
         P21_3_6,
         P21_3_7,
         P21_3_8,
         P21_3_9,
         P21_4_1A,
         P21_4_1B,
         P21_4_2A,
         P21_4_2B,
         P21_4_3,
         P21_4_4,
         P21_4_5,
         P21_4_6,
         P21_4_7,
         P21_4_8,
         P21_4_9,
         P21_5_1A,
         P21_5_1B,
         P21_5_2A,
         P21_5_2B,
         P21_5_3,
         P21_5_4,
         P21_5_5,
         P21_5_6,
         P21_5_7,
         P21_5_8,
         P21_5_9,
         P21_6_1A,
         P21_6_1B,
         P21_6_2A,
         P21_6_2B,
         P21_6_3,
         P21_6_4,
         P21_6_5,
         P21_6_6,
         P21_6_7,
         P21_6_8,
         P21_6_9,
         P21_7_1A,
         P21_7_1B,
         P21_7_2A,
         P21_7_2B,
         P21_7_3,
         P21_7_4,
         P21_7_5,
         P21_7_6,
         P21_7_7,
         P21_7_8,
         P21_7_9,
         P21_8_1A,
         P21_8_1B,
         P21_8_2A,
         P21_8_2B,
         P21_8_3,
         P21_8_4,
         P21_8_5,
         P21_8_6,
         P21_8_7,
         P21_8_8,
         P21_8_9,
         P21_9_1A,
         P21_9_1B,
         P21_9_2A,
         P21_9_2B,
         P21_9_3,
         P21_9_4,
         P21_9_5,
         P21_9_6,
         P21_9_7,
         P21_9_8,
         P21_9_9,
         P21_10_1A,
         P21_10_1B,
         P21_10_2A,
         P21_10_2B,
         P21_10_3,
         P21_10_4,
         P21_10_5,
         P21_10_6,
         P21_10_7,
         P21_10_8,
         P21_10_9,
         P21_11_3,
         P21_12_3,
         P21_11,
         P21_21,
         P21_131,
         P21_31,
         P21_41,
         P21_51,
         P21_61,
         P21_71,
         P21_81,
         P21_91,
         P21_101,
         P21_111,
         P21_121,
         P21_141,
         P21_151_sin_mza,
         P21_161_sin_mza,
         P21_171,
         P21_181)

#Limpiamos los nombres de las variables

eti_nr_2014_2022 <- eti_nr_2014_2022 %>% 
  clean_names

colnames(eti_nr_2014_2022)

#Se genera la variable indice de tiempo

eti_nr_2014_2022 <- eti_nr_2014_2022 %>% 
  mutate(indice_tiempo=as.Date(glue("{p3_3}-{p3_2}-01")))

#chequeo

eti_nr_2014_2022 %>% 
  group_by(indice_tiempo) %>% 
  summarise(sum(viajeros))

#Se genera la variable anio_trim


eti_nr_2014_2022 <- eti_nr_2014_2022 %>% 
  mutate(trimestre=case_when(p3_2 %in% seq(from=1,to=3)~1,
                             p3_2 %in% seq(from=4,to=6)~4,
                             p3_2 %in% seq(from=7,to=9)~7,
                             p3_2 %in% seq(from=10,to=12)~10),
         anio_trim=as.Date(glue("{p3_3}-{trimestre}-01"))) 

eti_nr_2014_2022$trimestre=NULL

#chequeo

eti_nr_2014_2022 %>%  
  group_by(p3_3,anio_trim,trimnue) %>% 
  summarise(sum(viajeros))

#Se genera la variable origen Ezeiza y Aeroparque

eti_nr_2014_2022 <- eti_nr_2014_2022 %>%
  mutate(orig_eya=case_when(origen_final==1~"Bolivia",
                            origen_final==2~"Brasil",
                            origen_final==3~"Chile",
                            origen_final==4~"Paraguay",
                            origen_final==5~"Uruguay",
                            origen_final==6~"EE.UU y Canadá",
                            origen_final==7~"Resto América",
                            origen_final==8~"Europa",
                            origen_final==9~"Resto del Mundo"))

#Se genera la variable origen Córdoba

eti_nr_2014_2022 <- eti_nr_2014_2022 %>%
  mutate(orig_cor=case_when(origen_final==1~"Resto de América",
                            origen_final==2~"Brasil",
                            origen_final==3~"Chile",
                            origen_final==4~"Resto de América",
                            origen_final==5~"Resto de América",
                            origen_final==6~"EE.UU, Canadá y México",
                            origen_final==7 & p15_1 !=54 ~"Resto de América",
                            origen_final==8~"Europa y resto del mundo",
                            origen_final==9~"Europa y resto del mundo",
                            origen_final==7 & p15_1 ==54 ~"EE.UU, Canadá y México"))



#Se genera la variable origen Mendoza

eti_nr_2014_2022 <- eti_nr_2014_2022 %>%
  mutate(orig_mdz=case_when(origen_final==1~"Resto de América",
                            origen_final==2~"Brasil",
                            origen_final==3~"Chile",
                            origen_final==4~"Resto de América",
                            origen_final==5~"Resto de América",
                            origen_final==6~"EE.UU, Canadá",
                            origen_final==7~"Resto de América",
                            origen_final %in% c(8,9)~"Europa y resto del mundo"))





#Se genera la variable origen Cristo

eti_nr_2014_2022 <- eti_nr_2014_2022 %>%
  mutate(orig_cristo=case_when(origen_final==3~"Chile",
                               TRUE~"Resto del mundo"))


#Se genera la variable origen Puerto



eti_nr_2014_2022 <- eti_nr_2014_2022 %>%
  mutate(orig_puerto=case_when(origen_final==5~"Uruguay",
                               TRUE~"Resto del Mundo"))



#Se genera la variable Paso

eti_nr_2014_2022 <- eti_nr_2014_2022 %>%
  mutate(paso_final=case_when(paso_agrup==1~"Ezeiza y Aeroparque",
                              paso_agrup==4~"Aep. Córdoba",
                              paso_agrup==6~"Cristo Redentor",
                              paso_agrup==10~"Puerto de Buenos Aires",
                              paso_agrup==12~"Aep. Mendoza"))

# Se genera la variable motivo de viaje

eti_nr_2014_2022 <- eti_nr_2014_2022 %>%
  mutate(alojamiento=case_when(p21_ta_publ==1~"Casa de familiares o amigos",
                               p21_ta_publ==2~"Hotel de 1, 2 o 3 estrellas",
                               p21_ta_publ==3~"Hotel de 4 y 5 estrellas",
                               p21_ta_publ==4~"Otros"))

# Se genera la variable alojamiento


eti_nr_2014_2022 <- eti_nr_2014_2022 %>%
  mutate(motivo_viaje=case_when(p17nuev==1~"Vacaciones, ocio o recreación",
                                p17nuev==2~"Visita de familiares o amigos",
                                p17nuev==3~"Negocios, congreso o conferencia",
                                p17nuev %in% c(4,5)~"Otros"))


#Se salva la base como rds

saveRDS(eti_nr_2014_2022,"salidas/eti_nr_2014_2022.rds")
