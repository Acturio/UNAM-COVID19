library(foreign)
#library(tidyverse)
library(dplyr) #filtrar datos, usar %>%
library(tidyr) #replace_na
library(naniar) #replace_na_with_if
library(sampling) #MAS
library(stringr)


mzn_viv<- read.dbf("tablas/cpv2010_manzanas_viviendas.dbf") 
mzn_pob<-read.dbf("manzanas.dbf")

# Marco de muestreo 
#viv2
#CTRL + SHIFT + M -> %>%

marco <- mzn_viv %>% select(CVEGEO, VIV2) %>% 
  left_join(mzn_pob, by = "CVEGEO") %>% 
  mutate(ID_unit = 1:nrow(mzn_viv)) %>% 
  select(ID_unit, CVEGEO, VIV2, POB21, POB52, POB77) %>% 
  naniar::replace_with_na_if(.predicate = is.numeric, condition = ~.x < 0)

#srswor MAS sin remplazo
#srswr MAS con remplazo
#systematic proporcional al tama?o

set.seed(310308261)
muestra <- sampling::strata(marco, stratanames = NULL, size = 20, method = "srswor", description = TRUE)

#fe_mzn: factor de expansion
muestra_ponde <- marco %>% 
  filter(ID_unit %in% muestra$ID_unit) %>% 
  left_join(muestra, by = "ID_unit") %>% 
  mutate(fe_mzn = 1/Prob)

#Estimaci?n del numero de viviendas con base en una muestra de 20 manzanas
muestra_ponde %>% 
  summarise(Total = sum(fe_mzn), 
            pob_total = sum(fe_mzn*POB21),
            viviendas = sum(fe_mzn * VIV2))

# valores reales
marco %>% 
  summarise(viviendas = sum(VIV2),
            pob_total = sum(POB21, na.rm = TRUE))

#error relativo viviendas
dif = (2378763 - 2260794) / 2378763

#error relativo poblacion
dif2 = (6640095 - 6634328) / 6640095


#####################

# "systematic" probabilidades desiguales 

muestra2 <- sampling::strata(marco, stratanames = NULL, size = 20, method = "systematic", pik = marco$VIV2, description = TRUE)

#fe_mzn: factor de expansion
muestra_ponde <- marco %>% 
  filter(ID_unit %in% muestra2$ID_unit) %>% 
  left_join(muestra2, by = "ID_unit") %>% 
  mutate(fe_mzn = 1/Prob)


############################################################ 15 ENERO ###########################################################

# Variable de estratificación: por alcaldía 

marco_estratificado <- marco %>% mutate(alcaldia = str_sub(CVEGEO,3,5) ) 

diccionario_estratos <- marco_estratificado %>% 
                        select(alcaldia) %>% 
                        distinct() %>% 
                        mutate(id_estrato = row_number())


marco_estratificado <- marco_estratificado %>% 
  left_join(diccionario_estratos, by = "alcaldia") %>%
  arrange(id_estrato)

# Estratificado PPT (probabilidad proporcional al tamaño) con afijación simple (mismo tamaño de muestra)
muestra_estratificada <- sampling::strata(marco_estratificado, stratanames = "id_estrato",
                                          size = rep(20,16), method = "systematic", 
                                          pik = marco_estratificado$VIV2, description = TRUE)

# Estratificado con afijación proporcional y probabilidad 

afijacion_proporcional <- marco_estratificado %>% 
                          group_by(id_estrato) %>% 
                          summarise(viv_total = sum(VIV2), .groups = "drop") %>% 
                          mutate(prop_viv = viv_total/sum(viv_total),
                                 n_h = round(prop_viv * 2500, 0)) %>% 
                          pull(n_h)
                      
muestra_estratificada2 <- sampling::strata(marco_estratificado, stratanames = "id_estrato",
                                          size = afijacion_proporcional, method = "systematic", 
                                          pik = marco_estratificado$VIV2, description = TRUE)

muestra_estratificada_prob_ppt <- marco_estratificado %>% 
  filter(ID_unit %in% muestra_estratificada2$ID_unit) %>% 
  left_join(muestra_estratificada2, by = "ID_unit") %>% 
  mutate(fe_mzn = 1/Prob)