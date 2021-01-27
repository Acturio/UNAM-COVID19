library(dplyr)
library(readr)
library(readxl)
library(magrittr)
library(purrr)
library(lintr)
library(sampling)
library(stringr)

datos <- read_csv("conjunto_de_datos_ageb_urbana_09_cpv2020.csv")

# Limpieza: 

datos %<>% 
  select(ENTIDAD, NOM_ENT, MUN,NOM_MUN, LOC, NOM_LOC, AGEB, MZA,
         POBTOT, P_18YMAS, P_18YMAS_F, P_18YMAS_M, VIVPAR_HAB) %>% 
  filter(ENTIDAD != "00",
         MUN != "000",
         LOC != "0000",
         AGEB != "0000",
         MZA != "000",
         VIVPAR_HAB > 5,
         VIVPAR_HAB != "N/D") %>% 
  mutate(UPM = str_c(ENTIDAD, MUN, LOC, AGEB, MZA)) 

marco <- datos %>% 
  mutate(ID_unit = 1:nrow(datos))

# MAS sin remplazo 

set.seed(26012021)

muestra <- sampling::strata(marco, stratanames = NULL,
                            size = 6, method = "srswor", description = TRUE)

muestra_ponde <- marco %>% 
  filter(ID_unit %in% muestra$ID_unit) %>% 
  left_join(muestra, by = "ID_unit") %>% 
  mutate(fe_mzn = 1/Prob)

saveRDS(muestra_ponde, file = "piloto_srswor.rds")

# Systematic

set.seed(26012021)

muestra_2 <- sampling::strata(marco, stratanames = NULL, 
                              size = 6, method = "systematic",
                              pik = as.integer(marco$VIVPAR_HAB),
                              description = TRUE)

#fe_mzn: factor de expansion
muestra_ponde_2 <- marco %>% 
  filter(ID_unit %in% muestra_2$ID_unit) %>% 
  left_join(muestra_2, by = "ID_unit") %>% 
  mutate(fe_mzn = 1/Prob)

saveRDS(muestra_ponde_2, file = "piloto_systematic.rds")
