library(foreign)
library(dplyr) 
library(tidyr) 
library(naniar)
library(sampling) 
library(stringr)
library(tidyverse)
library(magrittr)
library(stratification)
library(classInt)
library(leaflet)
library(rgdal)
library(rgeos)
library(xlsx)
library(readr)
library(readxl)
library(feather)

datos <- read_csv("conjunto_de_datos_ageb_urbana_09_cpv2020.csv",
                   na = c("*", 'N/D', "N/A"))

datos_mza <- datos %>% 
  select(ENTIDAD, NOM_ENT, MUN,NOM_MUN, LOC, NOM_LOC, AGEB, MZA,
         POBTOT, P_18YMAS, P_18YMAS_F, P_18YMAS_M, VIVPAR_HAB, PEA, GRAPROES,
         PRO_OCUP_C) %>% 
  filter(ENTIDAD != "00",
         MUN != "000",
         LOC != "0000",
         AGEB != "0000",
         MZA != "000",
         VIVPAR_HAB > 5) %>% 
  mutate(UPM = str_c(ENTIDAD, MUN, LOC, AGEB, MZA),
         ESTRATO_PEA = PEA/P_18YMAS,
         ESTRATO_GRAD_PROM_ESC = GRAPROES,
         ESTRATO_HACINAMIENTO = PRO_OCUP_C
         ) 

marco <- datos_mza %>%
  mutate(ESTRATO_ID_PEA =  strata.cumrootf(
      ESTRATO_PEA, CV = 0.05, Ls = 3,  nclass= 30)$stratumID, 
    
    ESTRATO_ID_GRAD_PROM_ESC = strata.cumrootf(
      ESTRATO_GRAD_PROM_ESC, CV = 0.05, Ls = 2, nclass= 20)$stratumID,
    
    ESTRATO_ID_HACINAMIENTO = strata.cumrootf(
     ESTRATO_HACINAMIENTO, CV = 0.05, Ls = 2, nclass = 20)$stratumID,
  
    ESTRATO_ID = str_c(
    MUN, ESTRATO_ID_PEA, 
    ESTRATO_ID_HACINAMIENTO, sep = "-"),
    
    CVE_ESTRATO = as.numeric(as.factor(ESTRATO_ID))
    )
    
distribucion_estratos <- marco %>%
  group_by(MUN, ESTRATO_ID) %>%
  tally() %>%
  group_by(MUN) %>% 
  mutate(prop = n/sum(n), 
         n_muestra = round(prop *186,0)) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  arrange(ESTRATO_ID) %>% 
  mutate(n_muestra_ajustada = n_muestra)

write.xlsx(
 distribucion_estratos, '../data/distribucion_muestral.xlsx', row.names = FALSE)

distribucion_muestral_ajustada <- read_xlsx("distribucion_muestral.xlsx")


marco %<>% arrange(CVE_ESTRATO)

# Muestra
set.seed(20210302)

muestra <- sampling::strata(
marco, stratanames = 'CVE_ESTRATO',
size = distribucion_muestral_ajustada$n_muestra_ajustada,
method = "systematic", pik = as.integer(marco$VIVPAR_HAB), description = TRUE)

marco_muestral <- getdata(marco, muestra)

# Comprobación n_muestra_ajustada

marco_muestral %>%
  group_by(ESTRATO_ID) %>% 
  tally() %>%
  as.data.frame() %>% 
  left_join(distribucion_muestral_ajustada, by =c('ESTRATO_ID'= 'ESTRATO_1')) %>% 
  mutate(resta = n.x - n_muestra_ajustada)

# Mapa marco muestral

leaflet() %>% 
  addTiles() %>% 
  setView(lng = -99.4, lat = 19.14, zoom = 10)

shp_mzn <- readOGR("manzanas.shp") %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84"))

shp_mun <- readOGR("municipal.shp") %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84"))

temp <- shp_mzn@polygons[[1]]@Polygons[[1]]@coords %>% 
  as.data.frame() %>% 
  rename(lng = V1, lat = V2)

shp <- shp_mzn[shp_mzn@data$CVEGEO %in% marco_muestral$UPM,] 

shp_mzn@data <- shp_mzn@data %>% left_join(marco_muestral, by = c('CVEGEO' = 'UPM'))

pal <- colorFactor(palette = rainbow(n = 96), domain = 1:96)

shp %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = shp_mun, fillColor = "transparent", color = "black", 
              weight = 1) %>% 
  addPolygons(
    data = shp_mzn, color = ~pal(CVE_ESTRATO), weight = 0.5, 
    fillOpacity = 0.7, label = ~ESTRATO_ID)


marco_muestral %>% 
  write_feather('sample/MUESTRA-COVID19.feather')

marco_muestral %>% 
  as.data.frame() %>% 
  write.xlsx('sample/MUESTRA-COVID19.xlsx',
             row.names = FALSE
  )
