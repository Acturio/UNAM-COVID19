library(foreign)
library(dplyr) 
library(tidyr) 
library(naniar)
library(sampling) 
library(stringr)
library(tidyverse)
library(stratification)
library(classInt)
library(leaflet)
library(rgdal)
library(rgeos)
library(xlsx)

datos<- read_csv("conjunto_de_datos_ageb_urbana_09_cpv2020.csv",
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
         #ESTRATO_ESTUDIO = P15SEC_CO/P_15YMAS
         ) 

marco <- datos_mza %>%
  mutate(ESTRATO_ID_PEA =  strata.cumrootf(
      ESTRATO_PEA, CV = 0.05, Ls = 3,  nclass= 30)$stratumID, 
    
    ESTRATO_ID_GRAD_PROM_ESC = strata.cumrootf(
      ESTRATO_GRAD_PROM_ESC, CV = 0.05, Ls = 2, nclass= 20)$stratumID,
    
    ESTRATO_ID_HACINAMIENTO = strata.cumrootf(
     ESTRATO_HACINAMIENTO, CV = 0.05, Ls = 2, nclass = 20)$stratumID,
    
    #ESTRATO_ID_ESTUDIO =  strata.cumrootf(
      #ESTRATO_ESTUDIO, CV = 0.05, Ls = 2, nclass = 20)$stratumID,
    
    ESTRATO_1 = str_c(
    MUN, ESTRATO_ID_PEA, 
    ESTRATO_ID_HACINAMIENTO, sep = "-") ,
    
    ESTRATO_2 = str_c(
      MUN, ESTRATO_ID_PEA, sep = "-") ,
    
    ESTRATO_3 = str_c(
      MUN, ESTRATO_ID_GRAD_PROM_ESC, sep = "-") ,
    
    ESTRATO_4 = str_c(
      MUN,ESTRATO_ID_HACINAMIENTO, sep = "-") ,
    
    #CVE_ESTRATO_1 = as.numeric(as.factor(ESTRATO_1)),
    
    CVE_ESTRATO_2 = as.numeric(as.factor(ESTRATO_2)),
    
    CVE_ESTRATO_3 = as.numeric(as.factor(ESTRATO_3)),
    
    CVE_ESTRATO_4 = as.numeric(as.factor(ESTRATO_4))
    )
    

marco %>% group_by(CVE_ESTRATO_3) %>% tally() %>% as.data.frame() %>% 
  mutate(prop = n/sum(n), 
         m = prop * 2975/5)


marco %>% group_by(CVE_ESTRATO_1) %>% tally() %>% as.data.frame() %>% 
  mutate(prop = n/sum(n), 
         m = prop * 2975/5)

marco %>% group_by(CVE_ESTRATO_2) %>% tally() %>% as.data.frame() %>% 
  mutate(prop = n/sum(n), 
         m = round(prop * 2975/5, 0))


marco %>% group_by(ESTRATO_1) %>% tally() %>% as.data.frame() %>% 
  mutate(prop = n/sum(n), 
         m = round(prop * 2975/5, 0)) %>% 
  arrange(m)


distribucion_estratos <- marco %>%
  group_by(MUN, ESTRATO_1) %>%
  tally() %>%
  group_by(MUN) %>% 
  mutate(prop = n/sum(n), 
         n_muestra = round(prop *186,0)) %>% 
  ungroup() %>% 
  as.data.frame() %>% 
  arrange(ESTRATO_1) %>% 
  mutate(n_muestra_ajustada = n_muestra)

write.xlsx(
 distribucion_estratos, '../data/distribucion_muestral.xlsx', row.names = FALSE)


##PONERLO EN MAPA


distribucion_estratos%>% 
  group_by(MUN) %>% 
  summarise( muestra = sum(m))



# 186: Número de manzanas de cada municipio 
# 2976 
# Mapa






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

shp <- shp_mzn[shp_mzn@data$CVEGEO %in% marco$UPM,] 

shp_nulos_15 <- shp[!shp@data$CVEGEO %in% datos_nulos_15_sec,]

shp_mzn@data <- shp_mzn@data %>% left_join(marco, by = c('CVEGEO' = 'UPM'))

pal_1 <- colorFactor(palette = rainbow(n = 192), domain = 1:192)

pal_2 <- colorFactor(palette = rainbow(n = 48), domain = 1:48)

pal_3 <- colorFactor(palette = rainbow(n = 32), domain = 1:32)

pal_4 <- colorFactor(palette = rainbow(n = 48), domain = 1:48)

shp %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = shp_mun, fillColor = "transparent", color = "black", 
              weight = 1) %>% 
  addPolygons(
    data = shp_mzn, color = ~pal_4(CVE_ESTRATO_4), weight = 0.5, 
    fillOpacity = 0.7, label = ~CVE_ESTRATO_4)

  

marco %<>%
  arrange(CVE_ESTRATO, UPM) %>%
  group_by(CVE_ESTRATO, UPM) 
 

# Muestra
set.seed(20212602)

muestra <-  sampling::strata(marco, stratanames = NULL, 
                             size = 2976, method = "systematic",
                             pik = as.integer(marco$VIVPAR_HAB),
                             description = TRUE)

getdata(marco, muestra)

