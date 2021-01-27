library(dplyr)
library(leaflet)
library(rgdal)
library(rgeos)

piloto_srswor <- readRDS("piloto_srswor.rds")

leaflet() %>% 
  addTiles() %>% 
  setView(lng = -99.4, lat = 19.14, zoom = 10)

shp_mzn <- readOGR("09m.shp") %>% 
  spTransform(CRS("+proj=longlat +datum=WGS84"))

temp <- shp_mzn@polygons[[1]]@Polygons[[1]]@coords %>% 
  as.data.frame() %>% 
  rename(lng = V1, lat = V2)

shp_piloto <- shp_mzn[shp_mzn@data$CVEGEO %in% piloto_srswor$UPM,] 

centroides_piloto <- gCentroid(shp_piloto, byid = T) %>% 
  as.data.frame()

shp_piloto %>% 
  leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = shp_mun, fillColor = "transparent", color = "black", 
              weight = 1) %>%  ###### falta
  addPolygons() %>% 
  addMarkers(data = centroides_piloto, lng = ~x, lat = ~y)



