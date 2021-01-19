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
                                 n_h = round(prop_viv * 2500, 0))
                      
muestra_estratificada2 <- sampling::strata(marco_estratificado, stratanames = "id_estrato",
                                          size = afijacion_proporcional %>% pull(n_h),
                                          method = "systematic", 
                                          pik = marco_estratificado$VIV2,
                                          description = TRUE)

muestra_estratificada_prob_ppt <- marco_estratificado %>% 
  filter(ID_unit %in% muestra_estratificada2$ID_unit) %>% 
  left_join(muestra_estratificada2, by = "ID_unit") %>% 
  mutate(fe_mzn = 1/Prob)


########################################### LUNES 18 ENERO #############################################

head(marco_estratificado)

marco_estratificado_pik<- marco_estratificado %>% 
  left_join(select(afijacion_proporcional, id_estrato, n_h), by = "id_estrato") %>% 
  mutate(pik = inclusionprobastrata(id_estrato,afijacion_proporcional %>% pull(n_h))) %>% 
  group_by(id_estrato) %>% 
  mutate(pik_2 = VIV2/sum(VIV2)*n_h)

#Corroborar pik_2
marco_estratificado_pik %>% 
  group_by(id_estrato) %>% 
  summarise(viv2 = sum(VIV2))

# group_split 
marco_estratificado_pik %>% 
  group_split(id_estrato)

#Muestra con base a la afijación proporcional 
muestra_estratificada_pik <- sampling::strata(marco_estratificado_pik,
                                              stratanames = "id_estrato",
                                              size = afijacion_proporcional %>% pull(n_h),
                                              method = "systematic", 
                                              pik = marco_estratificado_pik$pik, description = TRUE)


################################# Muestreo poli etápico ###########################################

############
## Example 1
############
# Two-stage cluster sampling
# Uses the 'swissmunicipalities' data 
data(swissmunicipalities)
b=swissmunicipalities
b=b[order(b$REG,b$CT),]
attach(b)
# the variable 'REG' (region) has 7 categories;
# it is used as clustering variable in the first-stage sample
# the variable 'CT' (canton) has 26 categories; 
# it is used as clustering variable in the second-stage sample
# 4 clusters (regions) are selected in the first-stage 
# 1 canton is selected in the second-stage from each sampled region 
# the method is simple random sampling without replacement in each stage
# (equal probability, without replacement)
m=mstage(b,stage=list("cluster","cluster"), varnames=list("REG","CT"),
         size=list(4,c(1,1,1,1)), method=list("srswor","srswor"), description = TRUE)
# the first stage is m[[1]], the second stage is m[[2]]
#the selected regions
unique(m[[1]]$REG)
#the selected cantons
unique(m[[2]]$CT)
# extracts the observed data
x=getdata(b,m)[[2]]
# check the output
table(x$REG,x$CT)


############
## Example 3
############
# Stratified one-stage cluster sampling
# The same data as in Example 2
# the variable 'state' is used as stratification variable 
# 165 units are in the first stratum and 70 in the second one
# the variable 'region' is used as clustering variable
# 1 cluster (region) is drawn in each state using "srswor" 
data=rbind(matrix(rep('n',165),165,1,byrow=TRUE),matrix(rep('s',70),70,1,byrow=TRUE))
data=cbind.data.frame(data,c(rep('A',115),rep('D',10),rep('E',40),rep('B',30),rep('C',40)),
                      100*runif(235))
names(data)=c("state","region","income")
data=data[order(data$state,data$region),]

m=mstage(data, stage=list("stratified","cluster"), varnames=list("state","region"), 
         size=list(c(165,70),c(1,1)),method=list("","srswor"), description = TRUE) 
# check the first stage
table(m[[1]]$state)
# check the second stage
table(m[[2]]$region)
# extracts the observed data
xx=getdata(data,m)[[2]]
# check the result
table(xx$state,xx$region)
