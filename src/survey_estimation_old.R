{
  library(foreign)
  library(readxl)
  library(survey)
  library(dplyr)
  library(tidyr)
  library(xlsx)
  library(caret)
} # Librerías

{
  dataset <- read.spss("BASE_CONACYT_260118.sav", to.data.frame=TRUE) # Lectura de datos de spss
  General <- "Nacional" # Nombre de estimación global (Puede ser nacional, cdmx, etc. Depende de la representatividad del estudio)
  
  Lista <- read_xlsx("Lista de Preguntas.xlsx", sheet = "Lista Preguntas")$Pregunta %>% as.vector()
  Lista_Preg <- read_xlsx("Lista de Preguntas.xlsx", sheet = "Lista Preguntas")$Nombre %>% as.vector()
  DB_Mult <- read_xlsx("Lista de Preguntas.xlsx", sheet = "Múltiple") %>% as.data.frame()
  Lista_Cont <- read_xlsx("Lista de Preguntas.xlsx", sheet = "Continuas")$VARIABLE %>% as.vector()
  Dominios <- read_xlsx("Lista de Preguntas.xlsx", sheet = "Dominios")$Dominios %>% as.vector()
  
  Multiples <- names(DB_Mult)
  Ponderador <- dataset$Pondi1
  save = ""
} # Archivos y Listas

{
  disenio_CAT_CONT<-svydesign(id =~ CV_ESC + ID_DIAO, strata = ~ ESTRATO, weights =~ Pondi1,
                              data = dataset, check.strata = T, pps = "brewer")
  disenio_MULT <- svydesign(id =~ CV_ESC + ID_DIAO, strata = ~ ESTRATO, weights =~ Pondi1,
                            data = dataset, check.strata = T, pps = "brewer")
} # Diseño muestral Variables Categóricas

{
  k=1
  k1=1 # indicador de renglones en hoja 1
  k2=1
  k3=1
  np=1
  wb = createWorkbook()
  sheet1 = createSheet(wb, "General")
  sheet2 = createSheet(wb, "Dominios")
  sheet3 = createSheet(wb, "Dispersión Dominios.")
} # Inicialización de índices y hojas


for(p in Lista){
  
  dataset$Pondi1 <- Ponderador
  
  if(p %in% Multiples){
    
    ps <- DB_Mult[,p][!is.na(DB_Mult[,p])]
    
    df <- dataset[,ps]
    categorias <- df %>% unlist() %>% levels()
    Num_Cat <- length(categorias) # Número de categorías
    df$ID <- row.names(df)
    
    {
      df_dicotomizada <- caret::dummyVars(ID ~ ., data = df)
      df_dicotomizada <- predict(df_dicotomizada, newdata = df) 
      df_dicotomizada[is.na(df_dicotomizada)]<- 0
      
      df_dico_2 <- as.data.frame(matrix(NA,nrow(df),ncol=Num_Cat))
      names(df_dico_2) <- categorias
      
      dum <- NULL
      for(j in 1:Num_Cat){
        dum <- df_dicotomizada[,j]
        for (i in 1:(length(ps)-1)) {
          dum <- dum + df_dicotomizada[,j+i*Num_Cat]  
        }
        df_dico_2[,j] <- dum
      } 
      df_dico_2[df_dico_2 > 1] <- 1
      
    } # Dicotomización
    
    dataset$Pondi1[is.na(df[,1])] <- 0
    
    disenio_M<-svydesign(id =~ CV_ESC + ID_DIAO, strata = ~ ESTRATO, weights =~ Pondi1,
                         data = dataset, check.strata = T, pps = "brewer")
    
    estadísticas_P <- list()
    estadísticas_S <- list()
    estadísticas_G <- list()
    
    for (categ in categorias) {
      
      {
        total <- svymean(~ df_dico_2[,categ], disenio_M, deff=T, na.rm=T)
        IC <- as.data.frame(confint(total))
        nacional <- as.data.frame(total)
        IC[IC < 0] <- 0
        IC[IC > 1] <- 1
        
        Nacional <- cbind.data.frame(nacional[,1],IC$`2.5 %`,IC$`97.5 %`,nacional[,-1])
        row.names(Nacional) <- General
        names(Nacional) <- c("Media","lim. inf.", "lim. sup.","Err. Est.", "DEFF")
        Nacional$Varianza <- (Nacional$`Err. Est.`^2) 
        Nacional$'Coef. Var.' <- Nacional$`Err. Est.`/Nacional$Media
      } # Tabla con resultados generales
      
      for(dom in Dominios){
        
        {
          estima <- svyby(~ df_dico_2[,categ], ~ dataset[,dom], disenio_M, svymean,
                          keep.names = T, keep.var = T, deff = T, na.rm = T)
          
          Intervalo<-as.data.frame(confint(estima))  
          Intervalo[Intervalo < 0] <- 0
          Intervalo[Intervalo > 1] <- 1
          estima$'lim. inf.' <- Intervalo$`2.5 %`
          estima$'lim. sup.' <- Intervalo$`97.5 %`
          
          tabla <- estima[,c(2,5,6,3,4)]
          tabla$Varianza <- tabla$se^2
          tabla$'Coef. Var.' <- tabla$se/tabla$`df_dico_2[, categ]`
          names(tabla) <- c("Media", "lim. inf.", "lim. sup.","Err. Est.","DEFF","Varianza","Coef. Var.")
        } # Tabla con resultados desagregados por dominio de estudio
        
        Nacional <- rbind.data.frame(Nacional, tabla)
        
      } # Resultados por dominios
      
      estadísticas_P[[categ]] <- Nacional[,1:3] 
      estadísticas_S[[categ]] <- Nacional[,-c(1,2,3)]
      estadísticas_G[[categ]] <- Nacional[1,] 
      
    } # Estadísticas Generales y desagregadas por cada categorías
    
    estimaciones_P <- estadísticas_P[[1]]
    estimaciones_S <- estadísticas_S[[1]]
    estimaciones_G <- estadísticas_G[[1]]
    
    for (categ2 in 2:length(categorias)) {
      
      estimaciones_P <- cbind.data.frame(estimaciones_P,estadísticas_P[[categ2]])
      estimaciones_S <- cbind.data.frame(estimaciones_S,estadísticas_S[[categ2]])  
      estimaciones_G <- rbind.data.frame(estimaciones_G,estadísticas_G[[categ2]])
      
    }
    
    row.names(estimaciones_G) <- categorias
    
    Pregu <- Lista_Preg[np]
    
    nombres_cat_P <- c(categorias[1],NA,NA)
    nombres_cat_S <- c(categorias[1],NA,NA,NA)
    for (n in 2:length(categorias)) {
      nombres_cat_P <- c(nombres_cat_P,categorias[n],NA,NA)
      nombres_cat_S <- c(nombres_cat_S,categorias[n],NA,NA,NA)
    }
    
    {
      rows <- createRow(sheet1, rowIndex = k1)
      cell <- createCell(rows, colIndex = 1)[[1,1]]
      setCellValue(cell, Pregu)
      addDataFrame(estimaciones_G, sheet = sheet1, startColumn = 1, row.names = T, startRow = k1+1)
    } # Hoja Resultados Generals
    
    {
      rows <- createRow(sheet2, rowIndex = k2)
      cell <- createCell(rows, colIndex = 1)[[1,1]]
      setCellValue(cell, Pregu)
      addDataFrame(t(as.data.frame(nombres_cat_P)), sheet = sheet2, startColumn = 2, row.names = F, col.names = F, startRow = k2+1,characterNA = NA)
      addDataFrame(estimaciones_P, sheet = sheet2, startColumn = 1, row.names = T, startRow = k2+2)
    } # Estadísticas por dominio
    
    {
      rows <- createRow(sheet3, rowIndex = k3)
      cell <- createCell(rows, colIndex = 1)[[1,1]]
      setCellValue(cell, Pregu)
      addDataFrame(t(as.data.frame(nombres_cat_S)), sheet = sheet3, startColumn = 2, row.names = F, col.names = F, startRow = k3+1,characterNA = NA)
      addDataFrame(estimaciones_S, sheet = sheet3, startColumn = 1, row.names = T, startRow = k3+2)
    } # Dispersión por dominio
    
    {
      k=k+1
      k1=k1 + 1 + nrow(estimaciones_G) + 4
      k2=k2 + 2 + nrow(estimaciones_P) + 4
      k3=k3 + 2 + nrow(estimaciones_S) + 4
      np=np + 1
    } # Índices
    
  }  # Proceso para generar estimaciones de las preguntas Múltiples
  
  else if(p %in% Lista_Cont){
    
    # Convertir a valores numéricos las respuestas que vienen como categorías
    dataset[,p] <- as.numeric(levels(dataset[,p])[dataset[,p]])
    
    {
      total <- svymean(~ dataset[,p], disenio_CAT_CONT, deff=T, na.rm=T) # Nacional
      IC<-as.data.frame(confint(total))
      nacional<-as.data.frame(total)
      IC[IC < min(dataset[,p], na.rm = T)] <- min(dataset[,p], na.rm = T)
      IC[IC > max(dataset[,p], na.rm = T)] <- max(dataset[,p], na.rm = T)
      
      Nacional<- cbind.data.frame(nacional[,1],round(IC$`2.5 %`,2),round(IC$`97.5 %`,2),nacional[,2:ncol(nacional)])
      row.names(Nacional) <- General
      names(Nacional)<-c("Media","lim. inf.","lim. sup.","Err. Est.","DEFF")
      Nacional$Varianza <- (Nacional$'Err. Est.'^2)
      Nacional$'Coef. Var.' <- Nacional$'Err. Est.'/Nacional$Media
      Nacional$Media<- round(Nacional$Media,2)
    } # Tablas de estadísticas Nacionales
    
    for (dom in Dominios) {
      
      {
        estima<-svyby(~ dataset[,p], ~ dataset[,dom], disenio_CAT_CONT, svymean,
                      keep.names = T,keep.var=TRUE, deff=T, na.rm=T) # Por dominio
        
        Intervalo<-as.data.frame(confint(estima))
        Intervalo[Intervalo < min(dataset[,p], na.rm = T)] <- min(dataset[,p], na.rm = T) 
        Intervalo[Intervalo > max(dataset[,p], na.rm = T)] <- max(dataset[,p], na.rm = T) 
        
        estima$'lim. inf.' <- round(Intervalo$`2.5 %`,2)
        estima$'lim. sup.' <- round(Intervalo$`97.5 %`,2)
        tabla <- estima[,c(2,5,6,3,4)]
        tabla$Varianza <- tabla$se^2
        tabla$C.V. <- tabla$se/tabla$`dataset[, p]`
        names(tabla)<-c("Media","lim. inf.","lim. sup.","Err. Est.","DEFF","Varianza","Coef. Var.")
        tabla$Media <- round(tabla$Media,2)
      } # Tabla con resultados por dominio de estudio
      
      Nacional <- rbind.data.frame(Nacional, tabla)
      
    } # Estimación por dominios
    
    {
      Pregu<-Lista_Preg[np]
    } # Nombre de la pregunta
    
    {
      rows  <- createRow(sheet1, rowIndex=k1) 
      cell <- createCell(rows, colIndex=1)[[1,1]]     
      setCellValue(cell, Pregu)
      addDataFrame(Nacional[1,], sheet=sheet1, startColumn=1, row.names=T, startRow = k1+1)
    } # Hoja 1 : Resultados Nacionales
    
    {
      rows  <- createRow(sheet2, rowIndex=k2 ) 
      cell <- createCell(rows, colIndex=1)[[1,1]]     
      setCellValue(cell, Pregu)
      addDataFrame(Nacional[,1:3], sheet=sheet2, startColumn=1, row.names=T, startRow = k2+1)
    } # Hoja 2 : Resultados por Dominio (Intervalos)
    
    {
      rows  <- createRow(sheet3, rowIndex=k3 ) 
      cell <- createCell(rows, colIndex=1)[[1,1]]     
      setCellValue(cell, Pregu)
      addDataFrame(Nacional[,-c(1,2,3)], sheet=sheet3, startColumn=1, row.names=T, startRow = k3+1)
    } # Hoja 3 : Resultados por Dominio (Disepersión)
    
    {
      k=k+1
      k1=k1 + 1 + nrow(Nacional[1,]) + 4
      k2=k2 + 1 + nrow(Nacional) + 4
      k3=k3 + 1 + nrow(Nacional) + 4
      np=np + 1 # Se actualiza número de pregunta
    } # Índices
    
  }  # Proceso para generar estimaciones de las preguntas Continuas
  
  else({
    
    {
      # estimación de promedio Nacional
      total<-svymean(~ dataset[,p], disenio_CAT_CONT, deff=T,na.rm=T)
      
      # se calcula intervalo de confianza y se convierte en dataframe
      IC<-as.data.frame(confint(total)) 
      
      # Si el límite inferior es menor a cero, se acota en cero, si es mayor a 1, se acota en 1
      IC[IC < 0] <- 0 
      IC[IC > 1] <- 1
      
      # Se convierte la estimación en tabla
      nacional<-as.data.frame(total) 
      
      # Se ordenan los resultados
      Nacional<- cbind.data.frame("Media"=nacional[,1],"Lim. inf." = IC$`2.5 %`,
                                  "Lim. sup." = IC$`97.5 %`,nacional[,-1]) 
      
      # Se guardan los nombres de las categorías de la pregunta
      row.names(Nacional) <- levels(dataset[,p])
      
      # Se asignan nombres de columnas
      names(Nacional) <- c("Media","Lim. inf.", "Lim. sup.","Err. Est.", "DEFF") 
      
      # Se calcula la varianza
      Nacional$Varianza <- (Nacional$'Err. Est.'^2) 
      
      # Se calcula el coeficiente de variación
      Nacional$"Coef. Var." <- Nacional$'Err. Est.'/Nacional$Media 
      
      # Se crea tabla con resultados para segunda hoja del excel (resultados con intervalos)
      Nacional2<-data.frame()
      for(t in 1:nrow(Nacional)){
        Nacional2 <- unlist(c(Nacional2,unlist(Nacional[t,1:3])))
      }
      
      # Se crea tabla con resultados para tercera hoja del excel (resultados con medidas de precisión)
      Nacional3<-data.frame()
      for(t in 1:nrow(Nacional)){
        Nacional3 <- unlist(c(Nacional3,unlist(Nacional[t,4:7])))
      }
    } # Nacional
    
    for(dom in Dominios){
      
      {
        # estimación de media desagregada por dominio
        estima<-svyby(~ dataset[,p], ~dataset[,dom], disenio_CAT_CONT, svymean,
                      keep.names = T,keep.var=TRUE, deff=T, na.rm = T) 
        
        # se calcula intervalo de confianza y se convierte en dataframe
        Intervalo<-as.data.frame(confint(estima))
        
        # Si el límite inferior es menor a cero, se acota en cero, si es mayor a 1, se acota en 1
        Intervalo[Intervalo < 0] <- 0
        Intervalo[Intervalo > 1] <- 1
      } # Cálculo general por dominio
      
      {
        pos<-regexpr(":",row.names(Intervalo))
        pos1<-regexpr(']',row.names(Intervalo))
        Intervalo$Dominio<-substr(row.names(Intervalo),1,pos-1)
        Intervalo$Pregunta<-substr(row.names(Intervalo),pos1+1,nchar(row.names(Intervalo)))
        
        # Se convierte la tabla en formato wide (para los intervalos de confianza)
        tabla_inf <- Intervalo[,c(3,4,1)] %>% spread(Pregunta,'2.5 %')
        tabla_sup <- Intervalo[,c(3,4,2)] %>% spread(Pregunta,'97.5 %')
        
        # Se ordenan las estimaciones puntuales e intervalos en la tabla
        tabla<- tabla_inf[,1]
        for(t in 2:ncol(tabla_inf)){
          tabla <- cbind.data.frame(tabla,0.5*(tabla_inf[,t]+tabla_sup[,t]),tabla_inf[,t],tabla_sup[,t])
        }
      } # Agrupación de tabla
      
      {
        row.names(tabla) <- tabla[,1]
        tabla <- tabla[,-1]
        
        # nombres de categorías
        nombres_cat <- levels(dataset[,p])
        
        # encabezados de tabla. Se usan NA para dejar vacías algunas celdas
        nombres_cat_P <- c(nombres_cat[1],NA,NA)
        nombres_cat_S <- c(nombres_cat[1],NA,NA,NA)
        
        # Se crea vector con nombre de encabezados de toda la tabla
        for (n in 2:length(nombres_cat)) {
          nombres_cat_P <- c(nombres_cat_P,nombres_cat[n],NA,NA)
          nombres_cat_S <- c(nombres_cat_S,nombres_cat[n],NA,NA,NA)
        }
        
        names(tabla) <- names(Nacional2)
        
        # Se pegan los resultados nacionales a los desagregados por dominio
        Nacional2 <- rbind.data.frame(Nacional2,tabla)
      } # Tablas intervalos
      
      {
        # Selección de columnas del error estandar, deff, varianza, promedio y cv
        tabla_se <- estima %>% select(starts_with("se"))
        tabla_deff <- estima %>% select(starts_with("Deff"))
        tabla_var <- tabla_se^2
        tabla_media <- Nacional2[row.names(tabla_var),seq(1,ncol(Nacional2),3)]
        tabla_Coef <- tabla_se/tabla_media
        
        # Se crea tabla y se ordenan los resultados en el orden deseado
        Dominios2 <- data.frame()
        Dominios2 <- cbind.data.frame(tabla_se[,1],tabla_deff[,1],tabla_var[,1],tabla_Coef[,1])
        
        # Se concatenan las columnas de cada tabla. Una por cada categoría de la pregunta
        for (f in 2:ncol(tabla_se)) {
          Dominios2 <- cbind.data.frame(Dominios2,tabla_se[,f],tabla_deff[,f],tabla_var[,f],tabla_Coef[,f])
        }
        
        names(Dominios2) <- names(Nacional3)
        
        # Se unen las tablas con resultado global y resultados desagregados
        Nacional3 <- rbind.data.frame(Nacional3,Dominios2)
      } # Tablas dispersión
      
    } # Estimaciones desagregadas por dominios de estudio
    
    row.names(Nacional2)[1] <- General    
    row.names(Nacional3)[1] <- General
    
    {
      Pregu<-Lista_Preg[np]
    } # Nombre de pregunta
    
    {
      # Se crea espacio vacío en renglón de xlsx
      rows  <- createRow(sheet1, rowIndex=k1)
      
      # Se crea celda vacía
      cell <- createCell(rows, colIndex=1)[[1,1]]
      
      # Se asigna la pregunta a la celda
      setCellValue(cell, Pregu)
      
      # Se añade la tabla con frecuencias simples en la hoja 1
      addDataFrame(Nacional, sheet=sheet1, startColumn=1, row.names=T, startRow = k1+1)
    } # Hoja 1 (explicado)
    
    {
      rows  <- createRow(sheet2, rowIndex=k2)
      cell <- createCell(rows, colIndex=1)[[1,1]]
      setCellValue(cell, Pregu)
      addDataFrame(t(as.data.frame(nombres_cat_P)), sheet = sheet2, startColumn = 2, row.names = F, col.names = F, startRow = k2+1,characterNA = NA)
      addDataFrame(Nacional2, sheet=sheet2, startColumn=1, row.names=T, startRow = k2+2)
    } # Hoja 2
    
    {
      rows <- createRow(sheet3, rowIndex=k2)
      cell <- createCell(rows, colIndex=1)[[1,1]]
      setCellValue(cell, Pregu)
      addDataFrame(t(as.data.frame(nombres_cat_S)), sheet = sheet3, startColumn = 2, row.names = F, col.names = F, startRow = k3+1,characterNA = NA)
      addDataFrame(Nacional3, sheet=sheet3, startColumn=1, row.names=T, startRow = k3+2)
    } # Hoja 3
    
    {
      # Actualización de índice de renglón
      k=k+1
      
      # Actualización de índice de renglón para tablas de hoja 1,2 y 3. Se dejan 4 espacios entre tabla y tabla
      k1=k1 + 1 + nrow(Nacional) + 4
      k2=k2 + 1 + nrow(Nacional2) + 4
      k3=k3 + 1 + nrow(Nacional3) + 4
      
      # Se actualiza número de pregunta
      np=np + 1
    } # Índices (explicado)
    
    
  }) # Proceso para generar estimaciones de las preguntas categóricas
  
} # Fin de proceso estadístico

saveWorkbook(wb, "Estadísticas.xlsx")
