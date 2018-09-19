  #------------------------------------------------------------------------------#
  
  #	  LVTP - Corridor cadastre buffer 
  
  #------------------------------------------------------------------------------#

  
  #------------------------------------------------------------------------------#
  #### LOAD AND PROCESS DATA ####
  
  # Corridor
  cor <- readOGR(LVTP_shapeFiles, "Ngoma_Nyanza")
  
  # Districts
  districts <- readOGR(file.path(RFR_shapeFiles,"Districts"), "District_boundaries")
  


  cor_dist_lis_expanded <- 
    c("NYANZA",
      "BUGESERA",
      "NGOMA",
      "HUYE",
      "GISAGARA",
      "RUHANGO",
      "KAMONYI",
      "KAYONZA",
      "KIREHE",
      "RWAMAGANA") 
  
  
  #cor_dist_lis_expanded <- tolower(cor_dist_lis_expanded)
  
  
  #-----------------#
  #### Reproject ####
  
  
  # Define projection
  RwaProj <-  CRS("+proj=aeqd +lat_0=-1.999036 +lon_0=29.95608") 
  RwaProj_unp <-  CRS("+init=epsg:4326")  # unprojected
  
  # Reproject
  districts <- spTransform(districts, RwaProj)
  cor	<- spTransform(cor, RwaProj)

  
  #------------------------------------------------------------------------------#
  #### BUFFER ####
  
  cor.buf05 <- gBuffer(cor, width = 5000)
  cor.buf10 <- gBuffer(cor, width = 10000)
  
  
  #------------------------------------------------------------------------------#
  #### Individual district buffer ####
  
  NYANZA_corBuf <- intersect(cor.buf10, districts[districts@data$NOMDISTR == "NYANZA",])
  BUGESERA_corBuf <-   intersect(cor.buf10, districts[districts@data$NOMDISTR =="BUGESERA",])
  NGOMA_corBuf <- intersect(cor.buf10, districts[districts@data$NOMDISTR =="NGOMA",])
  HUYE_corBuf <- intersect(cor.buf10, districts[districts@data$NOMDISTR =="HUYE",])
  GISAGARA_corBuf <- intersect(cor.buf10, districts[districts@data$NOMDISTR =="GISAGARA",])
  RUHANGO_corBuf <- intersect(cor.buf10, districts[districts@data$NOMDISTR =="RUHANGO",])
  KAMONYI_corBuf <- intersect(cor.buf10, districts[districts@data$NOMDISTR =="KAMONYI",])
  KAYONZA_corBuf <- intersect(cor.buf10, districts[districts@data$NOMDISTR =="KAYONZA",])
  KIREHE_corBuf <- intersect(cor.buf10, districts[districts@data$NOMDISTR =="KIREHE",])
  RWAMAGANA_corBuf <- intersect(cor.buf10, districts[districts@data$NOMDISTR == "RWAMAGANA"]) 
  
  
  bufNameVec <- paste0(cor_dist_lis_expanded, "_corBuf")
  
  
  #------------------------------------------------------------------------------#
  #### Cross with cadaster ####
  
  start_time <- Sys.time()

    Nyanza_cad <- readOGR(file.path(CAD_temp_path, "nyanza"), "nyanza")
    
    # Project and reproject to match other shps
    proj4string(Nyanza_cad) <- CRS("+proj=tmerc +lat_0=0 +lon_0=30 +k=0.9999 +x_0=500000 +y_0=5000000 +ellps=GRS80 +units=m +no_defs")
    caDist	<- spTransform(Nyanza_cad	, RwaProj)
    
    #### Intersect cadaster with feed buffer
    cadInt <- NULL
    #cadInt <- intersect(get(paste0(dist, "_feedBuf" )), caDist)
    
    Nya_cadCor	  <- subset(Nyanza_cad, 	
                           gIntersects(Nyanza_cad, 	
                                       gUnaryUnion(NYANZA_corBuf), 
                                       byid = T) %>% as.logical)
    
    #### Drop original shapefile
    rm(Nyanza_cad)

    finish_time <- Sys.time()
    
    finish_time - start_time
    
    
    
  #------------------------------------------------------------------------------#
  #### DRAFT
    
  Nya_cadCor.df <- tidy(Nya_cadCor)
  
  ggplot()+
    geom_path(data = cor.df,
               aes(y= lat, 
                   x = long,
                   group = group,
                   col = "Corridor"),
               size = 1) +
    geom_polygon(data = Nya_cadCor.df,
                 aes(y= lat, 
                     x = long,
                     group = group))
    

  
  