  #------------------------------------------------------------------------------#
  
  #	  LVTP - Corridor buffer
  
  #------------------------------------------------------------------------------#


  # Porpuse: Create a X km buffer around corridor and calculate descriptive stats
  # with Establishment census and land cadastre data.


  library(rgdal)
  library(rgeos)
  library(sp)
  
  #------------------------------------------------------------------------------#
  #### PATHS ####

  DROPBOX <- "C:/Users/Leonardo/Dropbox/Work/WB"
  GITHUB  <- "C:/Users/Leonardo/Documents/GitHub/Pilot_miscellaneous"

  LVTP <- file.path(DROPBOX, "Rwanda Lake Victoria Transport Corridor")
  RFR  <- file.path(DROPBOX, "Rwanda Feeder Roads")
  
  EC_data <- file.path(RFR, "data/admin/Establishment_census_data")
  RFR_shapeFiles <- file.path(RFR, "data/gis/Shapefiles")
  LVTP_shapeFiles <- file.path(LVTP, "data/corridor shapefile")
  
  #CAD_data file.path(DROPBOX,
  
  OUT <- file.path(LVTP, "Analysis")
  
  #------------------------------------------------------------------------------#
  #### LOAD AND PROCESS DATA ####
  
  # Corridor
  cor <- readOGR(LVTP_shapeFiles, "Ngoma_Nyanza")
  
  # Districts
  districts <- readOGR(file.path(RFR_shapeFiles,"Districts"), "District_boundaries")
  
  
  
  # Villages
  
  # Cadastre
  
  #-----------------#
  #### Reproject ####
  
  # Define projection
  RwaProj <-  CRS("+proj=aeqd +lat_0=-1.999036 +lon_0=29.95608") 
  
  # Reproject
  districts <- spTransform(districts, RwaProj)
  cor	<- spTransform(cor, RwaProj)
  
  #---------------#
  #### Subsets ####
  
  cor_districts_listing <- 
    c("NYANZA",
      "BUGESERA",
      "NGOMA") 
  
  cor_districts_listing_expanded <- 
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
  
  #cor_districts <- districts[districts@data$NOMDISTR %in% cor_districts_listing,]
  cor_districts <- districts[districts@data$NOMDISTR %in% cor_districts_listing_expanded,]
  
    
  #------------------------------------------------------------------------------#
  #### BUFFER ####
  
  cor.buf05 <- gBuffer(cor, width = 5000)
  cor.buf10 <- gBuffer(cor, width = 10000)
  
  
  #### Test plot
  
  png(file.path(OUT, "buffers_05km_and_10km.png"),
      width = 1000, height = 1000)
  
  plot(cor_districts)
  plot(cor.buf05, 
       col = rgb(red = 0, 
                 green = 0, 
                 blue = 1, 
                 alpha = 0.3),
       add = T)
  plot(cor.buf10, 
       col = rgb(red = 0, 
                 green = 1, 
                 blue = 0, 
                 alpha = 0.3),
       add = T)
  plot(cor, col = "red", lwd = 2, 
       add = T)
  
  dev.off()
  
  #------------------------------------------------------------------------------#
  #### VILLAGE BUFFER ####
  
  #### CADASTRE BUFFER ####
  
  #### MATCH EC DATA ####
  
  #### EC STATS ####
    
  # Jobs
  
  # Business
  
  #### HEAT MAP ####
  
  # Rwanda
  
  # Around corriodor
  
  #### CADASTRE MAP EXAMPLE ####
  
  