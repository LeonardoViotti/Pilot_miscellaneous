  #------------------------------------------------------------------------------#
  
  #	  LVTP - MASTER
  
  #------------------------------------------------------------------------------#
  #------------------------------------------------------------------------------#
  #### 1. SETTINGS ####
  
  # Set warnings 
  options(warn = 0)
  
  #### Delete everything already in R memory
  # (Equivalent to clear all in Stata)
  rm(list = ls())
  
  
  #-------------------------#
  #### 1.1 Load packages ####

  library(viridis)
  
  library(tidyverse)
  library(dplyr)
  library(readstata13)
  library(ggrepel)
  library(ggthemes)
  
  # Regression
  library(lfe)
  library(stargazer)
  
  # Spacial
  library(sp)
  library(sf)
  library(rgdal)
  library(rgeos)
  library(maptools)
  library(raster)
  library(broom)
  library(ggmap)
  library(leaflet)

  
  
  #----------------------------------#
  #### 1.2. Function definitions	####
  
  # Inverse hyperbolic sine
  inv_hsine <- 
    function(yi){
      log(yi+sqrt((yi^2)+1))
    }
  
  #------------------------------------------------------------------------------#
  #### SWITCHES ####
  
  DECSERVER_run = 0
  
  
  #------------------------------------------------------------------------------#
  #### PATHS ####
  
  
  
  # Leonardo
  if (Sys.getenv("USERNAME") == "wb519128" | Sys.getenv("USERNAME") == "WB519128"){
    if (DECSERVER_run == 1){
      DROPBOX <- file.path("//tsclient/C/Users/WB519128/Dropbox/Work/WB")
      GITHUB  <- file.path("//tsclient/C/Users/WB519128/Documents/GitHub/Pilot_miscellaneous")
    } else{
      DROPBOX <- file.path("C:/Users/WB519128/Dropbox/Work/WB")
      GITHUB  <- file.path("C:/Users/WB519128/Documents/GitHub/Pilot_miscellaneous")
    }
  }
  
  # Leonardo Laptop
  if (Sys.getenv("USERNAME") == "Leonardo" ){
    DROPBOX <- file.path("C:/Users/Leonardo/Dropbox/Work/WB")
    GITHUB  <- file.path("C:/Users/Leonardo/Documents/GitHub/Pilot_miscellaneous")
    
  }
  
 
  LVTP <- file.path(DROPBOX, "Rwanda Lake Victoria Transport Corridor")
  RFR  <- file.path(DROPBOX, "Rwanda Feeder Roads")
  
  
  #### Specific paths
  
  EC_data <- file.path(RFR, "data/admin/Establishment_census_data")
  RFR_shapeFiles <- file.path(RFR, "data/gis/Shapefiles")
  LVTP_shapeFiles <- file.path(LVTP, "data/corridor shapefile")
  
  RFR_data <- file.path(RFR, "data")
  LVTP_data <- file.path(LVTP, "data")
  
  RFR_CAD <- file.path(RFR_shapeFiles, "Cadastre")
  
  LAIS2018 <- file.path(RFR, "data/secondary data/LAIS/LAIS data request 2018")
  
  #CAD_temp_path <- "C:/Users/Leonardo/Dropbox/Work/cadaster/districts/"
  
  
  #CAD_data file.path(DROPBOX,
  
  OUT <- file.path(LVTP, "Analysis/Outputs")
  OUT_maps <- file.path(OUT, "Maps")
  OUT_tables <- file.path(OUT, "Tables")
  
  
  
