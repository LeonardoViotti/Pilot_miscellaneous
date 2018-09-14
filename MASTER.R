  #------------------------------------------------------------------------------#
  
  #	  LVTP - MASTER
  
  #------------------------------------------------------------------------------#
  
  
  
  library(rgdal)
  library(rgeos)
  library(sp)
  library(tidyverse)
  library(dplyr)
  library(readstata13)
  
  #------------------------------------------------------------------------------#
  #### PATHS ####
  
  DROPBOX <- "C:/Users/Leonardo/Dropbox/Work/WB"
  GITHUB  <- "C:/Users/Leonardo/Documents/GitHub/Pilot_miscellaneous"
  
  LVTP <- file.path(DROPBOX, "Rwanda Lake Victoria Transport Corridor")
  RFR  <- file.path(DROPBOX, "Rwanda Feeder Roads")
  
  EC_data <- file.path(RFR, "data/admin/Establishment_census_data")
  RFR_shapeFiles <- file.path(RFR, "data/gis/Shapefiles")
  LVTP_shapeFiles <- file.path(LVTP, "data/corridor shapefile")
  
  CAD_temp_path <- "C:/Users/Leonardo/Dropbox/Work/cadaster/districts/"
  
  
  #CAD_data file.path(DROPBOX,
  
  OUT <- file.path(LVTP, "Analysis")
  
