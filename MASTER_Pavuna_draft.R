#------------------------------------------------------------------------------#
  
#	  PAVUNA - MASTER (DRAFT)
  
#------------------------------------------------------------------------------#
  
  
# Porpuse:
  
#------------------------------------------------------------------------------#
#### SETTINGS ####

library(rgeos)
library(rgdal)
library(sp)
library(tidyverse)
library(openxlsx)
library(maps)
library(geosphere)

AZE_proj = F

if (AZE_proj){
  # Azimuthal equal distance projection with a point in the midle of Rwnada
  RjProj <-  CRS("+proj=aeqd +lat_0=-22.911522 +lon_0=-43.397503") 
} else{
  RjProj <-  CRS("+init=epsg:4326")  # unprojected
}

#------------------------------------------------------------------------------#
#### FILE PATHS ####

# Leonardo
if (Sys.getenv("USERNAME") == "wb519128" | Sys.getenv("USERNAME") == "WB519128"){
  DROPBOX <- file.path("C:/Users/WB519128/Dropbox/Work/WB/RJ - Pavuna")
  GITHUB  <- file.path("C:/Users/WB519128/Documents/GitHub/Pilot_miscellaneous")
  
}

DATA  <- file.path(DROPBOX, "Data")
GIS   <- file.path(DATA, "GIS")

ODMatrix <- file.path(DATA, "OD Matrix")

#------------------------------------------------------------------------------#