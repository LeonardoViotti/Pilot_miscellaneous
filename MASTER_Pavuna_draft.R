#------------------------------------------------------------------------------#
  
#	  PAVUNA - MASTER (DRAFT)
  
#------------------------------------------------------------------------------#
  
  
# Porpuse:
  
#------------------------------------------------------------------------------#
#### SETTINGS ####


#------------------------#
#### Setting switches ####

# Projected or unprojected
AZE_proj = F



library(data.table)
library(openxlsx)
library(tidyverse)

# Spatial
library(rgeos)
library(rgdal)
library(sp)
library(maps)
library(geosphere)



# Projection definition
if (AZE_proj){
  # Azimuthal equal distance projection with a point in the midle of Rio
  RjProj <-  CRS("+proj=aeqd +lat_0=-22.911522 +lon_0=-43.397503") 
} else{
  # unprojected
  RjProj <-  CRS("+init=epsg:4326")  
}


#------------------------------------------------------------------------------#
#### SECTION SWITCHES ####

RUN_RAIS_draftanalys = F
RUN_GIS_processing = F

#------------------------------------------------------------------------------#
#### FILE PATHS ####

# Leonardo
if (Sys.getenv("USERNAME") == "wb519128" | Sys.getenv("USERNAME") == "WB519128"){
  DROPBOX <- file.path("C:/Users/WB519128/Dropbox/Work/WB/RJ - Pavuna")
  GITHUB  <- file.path("C:/Users/WB519128/Documents/GitHub/Pilot_miscellaneous")
  
}

DATA      <- file.path(DROPBOX, "Data")

# Specific paths
GIS       <- file.path(DATA, "GIS")
RAIS_did  <- file.path(DATA, "RAIS/RAW")


ODMatrix <- file.path(DATA, "OD Matrix")

#------------------------------------------------------------------------------#
#### SECTIONS ####

if (RUN_RAIS_draftanalys){
  #source
}


if (RUN_GIS_processing){
  #source
}


