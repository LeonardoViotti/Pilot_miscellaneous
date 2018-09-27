#------------------------------------------------------------------------------#

#	  LVTP - Corridor cadastre map

#------------------------------------------------------------------------------#


CAD_temp <- file.path("C:/Users/Leonardo/Dropbox/Work/cadaster/districts")

#------------------------------------------------------------------------------#
#### Load data ####

# Corridor
cor <- readOGR(LVTP_shapeFiles, "Ngoma_Nyanza")


Ngo_cad <- readOGR(file.path(CAD_temp, "ngoma"),
                   "ngoma")

#-----------------#
#### Reproject ####

RwaProj <-  CRS("+proj=aeqd +lat_0=-1.999036 +lon_0=29.95608") 
RwaProj_unp <-  CRS("+init=epsg:4326")  # unprojected

cor <- spTransform(cor, RwaProj)

proj4string(Ngo_cad) <- RwaProj

#------------------------------------------------------------------------------#
#### Corridor buffer ####

cor.buf01 <- gBuffer(cor, width = 1000)

#------------------------------------------------------------------------------#
#### Intersection  ####

cad.buff	<- subset(Ngo_cad,
                    gIntersects(Ngo_cad,
                                gUnaryUnion(cor.buf01),
                                byid = T) %>% as.logical)

#------------------------------------------------------------------------------#
#### ______MAPS_______ ####

#------------------------------------------------------------------------------#
#### Reproject ####

cor <- spTransform(cor, RwaProj_unp)
cad.buff <- spTransform(cad.buff, RwaProj_unp)


#------------------------------------------------------------------------------#
##### Transfor to dataframe ####

cor.df <- tidy(cor)
cad.buff.df <- tidy(cad.buff)

#------------------------------------------------------------------------------#
##### Base map ####

basemap_cor <- get_map(location = c(lon=mean(cor_districts.df$long), 
                                    lat=mean(cor_districts.df$lat)), 
                       zoom= 8,
                       maptype="satellite") # roadmap, satellite, etc. See help(get_map)
