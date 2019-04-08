#------------------------------------------------------------------------------#

#	  PAVUNA - GIS CLEANNING DRAFT 

#------------------------------------------------------------------------------#


# Porpuse:

#------------------------------------------------------------------------------#
#### SETTINGS ####

# library(rgeos)
# library(rgdal)
# library(sp)
# library(tidyverse)
# library(openxlsx)
# library(maps)
# library(geosphere)
# 
# AZE_proj = F
# 
# if (AZE_proj){
#   # Azimuthal equal distance projection with a point in the midle of Rwnada
#   RjProj <-  CRS("+proj=aeqd +lat_0=-22.911522 +lon_0=-43.397503") 
# } else{
#   RjProj <-  CRS("+init=epsg:4326")  # unprojected
# }
# 
# #------------------------------------------------------------------------------#
# #### FILE PATHS ####
# 
# # Leonardo
# if (Sys.getenv("USERNAME") == "wb519128" | Sys.getenv("USERNAME") == "WB519128"){
#   DROPBOX <- file.path("C:/Users/WB519128/Dropbox/Work/WB/RJ - Pavuna")
#   GITHUB  <- file.path("C:/Users/WB519128/Documents/GitHub/Pilot_miscellaneous")
# 
# }
# 
# DATA  <- file.path(DROPBOX, "Data")
# GIS   <- file.path(DATA, "GIS")
# 
# ODMatrix <- file.path(DATA, "OD Matrix")

#------------------------------------------------------------------------------#
#### LOAD DATA ####


sc <- readOGR(dsn = file.path(GIS, "Setores_censitarios"), "33SEE250GC_SIR")
sc <- spTransform(sc, RjProj)

rjn <- readOGR(dsn = file.path(GIS, "Bairros"), "Limite_Bairro")
rjn <- spTransform(rjn, RjProj)

mun <- readOGR(dsn = file.path(GIS, "Municipios_RJ"), "Municipios")
mun <- spTransform(mun, RjProj)


#od <- read.xlsx(file.path(ODMatrix, "Deslocamento3-Motivos OK.xlsx"), colNames = TRUE)
od <- read.csv(file.path(ODMatrix, "Deslocamento3-Motivos OK.csv"), header = TRUE)

sCensZona <- read.csv(file.path(ODMatrix, "Setor Censitário e Zoneamento Final.csv"), 
                      header = TRUE,
                      colClasses = "character")

#------------------------------------------------------------------------------#
#### SUBSETS ####

mun_rj <- mun %>% subset(Municipio == "Rio de Janeiro")

rj <- sc %>% subset(NM_MUNICIP == "RIO DE JANEIRO")

pavuna_nbhd <- c(310,309,315,311,252,322,260,316,313,312,253)
od_pav <- od %>% subset(Zona_Orig %in% pavuna_nbhd)

#sCensZona_pav <- sCensZona %>% subset(Zona.2012 %in% pavuna_nbhd)
names(sCensZona) <-c("setor","zona")


#------------------------------------------------------------------------------#
#### Pavuna OD matrix ####

odm_pav <- od_pav[,c("Zona_Orig", "Zona_Dest")]

odm_pav$Zona_Orig <- as.character(odm_pav$Zona_Orig)
odm_pav$Zona_Dest <- as.character(odm_pav$Zona_Dest)


# Number of trips

odm_pav <- 
  odm_pav %>% 
    group_by(Zona_Orig, Zona_Dest) %>%
    summarise(trips = n())


#------------------------------------------------------------------------------#
#### Setores Censitarios into zonas ####

# Add Zone Vatiable to setores censitarios shapefile
rj <- merge(rj, sCensZona, by.x = "CD_GEOCODI", by.y = "setor")

# Add Zona to neigbourhoods shapefiles

#### S. censitario centriods
rj_ct <- gCentroid(rj, byid = T)

rj_ctdf <- 
  SpatialPointsDataFrame(coords = rj_ct,
                         data = rj@data,
                         proj4string = RjProj)

rj_ctdf <- rj_ctdf[, c("CD_GEOCODI", "zona")]
names(rj_ctdf) <- c("setor","zona")

rj_ctdf$lat <- rj_ctdf@coords[,2]
rj_ctdf$long <- rj_ctdf@coords[,1]


#### Get centroids of each zone

zona_coords <- 
  rj_ctdf@data %>% 
    group_by(zona) %>% 
    summarise(lat=mean(lat), long=mean(long))


zmergeO <- zona_coords
names(zmergeO) <- c("zona", "olat", "olong")

odm_pav <- left_join(odm_pav, zmergeO, by = c("Zona_Orig" = "zona"))


zmergeD <- zona_coords
names(zmergeD) <- c("zona", "dlat", "dlong")

odm_pav <- left_join(odm_pav, zmergeD, by = c("Zona_Dest" = "zona"))

#------------------------------------------------------------------------------#
#### Plot ####


xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)

ggplot()+
  geom_polygon(data= mun_rj, 
               aes(x=long, y= lat, group = group), fill = "grey30") + 
  geom_point(data = odm_pav,
             aes(x= dlong , y= dlat),
             size = .88,
             col = "cadetblue2") + 
  geom_segment(data = odm_pav,
               aes(x=olong, 
                   y=olat,
                   xend=dlong, 
                   yend=dlat, 
                   alpha = trips), 
               col="cadetblue2",
               size = .9)+
  #Here is the magic bit that sets line transparency - essential to make the plot readable
  scale_alpha_continuous(range = c(0.1, 0.6))+
  quiet+coord_equal()





# sc_orig <-  data.frame(zona = unique(odm_pav$Zona_Orig))
# sc_dest <-  data.frame(zona = unique(odm_pav$Zona_Dest))
# 
# sc_orig <- left_join(sc_orig, sCensZona, by = c("zona"))
# sc_dest <- left_join(sc_dest, sCensZona, by = c("zona"))
# 
# 
# #rj <- merge(rj, sc_orig, by.x = "CD_GEOCODI", by.y = "setor")
# rj <- merge(rj, sc_dest, by.x = "CD_GEOCODI", by.y = "setor")
# 
# 
# 
# 
# #### Other stuff
# rj_od <- rj %>% subset(!is.na(zona))
# 
# rj_od_ct <- gCentroid(rj_od, byid = T)
# 
# foo	<- rjn %>% subset(gIntersects(rjn,
#                                 bar,
#                                 byid = T) %>% as.logical)
# 
# rjn_od <- subset(rjn, over(rjn, rj_od_ct) %>% as.logical)
# rjn_od_ct <- gCentroid(rjn_od, byid = T)
# 
# 
# #------------------------------------------------------------------------------#
# #### routs ####
# bar <- gCentroid(rj_od, byid = T)
# 
# p1 <- bar[10504,]
# p2 <- bar[42,]
# 
# inter <- gcIntermediate(p1,  p2, n=50, addStartEnd=TRUE, breakAtDateLine=F)             
# lines(inter, col="slateblue", lwd=2)


