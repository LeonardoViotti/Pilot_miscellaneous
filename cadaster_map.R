#------------------------------------------------------------------------------#

#	  LVTP - Corridor cadastre map

#------------------------------------------------------------------------------#


CAD_temp <- file.path("C:/Users/Leonardo/Dropbox/Work/cadaster/districts")

#------------------------------------------------------------------------------#
#### Load data ####

# Corridor
cor <- readOGR(LVTP_shapeFiles, "Ngoma_Nyanza")

# Districts
districts <- readOGR(file.path(RFR_shapeFiles,"Districts"), "District_boundaries")

# Ngoma
Ngo <- districts[districts@data$NOMDISTR == "NGOMA",]

# Ngomae cadastre
Ngo_cad <- readOGR(file.path(CAD_temp, "ngoma"),
                   "ngoma")
proj4string(Ngo_cad) <- CRS("+proj=tmerc +lat_0=0 +lon_0=30 +k=0.9999 +x_0=500000 +y_0=5000000 +ellps=GRS80 +units=m +no_defs")


#-----------------#
#### Reproject ####

RwaProj <-  CRS("+proj=aeqd +lat_0=-1.999036 +lon_0=29.95608") 
RwaProj_unp <-  CRS("+init=epsg:4326")  # unprojected

cor <- spTransform(cor, RwaProj)
Ngo <- spTransform(Ngo, RwaProj)


Ngo_cad <- spTransform(Ngo_cad, RwaProj)

#------------------------------------------------------------------------------#
#### Corridor buffer ####

cor.buf01 <- gBuffer(cor, width = 2500)

#------------------------------------------------------------------------------#
#### Intersections  ####

cad.buff	<- subset(Ngo_cad,
                    gIntersects(Ngo_cad,
                                gUnaryUnion(cor.buf01),
                                byid = T) %>% as.logical)

cor_ngo <- gIntersection(cor, Ngo, byid = T)
cor_ngo <- SpatialLinesDataFrame(cor_ngo, 
                                data = data.frame(ID = "0 10"), 
                                match.ID = FALSE)


cad.buff_ngo <- gIntersection(cad.buff, Ngo, byid = T)


#------------------------------------------------------------------------------#
#### ______MAPS_______ ####

#------------------------------------------------------------------------------#
#### Reproject ####

cor_ngo <- spTransform(cor_ngo, RwaProj_unp)
cad.buff_ngo <- spTransform(cad.buff, RwaProj_unp)
Ngo <-  spTransform(Ngo, RwaProj_unp)
cor <- spTransform(cor, RwaProj_unp)
#------------------------------------------------------------------------------#
##### Transfor to dataframe ####

Ngo.df <- tidy(Ngo)
cor_ngo.df <- tidy(cor_ngo)
#cor.df <- tidy(cor)
cad.buff_ngo.df <- tidy(cad.buff_ngo)


#------------------------------------------------------------------------------#
##### Base map ####

basemap_cor <- get_map(location = c(lon=mean(Ngo.df$long), 
                                    lat=mean(Ngo.df$lat)), 
                       zoom= 12,
                       maptype="satellite") # roadmap, satellite, etc. See help(get_map)


colsMap1 <- c( "Corridor" = "firebrick")


#ggplot()+
ggmap(basemap_cor)+

  geom_polygon(data= cad.buff_ngo.df, 
               aes(x= long, 
                   y = lat,
                   group = group),
               color = 'black',
               alpha = .3) +
  geom_path(data = cor_ngo.df, 
            aes(x= long, 
                y = lat,
                group = group,
                col = "Corridor"),
            size=1.5)+
coord_fixed(xlim=c(30.50, 30.55), ylim=c(-2.15, -2.2), ratio = 1) +
  theme(panel.background =element_blank(),
        panel.border = element_rect(colour = "black", fill=NA, size=.5),
        # legend.position = "bottom", 
        legend.text = element_text(size = 12),
        # legend.key.size = unit(.25, "cm"),
        legend.position = c(0.85, 0.15),
        legend.key = element_rect(color = NA, fill = NA),
        legend.title=element_blank(),
        legend.background = element_rect(fill = alpha("white", 0.5)),
        axis.text = element_text(size = 6),
        axis.title.x=element_blank(), 
        axis.title.y=element_blank()	) +
  scale_colour_manual(values=colsMap1)  +
  scale_fill_manual(values=colsMap1) +
  ggsave(file.path(OUT_maps, "corridor_cadastre_buffer.png"),
         width = 15, height = 15, units = "cm")

  