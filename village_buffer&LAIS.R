#------------------------------------------------------------------------------#

#	  LVTP - Cadastre maps around corridor

#------------------------------------------------------------------------------#


CAD_temp <- file.path("C:/Users/Leonardo/Dropbox/Work/cadaster/districts")
OUT_temp <- file.path("C:/Users/Leonardo/Dropbox/Work/WB/Rwanda Feeder Roads/data/admin/LAIS/LAIS data request 2018/Combined")

#------------------------------------------------------------------------------#
#### Load data ####

lais <- read.csv(file.path(LAIS2018, "Combined/LAIS18.csv"), header = T, stringsAsFactors=FALSE)

#### Shapefiles

# Corridor
cor <- readOGR(LVTP_shapeFiles, "Ngoma_Nyanza")

# Districts
districts <- readOGR(file.path(RFR_shapeFiles,"Districts"), "District_boundaries")
#nat_roads <- readOGR(file.path(RFR_shapeFiles,"Roads"), "National_rds")

districts_simp <- gSimplify(districts,
                            tol=.001, 
                            topologyPreserve=TRUE)

districts_simp <- SpatialPolygonsDataFrame(districts_simp, 
                                           data = districts@data,
                                           match.ID = FALSE)

districts <- districts_simp
districts_simp <- NULL


# Villages
villages <- readOGR(file.path(RFR_shapeFiles,"Villages"), "processed_villages_boundaries")

# Cells
#cells <- readOGR(file.path(RFR_shapeFiles,"Cells"), "Cell_Boundary_2012")


#-----------------#
#### Reproject ####

# Define projection
RwaProj <-  CRS("+proj=aeqd +lat_0=-1.999036 +lon_0=29.95608") 

# Reproject
districts <- spTransform(districts, RwaProj)
cor	<- spTransform(cor, RwaProj)
villages <- spTransform(villages, RwaProj)
cells <- spTransform(cells, RwaProj)
nat_roads <- spTransform(nat_roads , RwaProj)

#------------------------------------------------------------------------------#
#### Keep only sell transactions ####

lais$sell <- 0
lais$sell[grepl("Transfer of", lais$description)] <- 1

lais <- lais[lais$sell == 1,]


#------------------------------------------------------------------------------#
#### Aggregate by village level ####



lais$vill_code_str <- paste0(lais$District, lais$Sector, lais$Cell, lais$Village)

lais_vill <- aggregate(UPI ~ vill_code_str, 
                       FUN = length,
                       data = lais)

#------------------------------------------------------------------------------#
#### Merge with village shape files ####


villages@data$vill_code_str <- 
  paste0(villages@data$District, villages@data$Sector, villages@data$Cellule_1, villages@data$Village)


villages <- merge(villages, lais_vill, by = 'vill_code_str')


#------------------------------------------------------------------------------#
#### SUBSET VILLAGE SHAPEFILES ####

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

#cor_districts <- districts[districts@data$NOMDISTR %in% cor_districts_listing,]
cor_districts <- districts[districts@data$NOMDISTR %in% cor_dist_lis_expanded,]

#### Drop villages in other districts ####

cor_dist_vills <- villages[toupper(villages@data$District) %in% cor_dist_lis_expanded,]

#------------------------------------------------------------------------------#
#### BUFFER ####

cor.buf05 <- gBuffer(cor, width = 5000)


#------------------------------------------------------------------------------#
#### VILLAGE BUFFER ####

vill.buff	<- subset(cor_dist_vills,
                    gIntersects(cor_dist_vills,
                                gUnaryUnion(cor.buf05),
                                byid = T) %>% as.logical)

#------------------------------------------------------------------------------#
#### ______MAPS_______ ####


#------------------------------------------------------------------------------#
#### REPROJECT ####
RwaProj_unp <-  CRS("+init=epsg:4326")  # unprojected

districts <- spTransform(districts, RwaProj_unp)
villages <- spTransform(villages, RwaProj_unp)

nat_roads <- spTransform(nat_roads, RwaProj_unp)

cor_districts <- spTransform(cor_districts, RwaProj_unp)
cor	<- spTransform(cor, RwaProj_unp)
#villages <- spTransform(villages, RwaProj)
cor.buf05 <- spTransform(cor.buf05, RwaProj_unp)

vill.buff <- spTransform(vill.buff, RwaProj_unp)



##### Transfor to dataframe
districts.df <- tidy(districts)
#villages.df <- tidy(villages, region = "Code_vill_")
#villages.df <- merge(villages.df, villages, by.x = "id", by.y = "Code_vill_")

vill.buff.df <- tidy(vill.buff, region = "Code_vill_")
vill.buff.df <- merge(vill.buff.df, vill.buff, by.x = "id", by.y = "Code_vill_")


cor_districts.df <- tidy(cor_districts)
cor.df <- tidy(cor)
cor.buf05.df <- tidy(cor.buf05)  

#nat_roads.df <- tidy(nat_roads)


#------------------------------------------------------------------------------#


#------------------------------------------------------------------------------#
#### Transactions heat map ####


#### Around corridor

heat_cols <- c("Corridor" = "firebrick")

#cor_heatMap <- 
ggplot()+
  geom_polygon(data = vill.buff.df, 
               aes(y= lat, 
                   x = long,
                   group = group,
                   fill = UPI )) +
  geom_path(data = cor.df,
            aes(y= lat, 
                x = long,
                group = group,
                col = "Corridor"),
            size = 1) +
  theme_void() + 
  
  theme(legend.text = element_text(size = 11),
        legend.position = c(0.85, 0.2)) +
  
  scale_fill_gradientn(name ="Number of land\ntransfer transctions",
                       colours = viridis(10),
                       #labels = c("","5", "10", "15","20", "25+")
                       ) +

  scale_colour_manual(name = "",
                      values=heat_cols) +
  coord_quickmap() +
  
  
  ggsave(file.path(OUT_maps, "Corridor_landTransctions_heatMAp.png"),
         width = 30, height = 15, units = "cm")

