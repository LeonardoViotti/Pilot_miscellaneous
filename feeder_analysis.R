#------------------------------------------------------------------------------#

#			LVTP - Feeder roads analysis for CN

#------------------------------------------------------------------------------#

EXPORT_shapes <- F


#------------------------------------------------------------------------------#
#### LOAD AND PROCESS SHAPEFILES ####

#### Shapefiles
fr_sample	<- readOGR(file.path(RFR_shapeFiles,"Roads/Sample"), "feeder_sample_abr18", pointDropZ = T)
nat_roads <- readOGR(file.path(RFR_shapeFiles,"Roads"), "National_rds")

villages <- readOGR(file.path(RFR_shapeFiles,"Villages"), "processed_villages_boundaries")

#### EC
ec <- read.dta13(file.path(EC_data, "2014/2014 Establishment Census.dta"))


#-----------------#
#### Reproject ####

# Define projection
RwaProj <-  CRS("+proj=aeqd +lat_0=-1.999036 +lon_0=29.95608") 

# Reproject
fr_sample <- spTransform(fr_sample, RwaProj)
nat_roads	<- spTransform(nat_roads, RwaProj)
villages <- spTransform(villages, RwaProj)

#-----------------------------#
#### Remove large villages ####

# Larger than .99 percentile
villages <- villages[villages@data$Shape_Area < quantile(villages@data$Shape_Area, .99),]

#------------------------------#  
#### Simplify Feeders shape ####

fs_simp.pl <-  gSimplify(fr_sample, 
                         tol=.0001, 
                         topologyPreserve=TRUE) 
fs_simp <- SpatialLinesDataFrame(fs_simp.pl, 
                                 data = fr_sample@data,
                                 match.ID = FALSE)
fr_sample <- NULL


#------------------------------------------------------------------------------#
#### CALCULATE BUFFERS ####

fs.buf <- gBuffer(fs_simp, width = 1000, byid = T)
nat.buf <- gBuffer(nat_roads, width = 1000)

#------------------------------------------------------------------------------#
#### INTERSECTION ####

connect.buf <- intersect(fs.buf, nat.buf)
isolated.buf <- gDifference(fs.buf, connect.buf)




#------------------------------------------------------------------------------#
#### Merge with EC ####

# Transforme province code

ec$ID1_new <- NA
ec$ID1_new[ec$ID1 == "KIGALI CITY"]       <- 1
ec$ID1_new[ec$ID1 == "SOUTHERN PROVINCE"] <- 2
ec$ID1_new[ec$ID1 == "WESTERN PROVINCE"]  <- 3
ec$ID1_new[ec$ID1 == "NORTHERN PROVINCE"] <- 4
ec$ID1_new[ec$ID1 == "ESTHERN PROVINCE"]  <- 5

# Village code
ec$dist_code <- (ec$ID1_new*10) + ec$ID2
ec$sect_code <- (ec$dist_code*100) + ec$ID3
ec$cell_code <- (ec$sect_code*100) + ec$ID4
ec$vill_code <- (ec$cell_code*100) + ec$ID5

# Aggregate number of business by village
busiVill <- aggregate(key ~ vill_code,
                      data = ec,
                      FUN = length)
jobsVill <- aggregate(Q21C1 ~ vill_code,
                      data = ec,
                      FUN = sum,
                      na.action = NULL)
ecVilldata <- busiVill
ecVilldata$N_jobs <- jobsVill$Q21C1

names(ecVilldata) <- c("Code_vill_", "N_busi", "N_jobs")

#### Merge ####

villages <- merge(villages, ecVilldata, by = "Code_vill_")


#------------------------------------------------------------------------------#
#### Villages within buffers ####

vill_cont	<- subset(villages,
                    gIntersects(villages,
                                gUnaryUnion(isolated.buf),
                                byid = T) %>% as.logical)

vill_treat<- subset(villages,
                    gIntersects(villages,
                                gUnaryUnion(connect.buf),
                                byid = T) %>% as.logical)
#------------------------------------------------------------------------------#
#### REGRESSION ####

#### Create Data set ####
vill_cont@data$treated <- 0
vill_treat@data$treated <- 1

regVill_df <- rbind(vill_cont@data, vill_treat@data )

foo <- lm(N_busi ~ treated,
          data = regVill_df)

bar <- lm(N_jobs ~ treated,
          data = regVill_df )

#------------------------------------------------------------------------------#
#### EXPORT SHAPEFILES ####

connect.buf <- as(connect.buf, "SpatialPolygonsDataFrame")
isolated.buf <- as(isolated.buf, "SpatialPolygonsDataFrame")


if (EXPORT_shapes){

  writeOGR(fs_simp,
           dsn=file.path(OUT, "Shape-files/feeder_sample.shp"),
           layer="feeder_sample",
           overwrite_layer=T,
           driver="ESRI Shapefile")
  
  writeOGR(nat_roads,
           dsn=file.path(OUT, "Shape-files/national_roads.shp"),
           layer="national_roads",
           overwrite_layer=T,
           driver="ESRI Shapefile")
  
  writeOGR(connect.buf,
           dsn=file.path(OUT, "Shape-files/connected_buffer.shp"),
           layer="connected_buffer",
           overwrite_layer=T,
           driver="ESRI Shapefile")
  
  writeOGR(isolated.buf,
           dsn=file.path(OUT, "Shape-files/isolated_buffer.shp"),
           layer="isolated_buffer",
           overwrite_layer=T,
           driver="ESRI Shapefile")
}
