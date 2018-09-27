#------------------------------------------------------------------------------#

#			LVTP - Feeder roads analysis for CN

#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
#### LOAD data ####

#### Load LAIS
lais_E <- read.csv(file.path(LAIS2018, "WB_Eastern.csv"), header = T, sep = ";", stringsAsFactors=FALSE)
lais_W <- read.csv(file.path(LAIS2018, "WB_Western.csv"), header = T, sep = ";", stringsAsFactors=FALSE)
lais_S <- read.csv(file.path(LAIS2018, "WB_Southern.csv"), header = T, sep = ";", stringsAsFactors=FALSE)
lais_N <- read.csv(file.path(LAIS2018, "WB_Northern.csv"), header = T, sep = ";", stringsAsFactors=FALSE)

#### Shapefiles
fr_sample	<- readOGR(file.path(RFR_shapeFiles,"Roads/Sample"), "feeder_sample_abr18", pointDropZ = T)
nat_roads <- readOGR(file.path(RFR_shapeFiles,"Roads"), "National_rds")

#villages <- readOGR(file.path(RFR_shapeFiles,"Villages"), "processed_villages_boundaries")


#### Road monitoring
rms	 	<- read.dta13(	file.path(RFR_data, 
                               "Road Monitoring/Data/master_monitoring_objectid&status.dta"),
                     convert.factors = F)

#------------------------------------------------------------------------------#
#### PROCESS SHAPES ####

#-----------------#
#### Reproject ####

# Define projection
RwaProj <-  CRS("+proj=aeqd +lat_0=-1.999036 +lon_0=29.95608") 

# Reproject
fr_sample <- spTransform(fr_sample, RwaProj)
nat_roads	<- spTransform(nat_roads, RwaProj)
#villages <- spTransform(villages, RwaProj)

#------------------------------------------------------------------------------#
#### PROCESS LAIS ####


lais$upi <-as.numeric(gsub("/", "", lais$upi))


unique(gsub("Â","", lais_E$description))

#### Change factor levels with crazy characteds

lais_E$description <- gsub("Â","", lais_E$description)
lais_W$description <- gsub("Â","", lais_W$description)
lais_S$description <- gsub("Â","", lais_S$description)
lais_N$description <- gsub("Â","", lais_N$description)

#### Combine datasets ####
lais <- rbind(lais_E, lais_W, lais_S, lais_N)

# Fix upi
lais$upi <-as.numeric(gsub("/", "", lais$upi))


#### Keep only sell transactions

lais$sell <- 0
lais$sell[grepl("Transfer of", lais$description)] <- 1

lais <- lais[lais$sell == 1,]

#### Keep only interest varaibles
lais <- lais[, c("upi", "area", "fees_amount", "sell")]

names(lais) <- c("UPI", "area", "price", "sell")

#------------------------------------------------------------------------------#
#### CALCULATE BUFFERS ####

fs.buf <- gBuffer(fr_sample, width = 1000, byid = T)
#nat.buf <- gBuffer(nat_roads, width = 1000, byid = T)
nat.buf <- gBuffer(nat_roads, width = 1000)


#------------------------------------------------------------------------------#
#### INTERSECTION ####

connect.buf <- intersect(fs.buf, nat.buf)
isolated.buf <- gDifference(fs.buf, connect.buf)


#------------------------------------------------------------------------------#
#### DISTRICTS LOOP ####

# Districts with feeder roads
dFeed <- unique(rms$feeder_district_rms)

start_time <- Sys.time()


for (dis in dFeed){
#for (dis in  "Gisagara"){

  cad_i <- NULL
  cad_i <- readOGR(file.path(RFR_CAD, dis),
                   paste0(dis, "_cad"))
  
  cad_i <- spTransform(cad_i, RwaProj)
  
  #### Plot centroids
  cad_i_ct_sh <- NULL
  cad_i_ct_sh <- gCentroid(cad_i, byid = T)
  
  cad_i_ct    <- NULL
  cad_i_ct    <- SpatialPointsDataFrame(cad_i_ct_sh, 
                                        data = cad_i@data,
                                        match.ID = FALSE)
  # remove centroids
  rm(cad_i_ct_sh)
  
  # Remove original shapefiles
  rm(cad_i)
  
  
  #----------------------------#
  #### Merge with LAIS ####
  cad_i_ct <- merge(cad_i_ct, lais, by = "UPI", duplicateGeoms = TRUE)
  
  #----------------------------#
  #### Plots within buffers ####
  cad_cont  <- NULL
  cad_cont	<- subset(cad_i_ct,
                      gIntersects(cad_i_ct,
                                  gUnaryUnion(isolated.buf),
                                  byid = T) %>% as.logical)
  cad_treat<- NULL
  cad_treat<- subset(cad_i_ct,
                      gIntersects(cad_i_ct,
                                  gUnaryUnion(connect.buf),
                                  byid = T) %>% as.logical)
  
  ### Create Data set ####
  cad_cont@data$treated <- 0
  cad_treat@data$treated <- 1
  
  cad_data_i <- NULL
  cad_data_i <- rbind(cad_cont@data, cad_treat@data)
  
  # Remove shapefiles
  rm(cad_treat, cad_cont)
  
  
  #### Store dataset in object ####
  assign(paste0(dis, "_cadData"),
         cad_data_i)
  
}

end_time <- Sys.time()
end_time - start_time

cadData_list <- paste0(dFeed, "_cadData")



#------------------------------------------------------------------------------#
#### SINGLE DIFFERENCE ####

# foo






