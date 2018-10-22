#------------------------------------------------------------------------------#

#			LVTP - Feeder roads analysis for CN

#------------------------------------------------------------------------------#

# Export regression tables
EXPORT_tables = F

# Match cadastre and transactions
RUN_cadIntersection = F
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
                               "primary data/Road Monitoring/Data/master_monitoring_objectid&status.dta"),
                     convert.factors = F)



villages <- readOGR(file.path(RFR_shapeFiles,"Villages"), "processed_villages_boundaries")
villages <- villages[villages@data$Shape_Area < 90e+06,]


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


villages <- spTransform(villages, RwaProj)


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
#### PROCESS LAIS ####

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
lais <- lais[, c("upi", "area", "fees_amount", "sell", "approval_date")]

names(lais) <- c("UPI", "area", "price", "sell", "aDate")

#### Date variables
lais$aDate <- as.Date(lais$aDate)
lais$aDate_year <- format(lais$aDate,"%Y")
lais$aDate_month <- format(lais$aDate,"%m")


#------------------------------------------------------------------------------#
#### CALCULATE BUFFERS ####

fs.buf <- gBuffer(fs_simp, width = 1000, byid = T)
#nat.buf <- gBuffer(nat_roads, width = 1000, byid = T)
nat.buf <- gBuffer(nat_roads, width = 1000)


#------------------------------------------------------------------------------#
#### INTERSECTION ####

connect.buf <- intersect(fs.buf, nat.buf)
isolated.buf <- gDifference(fs.buf, connect.buf)


#------------------------------------------------------------------------------#
#### DISTRICTS LOOP ####

if (RUN_cadIntersection){

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
    #rm(cad_i)
    
    
    #----------------------------#
    #### Merge with LAIS ####
    cad_i_ct <- merge(cad_i_ct,
                      lais,
                      by = "UPI",
                      all.x = T,
                      duplicateGeoms = TRUE)

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
    #rm(cad_treat, cad_cont)
    
    
    #### Store dataset in object ####
    assign(paste0(dis, "_cadData"),
           cad_data_i)
    
  }
  
  end_time <- Sys.time()
  end_time - start_time
  
  cadData_list <- paste0(dFeed, "_cadData")
  
  #### Combine data sets
  foo <- rbind(Bugesera_cadData,
               Rulindo_cadData,
               Rubavu_cadData,
               Muhanga_cadData,
               Huye_cadData,
               Ngoma_cadData,
               Ngororero_cadData,
               Rwamagana_cadData,
               Karongi_cadData,
               Nyamasheke_cadData,
               Gisagara_cadData,
               Burera_cadData,
               Gicumbi_cadData,
               Musanze_cadData,
               Rusizi_cadData,
               Nyagatare_cadData, 
               Nyaruguru_cadData,
               Gatsibo_cadData,
               Rutsiro_cadData,
               Nyabihu_cadData,   
               Gakenke_cadData )
  
  write.csv(foo,
            #file = "C:/Users/WB519128/Dropbox/Work/WB/Rwanda Lake Victoria Transport Corridor/data/RFR_feeder_CN_data/cad_feed1km_data.csv",
            file.path(LVTP_data, "RFR_feeder_CN_data/cad_feed1km_101818.csv"),
            row.names = F)

}else{
  cad1km <- read.csv(file.path(LVTP_data, "RFR_feeder_CN_data/cad_feed1km_101818.csv"),
                     header = T)
}


#------------------------------------------------------------------------------#
#### Village level cadaster data ####
cad1km$vill_code_str <- paste0(cad1km$District, cad1km$Sector, cad1km$Cell, cad1km$Village)


cad1km_vill <- aggregate(sell ~ vill_code_str,
                         FUN = sum,
                         na.rm = T,
                         na.action=NULL,
                         data = cad1km)

cad_area <- aggregate(SHAPE_area ~ vill_code_str,
                      FUN = mean,
                      na.rm = T,
                      na.action=NULL,
                      data = cad1km)


cad1km_vill$area_m <- cad_area$SHAPE_area

cad1km_vill$area <- cad1km_vill$area_m/1000000




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

#foo <- gIntersects(vill_cont, vill_treat)

# Remove duplicated villages
vill_cont <- vill_cont[!(vill_cont@data$Code_vill_ %in% vill_treat@data$Code_vill_),]

#### Villages centroids
villages_ct_shape <- gCentroid(villages, byid = T)

villages_ct <- SpatialPointsDataFrame(villages_ct_shape, 
                                      data = villages@data,
                                      match.ID = FALSE)


### Create Data set ####
vill_cont@data$treated <- 0
vill_treat@data$treated <- 1

#------------------------------------------------------------------------------#
#### Merge with transaction data ####



regVill_df <- rbind(vill_cont@data, vill_treat@data)

regVill_df$vill_code_str <- 
  paste0(regVill_df$District, regVill_df$Sector, regVill_df$Cellule_1, regVill_df$Village)


#cad1km_vill <- merge(cad1km_vill, regVill_df[,c("vill_code_str", "treated")], by = "vill_code_str")


villData_reg <- merge(regVill_df[,c("vill_code_str",
                                    "District", 
                                    "treated")], 
                      cad1km_vill,
                      all.x = T,
                      by = "vill_code_str" )


#### Replace NAs with zeros, i.e. villages that had no transactions
villData_reg$sell[is.na(villData_reg$sell)] <- 0


#### Specifc districts 


#rwa_regData <- villData_reg[villData_reg$District == "Rwamagana",] 

#------------------------------------------------------------------------------#
#### SINGLE DIFFERENCE ####

reg1 <- felm(sell ~ treated  | 0 | 0 | vill_code_str,
             data = villData_reg )

reg2 <- felm(sell ~ treated + area | 0 | 0 | vill_code_str,
             data = villData_reg )

reg1_l <- felm(inv_hsine(sell) ~ treated  | 0 | 0 | vill_code_str,
             data = villData_reg )

reg2_l <- felm(inv_hsine(sell) ~ treated + area | 0 | 0 | vill_code_str,
             data = villData_reg )


# reg1_rw <- felm(sell ~ treated  | 0 | 0 | vill_code_str,
#              data = rwa_regData )
# 
# reg2_rw <- felm(sell ~ treated + area | 0 | 0 | vill_code_str,
#              data = rwa_regData )



reg1km_cad_tex <- 
  stargazer(reg1,
            reg2,
            reg1_l,
            reg2_l,
            title = "Number of transference transctions - simple difference",
            covariate.labels = c("Connected (1km buffer intersection)",
                                 "Average plot area (sq. Km)"),
            dep.var.labels   = c("Number of transactions",
                                 "Number of transactions (log)"),
            omit.stat = c("f", "ser"),
            add.lines = list(
              c("Mean of dependent variable", 
                round(mean(villData_reg$sell[complete.cases(villData_reg[,c("sell")])], na.rm = T),2),
                round(mean(villData_reg$sell[complete.cases(villData_reg[,c("sell", "area")])], na.rm = T),2),
                "-",
                "-"
              ),
              c("Std. deviation of dependent variable", 
                round(sd(villData_reg$sell[complete.cases(villData_reg[,c("sell")])], na.rm = T),2),
                round(sd(villData_reg$sell[complete.cases(villData_reg[,c("sell", "area")])], na.rm = T),2),
                "-",
                "-"
              )
              
            )
            
  )








# reg1km_cadRw_tex <- 
#   stargazer(reg1_rw,
#             reg2_rw,
#             title = "Number of Transctions - Simple difference (Rwamagana)",
#             covariate.labels = c("Connected (1km buffer intersection)",
#                                  "Average plot area"),
#             dep.var.labels   = c("Transference transactions (log)"),
#             omit.stat = c("f", "ser")
#   )


if (EXPORT_tables){
  write(reg1km_cad_tex,
        file = file.path(OUT_tables, "singleDif_cadTransactions.tex"))
  # write(reg1km_cadRw_tex,
  #       file = file.path(OUT_tables, "singleDif_cadTransactions_rwamagana.tex"))
}

#------------------------------------------------------------------------------#
#### P ####


data.frame(control = c(mean(villData_reg$sell[villData_reg$treated == 0]),
                       sd(villData_reg$sell[villData_reg$treated == 0]),
                       length(unique(villData_reg$vill_code_str[villData_reg$treated == 0]))),
           treatment = c(mean(villData_reg$sell[villData_reg$treated == 1]),
                         sd(villData_reg$sell[villData_reg$treated == 1]),
                         length(unique(villData_reg$vill_code_str[villData_reg$treated == 1]))),
           row.names = c("Mean", "Std. Deviation", "N"))




