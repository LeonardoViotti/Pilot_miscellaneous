#------------------------------------------------------------------------------#

#			LVTP - Feeder roads analysis for CN

#------------------------------------------------------------------------------#

library(lfe)
library(stargazer)


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
#villages <- villages[villages@data$Shape_Area < quantile(villages@data$Shape_Area, .999),]
villages <- villages[villages@data$Shape_Area < 90e+06,]

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

#### Villages centroids
villages_ct_shape <- gCentroid(villages, byid = T)

villages_ct <- SpatialPointsDataFrame(villages_ct_shape, 
                                      data = villages@data,
                                      match.ID = FALSE)




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
#### SINGLE DIFFERENCE ####

### Create Data set ####
vill_cont@data$treated <- 0
vill_treat@data$treated <- 1

regVill_df <- rbind(vill_cont@data, vill_treat@data)

#### Regression Variables #####


#### Convert village area
regVill_df$Shape_AreaKM <- regVill_df$Shape_Area/1000000

#### Regressions ####

reg_jobs1 <- felm(log(N_jobs) ~ treated  | 0 | 0 | Code_vill_,
                  data = regVill_df )

reg_jobs2 <- felm(log(N_jobs) ~ treated + Shape_AreaKM | 0 | 0 | Code_vill_,
                  data = regVill_df )

reg_jobs3 <- felm(log(N_jobs) ~ treated + Shape_AreaKM + Population | 0 | 0 | Code_vill_,
                  data = regVill_df )


reg_busi1 <- felm(log(N_busi) ~ treated | 0 | 0 | Code_vill_,
                  data = regVill_df )

reg_busi2 <- felm(log(N_busi) ~ treated + Shape_AreaKM | 0 | 0 | Code_vill_,
                  data = regVill_df )

reg_busi3 <- felm(log(N_busi) ~ treated + Shape_AreaKM + Population  | 0 | 0 | Code_vill_,
                  data = regVill_df )


#--------------------------#
#### EXPORT REGRESSIONS ####


regbusi.tex <- stargazer(reg_busi1, 
                         reg_busi2, 
                         reg_busi3, 
                         title = "Number of Businesses - Simple Difference",
                         covariate.labels = c("Connected (1km buffer intersection)", 
                                              "Area (sq. Km)", 
                                              "Population",
                                              "Constant"),
                         dep.var.labels   = c("Businesses (log)"))


regjobs.tex <- stargazer(reg_jobs1, 
                         reg_jobs2, 
                         reg_jobs3, 
                         title = "Number of Jobs - Simple Difference",
                         covariate.labels = c("Connected (1km buffer intersection)", 
                                              "Area (sq. Km)", 
                                              "Population",
                                              "Constant"),
                         dep.var.labels   = c( "Jobs (log)"))

#reg_busi.tex <- stargazer(reg_busi)


write(regbusi.tex,
      file = file.path(OUT_tables, "singleDif_busi.tex"))
write(regjobs.tex,
      file = file.path(OUT_tables, "singleDif_jobs.tex"))
# write(reg_jobs.tex,
#       file = file.path(OUT_tables, "jobs_reg.tex"))


#------------------------------------------------------------------------------#
#### TRIPLE DIFFERENCE ####

#### Other buffers ####
nat.buf5 <- gBuffer(connect.buf, width = 4000)
nat.buf2 <- gBuffer(connect.buf, width = 1000)

#### Regression Variables ####

#### Buffer dummies
villages_ct@data$buf5 <- 0
villages_ct@data$buf5[gIntersects(villages_ct,
                                  gUnaryUnion(nat.buf5),
                                  byid = T) %>% as.logical] <- 1

villages_ct@data$buf2 <- 0
villages_ct@data$buf2[gIntersects(villages_ct,
                                  gUnaryUnion(nat.buf2),
                                  byid = T) %>% as.logical] <- 1

villages_ct@data$buf1fs <- 0
villages_ct@data$buf1fs[gIntersects(villages_ct,
                                  gUnaryUnion(fs.buf),
                                  byid = T) %>% as.logical] <- 1


villages_ct@data$conected <- 0
villages_ct@data$conected[gIntersects(villages_ct,
                                      gUnaryUnion(connect.buf),
                                      byid = T) %>% as.logical] <- 1

villages_ct@data$isolated <- 0
villages_ct@data$isolated[gIntersects(villages_ct,
                                      gUnaryUnion(isolated.buf),
                                      byid = T) %>% as.logical] <- 1

#### Subset datasets ####
analysis_vills_bol_2km <- 
  !(villages_ct@data$buf2==0 & 
      villages_ct@data$buf1fs  == 0)

analysis_vills_bol_5km <- 
  !(villages_ct@data$buf5 ==0 & villages_ct@data$buf1fs  == 0)


villages_ct2 <- subset(villages_ct, analysis_vills_bol_2km)
villages_ct5 <- subset(villages_ct, analysis_vills_bol_5km)


#### Regressions ####

# Business 2 km
reg2km_busi_1 <- felm(log(N_busi) ~ buf2 | 0 | 0 | Code_vill_,
                      data = villages_ct2 )
reg2km_busi_2 <- felm(log(N_busi) ~ buf1fs  | 0 | 0 | Code_vill_,
                      data = villages_ct2 )
reg2km_busi_3 <- felm(log(N_busi) ~ buf2 + buf1fs +  buf2*buf1fs -1 | 0 | 0 | Code_vill_,
                      data = villages_ct2 )

# Jobs 2 km
reg2km_jobs_1 <- felm(log(N_jobs) ~ buf2 | 0 | 0 | Code_vill_,
                      data = villages_ct2 )
reg2km_jobs_2 <- felm(log(N_jobs) ~ buf1fs  | 0 | 0 | Code_vill_,
                      data = villages_ct2 )
reg2km_jobs_3 <- felm(log(N_jobs) ~ buf2 + buf1fs +  buf2*buf1fs -1 | 0 | 0 | Code_vill_,
                      data = villages_ct2 )

foo <- lm(log(N_jobs) ~ buf2 + buf1fs +  buf2*buf1fs, 
          data = villages_ct2)

# Business 5 km
reg5km_busi_1 <- felm(log(N_busi) ~ buf5 | 0 | 0 | Code_vill_,
                      data = villages_ct5 )
reg5km_busi_2 <- felm(log(N_busi) ~ buf1fs  | 0 | 0 | Code_vill_,
                      data = villages_ct5 )
reg5km_busi_3 <- felm(log(N_busi) ~ buf5 + buf1fs +  buf5*buf1fs -1 | 0 | 0 | Code_vill_,
                      data = villages_ct5 )

# Jobs 5 km
reg5km_jobs_1 <- felm(log(N_jobs) ~ buf5 | 0 | 0 | Code_vill_,
                      data = villages_ct5 )
reg5km_jobs_2 <- felm(log(N_jobs) ~ buf1fs  | 0 | 0 | Code_vill_,
                      data = villages_ct5 )
reg5km_jobs_3 <- felm(log(N_jobs) ~ buf5 + buf1fs +  buf5*buf1fs -1 | 0 | 0 | Code_vill_,
                      data = villages_ct5 )

#### Export regressions ####
reg2km_busi_tex <- 
  stargazer(reg2km_busi_1,
            reg2km_busi_2,
            reg2km_busi_3,
            title = "Number of Businesses - Triple Difference: 2km Buffer",
            covariate.labels = c("Within 2km Intersection", 
                                 "Within 1km Feeder Road", 
                                 "Within 2km Intersection X Within 1km Feeder Road"),
            dep.var.labels   = c("Businesses (log)"),
            omit.stat = c("f", "ser")
            )
write(reg2km_busi_tex,
      file = file.path(OUT_tables, "tripleDiff_2km_busi.tex"))

reg2km_jobs_tex <- 
  stargazer(reg2km_jobs_1,
            reg2km_jobs_2,
            reg2km_jobs_3,
            title = "Number of Jobs - Triple Difference: 2km Buffer",
            covariate.labels = c("Within 2km Intersection", 
                                 "Within 1km Feeder Road", 
                                 "Within 2km Intersection X Within 1km Feeder Road"),
            dep.var.labels   = c("Jobs (log)"),
            omit.stat = c("f", "ser"))
write(reg2km_jobs_tex,
      file = file.path(OUT_tables, "tripleDiff_2km_jobs.tex"))

reg5km_busi_tex <- 
  stargazer(reg5km_busi_1,
            reg5km_busi_2,
            reg5km_busi_3,
            title = "Number of Businesses - Triple Difference: 5km Buffer",
            covariate.labels = c("Within 5km Intersection", 
                                 "Within 1km Feeder Road", 
                                 "Within 5km Intersection X Within 1km Feeder Road"),
            dep.var.labels   = c("Businesses (log)"),
            omit.stat = c("f", "ser"))
write(reg5km_busi_tex,
      file = file.path(OUT_tables, "tripleDiff_5km_busi.tex"))

reg5km_jobs_tex <- 
  stargazer(reg5km_jobs_1,
            reg5km_jobs_2,
            reg5km_jobs_3,
            title = "Number of Jobs - Triple Difference: 5km Buffer",
            covariate.labels = c("Within 5km Intersection", 
                                 "Within 1km Feeder Road", 
                                 "Within 5km Intersection X Within 1km Feeder Road"),
            dep.var.labels   = c("Jobs (log)"),
            omit.stat = c("f", "ser"))
write(reg5km_jobs_tex,
      file = file.path(OUT_tables, "tripleDiff_5km_jobs.tex"))



#------------------------------------------------------------------------------#
#### EXPORT SHAPEFILES ####



if (EXPORT_shapes){

  connect.buf <- as(connect.buf, "SpatialPolygonsDataFrame")
  isolated.buf <- as(isolated.buf, "SpatialPolygonsDataFrame")
  
  
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



#------------------------------------------------------------------------------#
#### AUXILIARY MAPS ####


# Buffers for plot
fs4.buf <- gBuffer(fs_simp[4,], width = 1000, byid = T)
nat15<- nat_roads[nat_roads$ROAD_NO == "RN 29",]
nat15.buf <- gBuffer(nat_roads[nat_roads$ROAD_NO == "RN 29",], width = 1000)

connect4.buf <- intersect(fs4.buf, nat.buf)

nat4.buf2 <- gBuffer(connect4.buf, width = 1000)







png(file.path(OUT_maps, "indentification.png"),
    units="in",
    res=300,
    width=5, 
    height=5)
  
  par(mfrow=c(2,2),
      cex.main = .7)


  # 1km buffer
  plot(fs.buf[4,], 
       col = rgb(red = 0, green = 0.7, blue = 0, alpha = 0.3), 
       border = NA)
  title("1km feeder buffer")
  
  plot(fs_simp[4,], col = "forestgreen", add = T)
  plot(nat15, col = "navyblue", add = T)
  
  
  
  # intersection buffer intersection
  plot(fs.buf[4,], 
       col = rgb(red = 0, green = 0.7, blue = 0, alpha = 0.3), 
       border = NA)
  
  title("Connected (1km buffer intersection)")
  
  plot(fs_simp[4,], col = "forestgreen", add = T)
  
  plot(nat15.buf, 
       border = "blue",
       add = T)
  
  plot(connect4.buf, 
       col = rgb(red = 1, green = 0, blue = 0, alpha = 0.3), 
       border = "red",
       add = T)
  
  plot(nat15, col = "navyblue", add = T)
  
  
  
  
  # plot 2
  plot(nat4.buf2,
       col = rgb(red = 0, green = 0, blue = 1, alpha = 0.3), 
       border = NA)
  title("2km intersecetion buffer")
  plot(fs.buf[4,],        
       col = rgb(red = 0, green = 0.7, blue = 0, alpha = 0.3), 
       border = NA, add = T)
  plot(connect4.buf, 
       col = rgb(red = 1, green = 0, blue = 0, alpha = 0.3), 
       border = NA,
       add = T)
  plot(fs_simp[4,], 
       col = "forestgreen",
       add = T)
  plot(nat15, col = "navyblue", add = T)
  
  
  
  # Legend
  plot(NULL ,xaxt='n',yaxt='n',bty='n',ylab='',xlab='', xlim=0:1, ylim=0:1)
  legend("center",
         #ncol=2,
         legend = c("1m buffer around feeder road",
                    "Connected (1km buffer intersection)",
                    "2km buffer around intersection", 
                    "1km buffer around national road", 
                    "Feeder Road", 
                    "National road"), 
         lty= c(0,0,0,0,1,1),
         col = c("forestgreen", 
                 "red", 
                 "blue", 
                 "blue", 
                 "forestgreen", 
                 "navyblue"),
         fill= c( rgb(red = 0, green = 0.7, blue = 0, alpha = 0.3), 
                  rgb(red = 1, green = 0, blue = 0, alpha = 0.3), 
                  rgb(red = 0, green = 0, blue = 1, alpha = 0.3), 
                  NA, 
                  NA, 
                  NA),
         border = c(NA,NA,NA,"blue",NA,NA),
         cex = .5)

dev.off()
