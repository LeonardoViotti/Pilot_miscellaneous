  #------------------------------------------------------------------------------#
  
  #	  LVTP - 
  
  #------------------------------------------------------------------------------#
  
  
  # Porpuse:
  # with Establishment census and 
  
  
  
  #------------------------------------------------------------------------------#
  #### LOAD DATA ####
  
  cor <- readOGR(LVTP_shapeFiles, "Ngoma_Nyanza")
  
  # Districts
  districts <- readOGR(file.path(RFR_shapeFiles,"Districts"), "District_boundaries")
  nat_roads <- readOGR(file.path(RFR_shapeFiles,"Roads"), "National_rds")
  
  
  # Villages
  villages <- readOGR(file.path(RFR_shapeFiles,"Villages"), "processed_villages_boundaries")
  
  #### Remove data from large villages ####
  large_vills <- 
    #villages$Code_vill_[villages@data$Shape_Area > quantile(villages@data$Shape_Area, .99975)]
    villages$Code_vill_[villages@data$Shape_Area > 90e+06]
  
  
  
  # Remove large villages
  #villages <- villages[villages@data$Shape_Area < quantile(villages@data$Shape_Area, .99),]
  
  keep_vill_vars <- c("Code_vill_", 
                      "Village",
                      "District",
                      "Sector_1",
                      "Cellule_1",
                      "Population",
                      "Shape_Area")
  villages <- villages[,keep_vill_vars]
  
  # EC
  ec2014 <- read.dta13(file.path(EC_data, "2014/2014 Establishment Census.dta"))
  ec2011 <- read.dta13(file.path(EC_data, "2011/Establishment Census Merge.dta"))
  
  
  # Cadastre
  
  #-----------------#
  #### Reprojec2014t ####
  
  # Define projec2014tion
  RwaProj <-  CRS("+proj=aeqd +lat_0=-1.999036 +lon_0=29.95608") 
  
  # Reprojec2014t
  districts <- spTransform(districts, RwaProj)
  cor	<- spTransform(cor, RwaProj)
  villages <- spTransform(villages, RwaProj)
  cells <- spTransform(cells, RwaProj)
  nat_roads <- spTransform(nat_roads , RwaProj)
  
  
  #------------------------------------------------------------------------------#
  #### PROCESS EC 2011 DATA ####
  
  ec2011$Total_emp1[ec2011$Total_emp1 == 999999] <- NA
  
  
  # Transforme province code
  
  # ec2011$ID1_new <- NA
  # ec2011$ID1_new[KIGALI City == "KIGALI City"] <- 1
  # ec2011$ID1_new[ec2011$ID1 == "SOUTHERN"] <- 2
  # ec2011$ID1_new[ec2011$ID1 == "WESTERN"]  <- 3
  # ec2011$ID1_new[ec2011$ID1 == "NORTHERN"] <- 4
  # ec2011$ID1_new[ec2011$ID1 == "ESTHERN"]  <- 5
  
  # Village code
  ec2011$dist_code <- (ec2011$ID1*10) + ec2011$ID2
  ec2011$sect_code <- (ec2011$dist_code*100) + ec2011$ID3
  ec2011$cell_code <- (ec2011$sect_code*100) + ec2011$ID4
  ec2011$vill_code <- (ec2011$cell_code*100) + ec2011$ID5
  
  # Aggregate number of business by village
  ec2011Busi <- aggregate(ID ~ vill_code,
                          data = ec2011,
                          FUN = length)
  
  ec2011Jobs <- aggregate(Total_emp1 ~ vill_code,
                          data = ec2011,
                          na.action = NULL,
                          FUN = function(x) sum(x, na.rm = T))
  
  
  ec2011Vill <- ec2011Busi
  ec2011Vill$N_jobs <- ec2011Jobs$Total_emp1
  
  names(ec2011Vill) <- c("Code_vill_", "N_busi", "N_jobs")
  
  
  ec2011Vill$N_busi[ec2011Vill$Code_vill_ %in% large_vills] <- NA
  ec2011Vill$N_jobs[ec2011Vill$Code_vill_ %in% large_vills] <- NA
  
  #------------------------------------------------------------------------------#
  #### PROCESS EC 2014 DATA ####
  
  ec2014$Q20[ec2014$Q20 == 88888] <- NA
  
  
  # Transforme province code
  
  ec2014$ID1_new <- NA
  ec2014$ID1_new[ec2014$ID1 == "KIGALI CITY"]       <- 1
  ec2014$ID1_new[ec2014$ID1 == "SOUTHERN PROVINCE"] <- 2
  ec2014$ID1_new[ec2014$ID1 == "WESTERN PROVINCE"]  <- 3
  ec2014$ID1_new[ec2014$ID1 == "NORTHERN PROVINCE"] <- 4
  ec2014$ID1_new[ec2014$ID1 == "ESTHERN PROVINCE"]  <- 5
  
  # Village code
  ec2014$dist_code <- (ec2014$ID1_new*10) + ec2014$ID2
  ec2014$sect_code <- (ec2014$dist_code*100) + ec2014$ID3
  ec2014$cell_code <- (ec2014$sect_code*100) + ec2014$ID4
  ec2014$vill_code <- (ec2014$cell_code*100) + ec2014$ID5
  
  # Aggregate number of business by village
  ec2014Busi <- aggregate(key ~ vill_code,
                      data = ec2014,
                      FUN = length)
  
  ec2014Jobs <- aggregate(Q21C1 ~ vill_code,
                      data = ec2014,
                      FUN = sum)
  
  
  ec2014Vill <- ec2014Busi
  ec2014Vill$N_jobs <- ec2014Jobs$Q21C1
  
  names(ec2014Vill) <- c("Code_vill_", "N_busi", "N_jobs")
  
  
  ec2014Vill$N_busi[ec2014Vill$Code_vill_ %in% large_vills] <- NA
  ec2014Vill$N_jobs[ec2014Vill$Code_vill_ %in% large_vills] <- NA
  
  
  
  #------------------------------------------------------------------------------#
  #### SUBSETS ####
  
  cor_districts_listing <- 
    c("NYANZA",
      "BUGESERA",
      "NGOMA") 
  
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
  #### BUFFERs ####
  
  
  cor.buf01 <- gBuffer(cor, width = 1000)
  cor.buf02 <- gBuffer(cor, width = 2000)
  cor.buf03 <- gBuffer(cor, width = 3000)
  cor.buf04 <- gBuffer(cor, width = 4000)
  cor.buf05 <- gBuffer(cor, width = 5000)
  
  
  
  vill.buff1	<- subset(cor_dist_vills,
                       gIntersects(cor_dist_vills,
                                   gUnaryUnion(cor.buf01),
                                   byid = T) %>% as.logical)
  vill.buff2	<- subset(cor_dist_vills,
                       gIntersects(cor_dist_vills,
                                   gUnaryUnion(cor.buf02),
                                   byid = T) %>% as.logical)
  vill.buff3	<- subset(cor_dist_vills,
                       gIntersects(cor_dist_vills,
                                   gUnaryUnion(cor.buf03),
                                   byid = T) %>% as.logical)
  vill.buff4	<- subset(cor_dist_vills,
                       gIntersects(cor_dist_vills,
                                   gUnaryUnion(cor.buf04),
                                   byid = T) %>% as.logical)
  vill.buff5	<- subset(cor_dist_vills,
                       gIntersects(cor_dist_vills,
                                   gUnaryUnion(cor.buf05),
                                   byid = T) %>% as.logical)
  
  png(filename = "C:/Users/WB519128/Desktop/vills05.png",
      width = 800, height = 800)
  
  plot(vill.buff5)
  plot(cor, col = "red", lwd = 2, add =T)
  
  dev.off()
  
  #------------------------------------------------------------------------------#
  #### Regression variables ####
  
  villages$buff1 <- 0
  villages$buff2 <- 0
  villages$buff3 <- 0
  villages$buff4 <- 0
  villages$buff5 <- 0
  
  villages$buff1[gIntersects(villages,
                            gUnaryUnion(cor.buf01),
                            byid = T) %>% as.logical] <- 1
                
                      
  villages$buff2[gIntersects(villages,
                             gUnaryUnion(cor.buf02),
                             byid = T) %>% as.logical] <- 1
  
  villages$buff3[gIntersects(villages,
                             gUnaryUnion(cor.buf03),
                             byid = T) %>% as.logical] <- 1
  
  villages$buff4[gIntersects(villages,
                             gUnaryUnion(cor.buf04),
                             byid = T) %>% as.logical] <- 1
  
  villages$buff5[gIntersects(villages,
                             gUnaryUnion(cor.buf05),
                             byid = T) %>% as.logical] <- 1
  
  
  #### Drop villages not within 5km of corridor
  
  vill5 <- villages[villages$buff5 == 1,]
  
  #### Treatment definitions for 5 cenarios ####
  
  # Cenario 1
  vill5$c1_treat <- vill5$buff1
  
  vill5$c1_contr <- 0
  vill5$c1_contr[vill5$buff1 == 0] <- 1
  
  # Cenario 2
  vill5$c2_treat <- vill5$buff2
  
  vill5$c2_contr <- 0
  vill5$c2_contr[vill5$buff2 == 0] <- 1
  
  # Cenario 3
  vill5$c3_treat <- vill5$buff3
  
  vill5$c3_contr <- 0
  vill5$c3_contr[vill5$buff3 == 0] <- 1
  
  # Cenario 4
  vill5$c4_treat <- vill5$buff1
  
  vill5$c4_contr <- 0
  vill5$c4_contr[vill5$buff3 == 0] <- 1
  

  
  #------------------------------------------------------------------------------#
  #### Merge EC data with villages ####
  
  villages_11 <- merge(vill5, ec2011Vill, by = "Code_vill_")
  villages_14 <- merge(vill5, ec2014Vill, by = "Code_vill_")
  
  
  villages_11$year <- 2011
  villages_14$year <- 2014
  
  
  reg_vill_df <- rbind(villages_11, villages_14)
  
  
  #### Dummies and interactions
  
  reg_vill_df$d2011 <- 0 
  reg_vill_df$d2011[reg_vill_df$year == 2011] <- 1 
  
  reg_vill_df$d2014 <- 0 
  reg_vill_df$d2014[reg_vill_df$year == 2014] <- 1 
  
  #### Remove villages within 2 and 3km for c4
  reg_vill_df_c4 <- reg_vill_df[!(reg_vill_df$buff2 == 1 & reg_vill_df$buff1 == 0 ),]
  
  #------------------------------------------------------------------------------#
  #### Regressions ####
  
  
  # Cenario 1
  reg_c1 <- felm(inv_hsine(N_busi) ~ c1_treat * d2014 | 0 | 0 | Code_vill_ ,
                 data = reg_vill_df)
  
  # Cenario 2
  reg_c2 <- felm(inv_hsine(N_busi) ~ c2_treat * d2014 | 0 | 0 | Code_vill_ ,
                 data = reg_vill_df)
  # Cenario 3
  reg_c3 <- felm(inv_hsine(N_busi) ~ c3_treat * d2014 | 0 | 0 | Code_vill_ ,
                 data = reg_vill_df)
  # Cenario 4
  reg_c4 <- felm(inv_hsine(N_busi) ~ c4_treat * d2014 | 0 | 0 | Code_vill_ ,
                 data = reg_vill_df_c4)

  
  
  reg_c1_tex <- 
    stargazer(reg_c1,
              title = "Number of Businesses - Cenario 1",
              covariate.labels = c("Within 1km", 
                                   "Year 2014", 
                                   "Within 1km X Year 2014"),
              dep.var.labels  = c("Businesses (log)"),
              table.placement = "H",
              #column.labels = c("Cenario 1", "Cenario 2", "Cenario 3", "Cenario 4"), 
              omit.stat = c("f", "ser"))
  
  write(reg_c1_tex,
        file = file.path(OUT_tables, "corEC_c1_busi.tex"))
  
  
  reg_c2_tex <- 
    stargazer(reg_c2,
              title = "Number of Businesses - Cenario 2",
              covariate.labels = c("Within 2km", 
                                   "Year 2014", 
                                   "Within 2km X Year 2014"),
              dep.var.labels  = c("Businesses (log)"),
              table.placement = "H",
              #column.labels = c("Cenario 1", "Cenario 2", "Cenario 3", "Cenario 4"), 
              omit.stat = c("f", "ser"))
  
  write(reg_c2_tex,
        file = file.path(OUT_tables, "corEC_c2_busi.tex"))
  
  
  reg_c3_tex <- 
    stargazer(reg_c3,
              title = "Number of Businesses - Cenario 3",
              covariate.labels = c("Within 3km", 
                                   "Year 2014", 
                                   "Within 3km X Year 2014"),
              dep.var.labels  = c("Businesses (log)"),
              table.placement = "H",
              #column.labels = c("Cenario 1", "Cenario 2", "Cenario 3", "Cenario 4"), 
              omit.stat = c("f", "ser"))
  
  write(reg_c3_tex,
        file = file.path(OUT_tables, "corEC_c3_busi.tex"))
  
  
  reg_c4_tex <- 
    stargazer(reg_c4,
              title = "Number of Businesses - Cenario 1",
              covariate.labels = c("Within 1km", 
                                   "Year 2014", 
                                   "Within 1km X Year 2014"),
              dep.var.labels  = c("Businesses (log)"),
              table.placement = "H",
              #column.labels = c("Cenario 1", "Cenario 2", "Cenario 3", "Cenario 4"), 
              omit.stat = c("f", "ser"))
  
  write(reg_c4_tex,
        file = file.path(OUT_tables, "corEC_c4_busi.tex"))
  