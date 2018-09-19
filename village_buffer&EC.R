  #------------------------------------------------------------------------------#
  
  #	  LVTP - Corridor buffer and villages
  
  #------------------------------------------------------------------------------#


  # Porpuse: Create a X km buffer around corridor and calculate descriptive stats
  # with Establishment census and land cadastre data.


  #------------------------------------------------------------------------------#
  #### LOAD AND PROCESS DATA ####
  
  # Corridor
  cor <- readOGR(LVTP_shapeFiles, "Ngoma_Nyanza")
  
  # Districts
  districts <- readOGR(file.path(RFR_shapeFiles,"Districts"), "District_boundaries")
  nat_roads <- readOGR(file.path(RFR_shapeFiles,"Roads"), "National_rds")
  
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
  
  keep_vill_vars <- c("Code_vill_", 
                      "Village",
                      "District",
                      "Sector_1",
                      "Cellule_1",
                      "Population")
  villages <- villages[,keep_vill_vars]
  
  
  # EC
  ec <- read.dta13(file.path(EC_data, "2014/2014 Establishment Census.dta"))
  
  # Cadastre
  
  #-----------------#
  #### Reproject ####
  
  # Define projection
  RwaProj <-  CRS("+proj=aeqd +lat_0=-1.999036 +lon_0=29.95608") 
  
  # Reproject
  districts <- spTransform(districts, RwaProj)
  cor	<- spTransform(cor, RwaProj)
  villages <- spTransform(villages, RwaProj)
  nat_roads <- spTransform(nat_roads , RwaProj)
  
  #------------------------------------------------------------------------------#
  #### PROCESS EC DATA ####
  
  ec$Q20[ec$Q20 == 88888] <- NA
  
  
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
  ecVilldata$N_jobs <- jobsVill$Q20
  
  names(busiVill) <- c("Code_vill_", "N_busi")
  
  #------------------------------------------------------------------------------#
  #### BUSINESS IN VILL BUFFER ####
  
  villages <- merge(villages, busiVill, by = "Code_vill_")
  
  
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
  #### BUFFER ####
  
  cor.buf05 <- gBuffer(cor, width = 5000)
  cor.buf10 <- gBuffer(cor, width = 10000)
  
  
  #### Test plot
  
  png(file.path(OUT, "buffers_05km_and_10km.png"),
      width = 1000, height = 1000)
  
  plot(cor_districts)
  plot(cor.buf05, 
       col = rgb(red = 0, 
                 green = 0, 
                 blue = 1, 
                 alpha = 0.3),
       add = T)
  plot(cor.buf10, 
       col = rgb(red = 0, 
                 green = 1, 
                 blue = 0, 
                 alpha = 0.3),
       add = T)
  plot(cor, col = "red", lwd = 2, 
       add = T)
  
  dev.off()
  
  #------------------------------------------------------------------------------#
  #### VILLAGE BUFFER ####
  
  vill.buff	<- subset(cor_dist_vills,
                      gIntersects(cor_dist_vills,
                                  gUnaryUnion(cor.buf05),
                                  byid = T) %>% as.logical)
  

  
  

  # Aggregate number of employes per village 
  
  
  
  #------------------------------------------------------------------------------#
  #### ______MAPS_______ ####
  
  #### Reproject
  RwaProj_unp <-  CRS("+init=epsg:4326")  # unprojected
  
  
  districts <- spTransform(districts, RwaProj_unp)
  villages <- spTransform(villages, RwaProj_unp)
  
  nat_roads <- spTransform(nat_roads, RwaProj_unp)
  
  cor_districts <- spTransform(cor_districts, RwaProj_unp)
  cor	<- spTransform(cor, RwaProj_unp)
  #villages <- spTransform(villages, RwaProj)
  cor.buf10 <- spTransform(cor.buf10, RwaProj_unp)
  
  vill.buff <- spTransform(vill.buff, RwaProj_unp)
  
  ##### Transfor to dataframe
  districts.df <- tidy(districts)
  villages.df <- tidy(villages, region = "Code_vill_")
  villages.df <- merge(villages.df, villages, by.x = "id", by.y = "Code_vill_")
  
  vill.buff.df <- tidy(vill.buff, region = "Code_vill_")
  vill.buff.df <- merge(vill.buff.df, vill.buff, by.x = "id", by.y = "Code_vill_")
  
  
  cor_districts.df <- tidy(cor_districts)
  cor.df <- tidy(cor)
  cor.buf10.df <- tidy(cor.buf10)  
  
  nat_roads.df <- tidy(nat_roads)
  
  
  #------------------------------------------------------------------------------#
  #### LOAD BASEMAPS AND COUNTRY SHAPES ####
  
  # Country shapes
  rwa <- getData('GADM', country = 'RWA', level = 0)
  #bdi <- getData('GADM', country = 'BDI', level = 0)
  tza <- getData('GADM', country = 'TZA', level = 1)
  tza <- tza[6,]
  
  #bdi <- bdi[13,]
  
  # Basemaps 
  basemap_rwa <- get_map(location = c(lon=mean(districts.df$long), 
                                      lat=mean(districts.df$lat)), 
                         zoom= 8,
                         maptype="satellite") # roadmap, satellite, etc. See help(get_map)
  
  basemap_cor <- get_map(location = c(lon=mean(cor_districts.df$long), 
                                      lat=mean(cor_districts.df$lat)), 
                         zoom= 8,
                         maptype="satellite") # roadmap, satellite, etc. See help(get_map)
  
  #------------------------------------------------------------------------------#
  #### BUFFER MAP ####
  
  # District Labels
  cor_districts_ct <- gCentroid(cor_districts, byid = T)
  cor_districts_ct.df <- as.data.frame(coordinates(cor_districts_ct))
  
  cor_districts_ct.df$dist_name <- cor_districts@data$NOMDISTR
  names(cor_districts_ct.df) <- c("long", "lat", "dist_name")
  
  cor_districts_ct.df$dist_name <- tolower(cor_districts_ct.df$dist_name)
  cor_districts_ct.df$dist_name<- tools::toTitleCase(cor_districts_ct.df$dist_name)
  
  # Country lables
  country_labels <- data.frame(
    long = c(30.12, 30.20, 30.58),
    lat  = c(-1.73, -2.55, -2.58),
    name = c("RWANDA", "BURUNDI", "TANZANIA")
  )
  
  # Map
  buff_cols <- c("Corridor" = "firebrick",
                 "10km buffer" = "red",
                 "National roads" = "gold4")
  
  #buff_map <- 
    ggmap(basemap_cor) +
      #ggplot() + 
      
        geom_polygon(data = rwa, 
                     aes(y= lat, x = long, group = group),
                     fill= "gray58", 
                     alpha = 0.2, 
                     color = "black",  
                     size = .2) +
        geom_polygon(data = tza, 
                     aes(y= lat, x = long, group = group),
                     fill= "gray58", 
                     alpha = 0.00, 
                     color = "black",  
                     size = .7) +
        geom_polygon(data = cor_districts.df,
                     aes(y= lat, x = long, group = group),
                     fill= "gray87",
                     alpha = 0.2,
                     color = "black",
                     size = .7) +
      
      geom_path(data = nat_roads.df,
                aes(y= lat, 
                    x = long, 
                    group = group,
                    col = "National roads"),
                alpha = 0.8,
                size = 1) +
      
        geom_polygon(data = cor.buf10.df, 
                     aes(y= lat, 
                         x = long, 
                         group = group,
                         fill = "10km buffer"), 
                     alpha = 0.2,  
                     size = .1) +
        
        geom_text_repel(data = cor_districts_ct.df,
                        aes(y= lat, 
                            x = long, 
                            label = dist_name),
                        size = 7) +

        geom_path(data = cor.df,
                  aes(y= lat, 
                      x = long, 
                      group = group,
                      col = "Corridor"),
                  size = 1) +

        geom_text(data = country_labels,
                  aes(y= lat, 
                      x = long, 
                      label = name),
                  size = 7) + 
        
        theme(legend.text = element_text(size = 22),
              panel.background = element_blank(),
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(),
              legend.title=element_blank(),
              axis.title.x=element_blank(), 
              axis.title.y=element_blank()) + 
  
        coord_fixed(xlim=c(29.65, 30.75), ylim=c(-1.65, -2.75), ratio = 1) + 
      
        scale_colour_manual(values=buff_cols)  +
        scale_fill_manual(values=buff_cols) +
      
        ggsave(file.path(OUT, "Corridor_affected_districts.png"),
               width = 30, height = 30, units = "cm")
    
    
    
  #------------------------------------------------------------------------------#
  #### BUSINESSES HEAT MAP ####
  
  # Trim N business
  trim_level <- 30
  
  villages.df$N_busi_trim <- villages.df$N_busi
  villages.df$N_busi_trim[villages.df$N_busi_trim > trim_level] <- trim_level
  
  vill.buff.df$N_busi_trim <- vill.buff.df$N_busi
  vill.buff.df$N_busi_trim[vill.buff.df$N_busi_trim > trim_level] <- trim_level
  
  
  #### Rwanda ####
  #rwa_heatMap <- 
    ggplot()+
      geom_polygon(data = villages.df, 
                   aes(y= lat, 
                       x = long,
                       group = group,
                       fill = N_busi_trim )) +
     
    theme_void() +
    theme(legend.text = element_text(size = 11),
          #legend.title= element_text(size=22),
          legend.position = c(0.15, 0.8)) +
  
    scale_fill_gradientn(name ="Number of\nbusiness",
                         colours = viridis(10),
                         #palette = viridis_pal(),
                         labels = c("", "10", "20", "30+")) + 
    
    coord_quickmap() +
    
    ggsave(file.path(OUT, "Rwa_businesses_heatMAp.png"),
           width = 15, height = 15, units = "cm")
  
  
  #### Around corridor
  
  heat_cols <- c("Corridor" = "firebrick")
  
  #cor_heatMap <- 
    ggplot()+
      geom_polygon(data = vill.buff.df, 
                   aes(y= lat, 
                       x = long,
                       group = group,
                       fill = N_busi_trim )) +
      geom_path(data = cor.df,
                aes(y= lat, 
                    x = long,
                    group = group,
                    col = "Corridor"),
                size = 1) +
      theme_void() + 
    
      theme(legend.text = element_text(size = 11),
            legend.position = c(0.85, 0.2)) +

      scale_fill_gradientn(name ="Number of\nbusiness",
                           #colours=rev(heat.colors(10)),
                           colours = viridis(10),
                           #na.value="grey90",
                           labels = c("", "10", "20", "30+")
                          ) +
      scale_colour_manual(name = "",
                          values=heat_cols) +
      coord_quickmap() +
    
    
      ggsave(file.path(OUT, "Corridor_businesses_heatMAp.png"),
             width = 30, height = 15, units = "cm")
  
  #------------------------------------------------------------------------------#
  #### HEAT MAP ####
  
  # Rwanda
  
  # Around corriodor
  
  #### CADASTRE MAP EXAMPLE ####
  
  