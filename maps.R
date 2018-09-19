  #------------------------------------------------------------------------------#
  
  #	  LVTP - MAPs
  
  #------------------------------------------------------------------------------#
  
  library(ggplot2)
  library(ggmap)
  library(broom)
  library(ggrepel)
  library(raster)
  
  
  #------------------------------------------------------------------------------#
  #### Tidy ####
  
  #### Reproject
  RwaProj_unp <-  CRS("+init=epsg:4326")  # unprojected
  
  
  districts <- spTransform(districts, RwaProj_unp)
  villages <- spTransform(villages, RwaProj_unp)
  
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
    long = c(30.12, 30.20, 30.75),
    lat  = c(-1.73, -2.55, -2.55),
    name = c("RWANDA", "BURUNDI", "TANZANIA")
  )
  
  # Map
  ggmap(basemap_cor) +
    geom_polygon(data = rwa, 
                 aes(y= lat, x = long, group = group),
                 fill= "gray58", 
                 alpha = 0.04, 
                 color = "black",  
                 size = .2) +
    geom_polygon(data = tza, 
                 aes(y= lat, x = long, group = group),
                 fill= "gray58", 
                 alpha = 0.04, 
                 color = "black",  
                 size = .2) +
    geom_polygon(data = cor_districts.df, 
                 aes(y= lat, x = long, group = group),
                 fill= "gray58", 
                 alpha = 0.2, 
                 color = "black",  
                 size = .2) +
    geom_polygon(data = cor.buf10.df, 
                 aes(y= lat, x = long, group = group),
                 fill= "blue", 
                 alpha = 0.4,  
                 size = .1) +
    
    geom_text_repel(data = cor_districts_ct.df,
                    aes(y= lat, x = long, label = dist_name)) +
    
    geom_text(data = country_labels,
              aes(y= lat, x = long, label = name)) + 
    
    geom_path(data = cor.df,
              aes(y= lat, x = long, group = group),
              size = 1,
              col = "firebrick") +
  
    
    coord_fixed(xlim=c(29.5, 31), ylim=c(-1.5, -3), ratio = 1) +
    
    

  
  
  #------------------------------------------------------------------------------#
  #### BUSINESSES HEAT MAP ####
  
  # Trim N business
  trim_level <- 30
  
  villages.df$N_busi_trim <- villages.df$N_busi
  villages.df$N_busi_trim[villages.df$N_busi_trim > trim_level] <- trim_level
  
  vill.buff.df$N_busi_trim <- vill.buff.df$N_busi
  vill.buff.df$N_busi_trim[vill.buff.df$N_busi_trim > trim_level] <- trim_level
  
  
  # EDIT LEGEND TO +50!!!! 
  
  #### Rwanda ####
  ggplot()+
    geom_polygon(data = villages.df, 
                 aes(y= lat, 
                     x = long,
                     group = group,
                     fill = N_busi_trim )) +
    scale_fill_gradientn(colours=rev(heat.colors(10)),
                         na.value="grey90")
    
  
  
  #### Around corridor
  ggplot()+
    geom_polygon(data = vill.buff.df, 
                 aes(y= lat, 
                     x = long,
                     group = group,
                     fill = N_busi_trim )) +
    geom_path(data = cor.df,
              aes(y= lat, x = long, group = group),
              size = 1,
              col = "blue") +
  
    scale_fill_gradientn(colours=rev(heat.colors(10)),
                         na.value="grey90")
  
  
  #------------------------------------------------------------------------------#
  #### JOBS HEAT MAP ####
  
  #### Rwanda ####
  
  #### Around corridor
  
  #------------------------------------------------------------------------------#
  #### CADASTER EXAMPLE MAP ####