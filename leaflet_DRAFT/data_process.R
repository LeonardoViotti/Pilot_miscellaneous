
  #------------------------------------------------------------------------------#
  
  #				RFR - Shiny map base code
  
  #------------------------------------------------------------------------------#
  
  ## PURPOSE: Prepare data to shiny dashboard
  
  ## Written by:	                                        Leonardo Teixeira Viotti
  
  ## Last updated:                                                        JUN 2018
  
  
  cat(paste( "This code does not depend on a master to run."
             , sep = "\n"))
  
  #------------------------------------------------------------------------------#
  #### Settings ####
  #### Delete everything already in R memory
  # (Equivalent to clear all in Stata)
  rm(list = ls())
  
  #-------------------------#
  #### 1. Load packages ####
  
  # Spacial
  library(sp)
  library(sf)
  library(rgdal)
  library(rgeos)
  library(maptools)
  library(raster)
  library(broom)
  library(ggmap)
  library(leaflet)
  
  
  # Others
  library(plyr)
  library(tidyverse)
  library(reshape2)
  library(readstata13)
  library(stringr)
  library(gridExtra)
  library(zoo)
  
  
  #------------------------------------------------------------------------------#
  #### SWITCHES ####
  
  EXPORT_data = 1
  
  #------------------------------------------------------------------------------#
  #### 2. FILE PATHS 	####
  
  #### 2.1. Dropbox and GitHub paths	####
  
  # Leonardo
  if (Sys.getenv("USERNAME") == "wb519128" | Sys.getenv("USERNAME") == "WB519128"){
    DROPBOX <- file.path("C:/Users/WB519128/Dropbox/Work/WB/Rwanda Feeder Roads")
    GITHUB  <- file.path("C:/Users/WB519128/Documents/GitHub")
    
  }
  
  # Leonardo Laptop
  if (Sys.getenv("USERNAME") == "Leonardo" ){
    DROPBOX <- file.path("C:/Users/Leonardo/Dropbox/Work/WB/Rwanda Feeder Roads")
    GITHUB  <- file.path("C:/Users/Leonardo/Documents/GitHub")
    
  }
  
  # Maria
  if (Sys.getenv("USERNAME") == "wb357411"){
    DROPBOX <- file.path("C:/Users/wb357411/Dropbox/Rwanda Feeder Roads")
    #GITHUB  <- file.path("")
    
  }
  
  #### 2.2. Folder paths ####
  
  DATA       			     	  <- file.path(DROPBOX, "data/surveys")
  MASTER     			     	  <- file.path(DROPBOX, "data/master")
  SAMPLE     			     	  <- file.path(DROPBOX, "data/sample")
  GIS				 			        <- file.path(DROPBOX, "data/gis/Shapefiles")
  HH_SV	     			        <- file.path(DATA,  "HH survey/HH_panel")
  PRICE_SV 			        	<- file.path(DATA,  "price survey")
  SECOND    			        <- file.path(DATA,  "secondary (eicv)")
  UBUDEHE   			        <- file.path(DATA,  "source_data/Ubudehe DATA 2012")
  ESTAB_CENSUS            <- file.path(DROPBOX, "data/admin/establishment_census_data/Establishment Census dataset 2011_2014")
  
  CODES		 			        	<- file.path(GITHUB)
  
  OUTPUTS   			        <- file.path(DROPBOX, "Analysis/Outputs")
  
  COD_tables 			        <- file.path(CODES, "Tables")
  COD_graphs 			        <- file.path(CODES, "Graphs")
  
  OUT_graphs 	        		<- file.path(OUTPUTS, "Graphs")
  OUT_maps 		  	        <- file.path(OUTPUTS, "Maps")
  OUT_tables 			        <- file.path(OUTPUTS, "Tex/Tables")
  OUT_tables_regression 	<- file.path(OUTPUTS, "Tex/Tables/Regression")
  
  
  
  #### 2.3. Shape Paths
  
  DISTRICTS	<- file.path(GIS, "Districts")
  MARKETS 	<- file.path(GIS, "Markets")
  ROADS 	  <- file.path(GIS, "Roads")
  
  
  #### 2.4. Dashboard path
  TEMP <- file.path(GITHUB, "Pilot_miscellaneous/leaflet_DRAFT")
  
  #------------------------------------------------------------------------------#
  #### Load Data ####
  
  #### Shapefiles
  
  districts <- readOGR(DISTRICTS, "District_boundaries")
  #	sector	<- readOGR(BOUNDARIES, "Sector_Boundary_2012")
  #	cells 	<- readOGR(BOUNDARIES, "Cell_Boundary_2012")
  #roads		  <- readOGR(ROADS, "All_roads_Nov2016")
  
  markets	<- readOGR(MARKETS , "market_sample_abr18")
  # markets	  <- readOGR(MARKETS , "All_markets_July")
  # ps_markets<- readOGR(MARKETS , "Price survey")
  # ml_markets<- readOGR(MARKETS , "Market Listing")
  
  feeder_sample	<- readOGR(file.path(ROADS, "Sample"), "feeder_sample_abr18", pointDropZ = T)
  district_rd		<- readOGR(ROADS, "District_Road_Class_1")
  national_rd		<- readOGR(ROADS, "National_rds")
  
  #### Price Survey
  ps		<- read.dta13(file.path(PRICE_SV, "Price_survey_preprocess.dta"), convert.factors = F)
  
  #### Product list
  prod_list <- read.csv(file.path(PRICE_SV, "product_list.csv"), header = T)
  
  
  #### Product list
  rms	 	<- read.dta13(	file.path(DATA, "Road Monitoring/Data/master_monitoring_objectid&status.dta"), convert.factors = F)
  
  #remove ag inptus
  prod_list <- prod_list[prod_list$type != "input",]
  
  
  #------------------------------------------------------------------------------#
  #### Get everybody in the same projection ####
  
  leafProj <-  CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" )  # unprojected
  
  markets 	    <- spTransform(markets	, leafProj)
  districts	    <- spTransform(districts, leafProj)
  
  district_rd	 	<- spTransform(district_rd	, leafProj)
  national_rd 	<- spTransform(national_rd	, leafProj)
  feeder_sample	<- spTransform(feeder_sample, leafProj)
  
  #------------------------------------------------------------------------------#
  #### Average prices between traders ####
  
  #### Function to average between traders
  avgTr_fun <-  function(x){
    
    pat1 <- NULL  
    pat1 <- paste("w_std_price", x, "1", sep = "_")
    pat2 <- NULL  
    pat2 <- paste("w_std_price", x, "2", sep = "_")  
    
    bool <- NULL
    bool <- (grepl(pat1, names(ps)) |
               grepl(pat2, names(ps))  )
    
    rowMeans(ps[,bool], na.rm = T)
  }
  
  #### Loop trough all products
  avgPrices <- 
    sapply(as.character(prod_list$variable), 
           avgTr_fun)
  
  avgPrices <- as.data.frame(avgPrices)
  #names(avgPrices) <- prod_list$variable
  
  #### Include time and ID variables
  avgPrices$market_uid <- ps$market_uid
  avgPrices$year_month <- ps$year_month
  
  
  #------------------------------------------------------------------------------#
  #### MERGE DATA ####
  
  #### Price survey ####
  
  #### PS data processing to merge
  # Since we have a panel data, i.e. with multiple obs. for the same market, we need
  # to collapse the data somehow into one line per market. Here I'm just averaging, but
  # you could keep the last month for example.
  
  
  #### CHANGE THIS TO APP ####
  avgPrices[avgPrices$year_month < 201806 & avgPrices$year_month > 201802, ]
  
  
  #### Market level data
  
  # Average by market
  tomato_merge <- aggregate( . ~ market_uid,
                             FUN = mean, na.rm = T,
                             na.action=NULL,
                             data = avgPrices)
  
  
  
  
  # Above or bellow average var
  #tomato_merge$avg_diff <- tomato_merge$avg_tomato - mean(tomato_merge$avg_tomato)
  
  
  # Rempove NaNs
  tomato_merge <- as.data.frame(apply(tomato_merge, 
                                      2, 
                                      function(x) ifelse(is.nan(x), NA, x ) ) )
  
  # Turn everything to numericagain
  tomato_merge <- as.data.frame(apply(tomato_merge, 
                                      2, 
                                      function(x) as.numeric(x)) )
  
  # Calculate difference from average
  tomato_diff <- as.data.frame(apply(tomato_merge, 
                                     2, 
                                     function(x){ (x)/ mean(x, na.rm = T)}))
  
  names(tomato_diff) <- paste0("diff_", names(tomato_diff))
  
  # Combine average and diff
  tomato_merge <- cbind(tomato_merge, tomato_diff)
  
  
  #### Merge with shape-file
  #markets <- merge(markets, tomato_merge, by ="market_uid" )
  
  
  #### Road monitoring ####
  # feeder_sample <- merge(feeder_sample, 
  #                        rms[, c("feeder_oid", "feeder_status", "completed")],
  #                        by.x = "OBJECTID",
  #                        by.y = "feeder_oid")
  
  
  #------------------------------------------------------------------------------#
  #### EXPORT DATA ####
  
  if (EXPORT_data == 1){
    
    # Prices
    write.table(tomato_merge, 
                file.path(TEMP, "prices.csv"),
                sep = ",")
    # 
    # writeOGR(markets,
    #          dsn=file.path(TEMP, "markets.shp"),
    #          layer="sample_markets",
    #          overwrite_layer=T,
    #          driver="ESRI Shapefile")
    # 
    # writeOGR(districts,
    #          dsn=file.path(TEMP, "districts.shp"),
    #          layer="districts",
    #          overwrite_layer=T,
    #          driver="ESRI Shapefile")
    # 
    # #### Feeders shape
    # names(feeder_sample) <- strtrim(names(feeder_sample), 10)
    # 
    # writeOGR(feeder_sample,
    #          dsn=file.path(TEMP, "feeder_sample.shp"),
    #          layer="feeder_sample",
    #          overwrite_layer=T,
    #          driver="ESRI Shapefile")
    
  }
