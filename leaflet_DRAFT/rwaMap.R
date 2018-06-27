


markets	<- readOGR(MARKETS , "market_sample_abr18")
feeder_sample	<- readOGR(file.path(ROADS, "Sample"), "feeder_sample_abr18", pointDropZ = T)
district_rd		<- readOGR(ROADS, "District_Road_Class_1")
national_rd		<- readOGR(ROADS, "National_rds")


feeder_sample <- merge(feeder_sample, 
                       rms[, c("feeder_oid", "feeder_status", "completed")],
                       by.x = "OBJECTID",
                       by.y = "feeder_oid")

#tomato_merge <- read.csv(file.path(TEMP, "prices.csv"), header = T)
markets <- merge(markets, tomato_merge, by ="market_uid" )

# test variable
markets$foo <- rnorm(length(markets@data[,1]), mean = 1000, sd = 200)


pal_pric <- colorNumeric(c("blue", "red"), 
                    domain = markets$tomato)

pal_feed <- colorNumeric(c("limegreen", "gold", "firebrick"), 
                    domain = feeder_sample@data$feeder_status)

#CartoDB.VoyagerNoLabels, OpenMapSurfer.AdminBounds, JusticeMap.white
#Esri.WorldImagery

leaflet() %>%
  addProviderTiles("Esri.WorldImagery") %>% 
 # addTiles(group = "OSM (default)") %>%

 # addProviderTiles("Esri.WorldGrayCanvas") %>% 
 # addProviderTiles("CartoDB.Positron") %>% 
 #addProviderTiles("CartoDB.DarkMatter") %>% 
  
  
  # addPolylines(data=district_rd,
  #              color = "blue",
  #              opacity = 1.0,
  #              weight = 1) %>%
  # 
  # addPolylines(data=national_rd,
  #              color = "blue",
  #              opacity = 1.0,
  #              weight = 1) %>%
  
  addPolygons(data=districts, 
              color = "black",
              fillColor = "grey",
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 0, 
              fillOpacity = 0.55) %>%
  
  addCircleMarkers(data=markets,
             weight = 1,
             radius = 2,
             color = "firebrick",
             fillOpacity = .9) %>%
  
  addPolylines(data=feeder_sample[feeder_sample$feeder_status == 1,],
               color = ~pal_feed(feeder_status),
               group = "Completed",
               opacity = 1.0,
               weight = 2) %>%
  addPolylines(data=feeder_sample[feeder_sample$feeder_status == 2,],
               color = ~pal_feed(feeder_status),
               group = "Under construction",
               opacity = 1.0,
               weight = 2) %>%
  addPolylines(data=feeder_sample[feeder_sample$feeder_status == 3,],
               color = ~pal_feed(feeder_status),
               group = "Not started",
               opacity = 1.0,
               weight = 2) %>%
  addLayersControl(overlayGroups = c("Completed","Under construction", "Not started"),
                   options = layersControlOptions(collapsed = FALSE),
                   position = "bottomright") %>%
  
  addCircles(data=markets, 
             color = ~pal_pric(tomato), 
             weight = 1,
             radius = ~tomato^1.31, 
             fillOpacity = 0.55, 
             popup = ~paste(paste("Market:", market_nam), "\n",
                            paste("District:", district),
                            paste("Sector:",sector),
                            tomato, 
                            sep  = "\n"))
  
  #addLegendCustom(colors = c("blue", "blue", "red"), labels = c("A", "B", "C"), sizes = c(10, 20, 40))