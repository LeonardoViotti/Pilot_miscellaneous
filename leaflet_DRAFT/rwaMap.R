


markets	<- readOGR(MARKETS , "market_sample_abr18")
feeder_sample	<- readOGR(file.path(ROADS, "Sample"), "feeder_sample_abr18", pointDropZ = T)


tomato_merge <- read.csv(file.path(TEMP, "prices.csv"), header = T)
markets <- merge(markets, tomato_merge, by ="market_uid" )

# test variable
markets$foo <- rnorm(length(markets@data[,1]), mean = 1000, sd = 200)


pal_pric <- colorNumeric(c("blue", "red"), 
                    domain = markets$tomato)

pal_feed <- colorNumeric(c("green", "chocolate4"), 
                    domain = c(0,1))

#CartoDB.VoyagerNoLabels, OpenMapSurfer.AdminBounds, JusticeMap.white
#Esri.WorldImagery

leaflet() %>%
 # addProviderTiles("Esri.WorldImagery") %>% 
  addProviderTiles("Esri.WorldGrayCanvas") %>% 
 # addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(data=districts, 
              color = "black",
              fillColor = "grey",
              weight = 1, 
              smoothFactor = 0.5,
              opacity = 1.0, 
              fillOpacity = 0.55) %>%
  
  addCircles(data=markets,
             weight = 1,
             radius = 180,
             color = "firebrick",
             fillOpacity = .9) %>%
  
  addPolylines(data=feeder_sample,
               color = "forestgreen",
               opacity = 1.0,
               weight = 2) %>%
  
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