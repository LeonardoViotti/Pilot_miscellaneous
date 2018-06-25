

# Reprojecting
leafProj <-  CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0" )  # unprojected

markets 	    <- spTransform(markets	, leafProj)
districts	    <- spTransform(districts, leafProj)


imap_1 <- leaflet() %>%
  addProviderTiles("OpenStreetMap.Mapnik") %>% 
  addCircles(data=markets)