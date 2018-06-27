#------------------------------------------------------------------------------#
#### Load packages ####

library(shiny)
library(plotly)
library(rsconnect)
library(ggmap)
library(zoo)
library(leaflet)

#------------------------------------------------------------------------------#
#### Load data ####

#markets	<- readOGR(~ , "market_sample_abr18")
markets	<- readOGR(MARKETS , "market_sample_abr18")
feeder_sample	<- readOGR(file.path(ROADS, "Sample"), "feeder_sample_abr18", pointDropZ = T)
district_rd		<- readOGR(ROADS, "District_Road_Class_1")
national_rd		<- readOGR(ROADS, "National_rds")


#tomato_merge <- read.csv(file.path(TEMP, "prices.csv"), header = T)
markets <- merge(markets, tomato_merge, by ="market_uid" )

#------------------------------------------------------------------------------#
#### Data preparing ####

# Order product vector
prodVec <- as.character(prod_list$variable)
prodVec <- prodVec[order(prodVec)]
prodVec <- c(prodVec[grep("tomato", prodVec)[1]], 
             prodVec[-grep("tomato", prodVec)[1]])


#------------------------------------------------------------------------------#
#### APP ####


#### PAGE LAYOUT ####
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                selectInput(inputId = "prod_list",
                            label = "Products",
                            choices = prodVec),
                sliderInput("Time", "Range:",
                            min = 2016, max = 2018,
                            value = c(2017)),
                checkboxGroupInput("Roads_pane", h3("Feeder roads"),
                             choices = list("Not constructed" = "NC", 
                                            "Constructed" = "C",
                                            "Under construction" = "UC"),
                             selected = 1)
                
      )
)


#### SERVER ####
server <- function(input, output, session) {
  
  filteredData <- reactive({
    #markets@data[markets@data$year == 2018, ]
    #quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
    markets@data[, "product_selected"] <- markets@data[, as.character(input$prod_list)]
    markets@data[, "radious"] <- markets@data[, paste0("diff_", as.character(input$prod_list))]
    
    #markets <- markets[markets@data[, "year"] == input$Time, ]
    markets
  })
  
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>%
      #addProviderTiles("Esri.WorldImagery") %>% 
      addTiles(group = "OSM (default)") %>%
      
      # addProviderTiles("Esri.WorldGrayCanvas") %>% 
      # addProviderTiles("CartoDB.Positron") %>% 
      # addProviderTiles("CartoDB.DarkMatter") %>% 
      
      # Districts
      addPolygons(data=districts, 
                  color = "black",
                  fillColor = "grey",
                  weight = 1, 
                  smoothFactor = 0.5,
                  opacity = 0.2, 
                  fillOpacity = 0.25) %>%
      # Market dots
      addCircleMarkers(data=markets,
                       weight = 1,
                       radius = 2,
                       color = "firebrick",
                       fillOpacity = .9,
                       popup = ~paste(paste("Market:", market_nam),
                                      paste("District:", district),
                                      paste("Sector:", sector),
                                      paste("Cell:", cell),
                                      sep  = "<br>")
                       )
  })
  
  # Incremental changes to the map 
  observe({

    # Define color palet
    pal_pric <- colorNumeric(c("blue", "red"), 
                             domain = filteredData()$product_selected)
    
    leafletProxy("map", data = filteredData()) %>%
          #removeShape(layerId = 1:135) %>%
          clearGroup(group = "foo") %>%
          addCircles(
                   group= "foo",
                   color = ~pal_pric(product_selected), 
                   weight = 1,
                   #radius = ~tomato^1.31, 
                   radius = ~ (radious*150)^1.63, 
                   fillOpacity = 0.55,
                   popup = ~paste(paste("Market:", market_nam),
                                  paste("District:", district),
                                  paste("Sector:", sector),
                                  paste("Cell:", cell),
                                  paste("Price:", product_selected, "RWFs"), 
                                  sep  = "<br>"))

  })
  
  
  #### Add roads to map
  
  # observe({
  #     leafletProxy("map", data = filteredData()) %>%
  #       addPolylines(data=feeder_sample,
  #                    #color = ~pal_feed(feeder_sta),
  #                    opacity = 1.0,
  #                    weight = 2)
  # 
  # })
  
}

shinyApp(ui, server)