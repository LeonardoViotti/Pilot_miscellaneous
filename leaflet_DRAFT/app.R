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


#------------------------------------------------------------------------------#
#### APP ####


prodVec <- as.character(prod_list$variable)
prodVec <- prodVec[order(prodVec)]
prodVec <- c(prodVec[grep("tomato", prodVec)[1]], 
             prodVec[-grep("tomato", prodVec)[1]])


# #### UI
# ui <- fluidPage(
#   sidebarLayout( position = "right",
#                  sidebarPanel(
#                    selectInput(inputId = "prod_list",
#                                label = "Products",
#                                choices = prodVec)
#                  ),
#                  mainPanel(
#                    plotOutput("tomato")
#                  )
#   )
# )


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
                
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  #Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    #markets@data[markets@data$year == 2018, ]
    #quakes[quakes$mag >= input$range[1] & quakes$mag <= input$range[2],]
    markets@data[, "product_selected"] <- markets@data[, as.character(input$prod_list)]
    markets
    
  })
  
  
  
  # This reactive expression represents the palette function,
  # which changes as the user makes selections in UI.
  # colorpal <- reactive({
  #   colorNumeric(input$colors, quakes$mag)
  # })
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>% 
        # addTiles(group = "OSM (default)") %>%
        
        # addProviderTiles("Esri.WorldGrayCanvas") %>% 
        # addProviderTiles("CartoDB.Positron") %>% 
        
        # addPolylines(data=district_rd,
        #              color = "blue",
        #              opacity = 1.0,
        #              weight = 1) %>%
        # 
        # addPolylines(data=national_rd,
        #              color = "blue",
        #              opacity = 1.0,
        #              weight = 1) %>%
      
      # Districts
      addPolygons(data=districts, 
                  color = "black",
                  fillColor = "grey",
                  weight = 1, 
                  smoothFactor = 0.5,
                  opacity = 0.2, 
                  fillOpacity = 0.55) %>%
      # Market dots
      addCircleMarkers(data=markets,
                       weight = 1,
                       radius = 2,
                       color = "firebrick",
                       fillOpacity = .9)
  })
  
  # Incremental changes to the map (in this case, replacing the
  # circles when a new color is chosen) should be performed in
  # an observer. Each independent set of things that can change
  # should be managed in its own observer.
  # observe({
  #   pal <- colorpal()
  #   
  #   leafletProxy("map", data = filteredData()) %>%
  #     clearShapes() %>%
  #     addCircles(radius = ~10^mag/10, weight = 1, color = "#777777",
  #                fillColor = ~pal(mag), fillOpacity = 0.7, popup = ~paste(mag)
  #     )
  # })
  
  observe({

      #if (max(markets@data[, as.character(input$prod_list)], na.rm = T) < 1000){
        leafletProxy("map", data = filteredData()) %>%
          #removeShape(layerId = 1:135) %>%
          clearGroup(group = "foo") %>%
          addCircles(
                   group= "foo",
                   #color = ~pal_pric(tomato), 
                   weight = 1,
                   #radius = ~tomato^1.31, 
                   radius = ~ product_selected^1.4 - product_selected^1.1, 
                   fillOpacity = 0.55) 
      # }  else if (max(markets@data[, as.character(input$prod_list)], na.rm = T) >= 1000 {
      #   leafletProxy("map", data = filteredData()) %>%
      #     #removeShape(layerId = 1:135) %>%
      #     clearGroup(group = "foo") %>%
      #     addCircles(
      #       group= "foo",
      #       #color = ~pal_pric(tomato),
      #       weight = 1,
      #       #radius = ~tomato^1.31,
      #       radius = ~product_selected^1.31,
      #       fillOpacity = 0.55)
      # }
    

  })
  # # Use a separate observer to recreate the legend as needed.
  # observe({
  #   proxy <- leafletProxy("map", data = quakes)
  #   
  #   # Remove any existing legend, and only if the legend is
  #   # enabled, create a new one.
  #   proxy %>% clearControls()
  #   if (input$legend) {
  #     pal <- colorpal()
  #     proxy %>% addLegend(position = "bottomright",
  #                         pal = pal, values = ~mag
  #    )
  #   }
  # })
}

shinyApp(ui, server)