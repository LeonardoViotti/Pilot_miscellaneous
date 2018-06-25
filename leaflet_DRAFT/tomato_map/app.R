#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


#### Create choices
prodVec <- as.character(prod_list$variable)
prodVec <- prodVec[order(prodVec)]
prodVec <- c(prodVec[grep("tomato", prodVec)[1]], 
             prodVec[-grep("tomato", prodVec)[1]])


# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Tomato Map"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout( position = "right",
      sidebarPanel(
         selectInput(inputId = "prod_list",
                     label = "Products",
                     choices = prodVec)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         leafletOutput("tomap")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
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
    
  })

}

# Run the application 
shinyApp(ui = ui, server = server)

