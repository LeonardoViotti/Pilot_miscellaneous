#------------------------------------------------------------------------------#
#### Load packages ####

library(shiny)
library(plotly)
library(rsconnect)
library(ggmap)
library(zoo)
library(leaflet)

#------------------------------------------------------------------------------#
#### APP ####


prodVec <- as.character(prod_list$variable)
prodVec <- prodVec[order(prodVec)]
prodVec <- c(prodVec[grep("tomato", prodVec)[1]], 
             prodVec[-grep("tomato", prodVec)[1]])


#### UI
ui <- fluidPage(
  sidebarLayout( position = "right",
                 sidebarPanel(
                   selectInput(inputId = "prod_list",
                               label = "Products",
                               choices = prodVec)
                 ),
                 mainPanel(
                   plotOutput("tomato")
                 )
  )
)

server <- function(input, output) {}