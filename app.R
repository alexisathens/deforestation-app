library(rsconnect)
library(shiny)
library(ggplot2)
#rsconnect::deployApp()
library(tidyverse)
library(leaflet)
library(maptools)
library(Rcpp)
library(DT) #output data frames
#source('def_aux.R') #auxiliary R functions
#sourceCpp('rcpp_functions.cpp') #auxiliary C++ functions

coords <- data.frame(city_num=1:2,
                     city_name=c("Manaus","Porto Velho"),
                     city_id=c("manaus","velho"),
                     city_lat=c(-3.117034,-8.7666667),
                     city_long=c(-60.025780,-63.9))

points_df <- data.frame("lat"=NA,"long"=NA)

ui <- fluidPage(
  
  titlePanel("Deforestation in the Amazon Basin"),
  headerPanel(h4("Interactive tool that predicts deforestation with respect to new infrastructure")),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Input:"),
      
      #selectInput("state",
      #           label="State",
      #          choices=c("Amazonas", #"Ghana 2014 (DHS)" = "GH6", 
      #                   "Rondônia")), #Brazilian states
      
      selectInput(inputId = "plot",
                  label="Plot selection",
                  choices=c("Manaus" = "manaus", "Porto Velho" = "velho"),
                  selected="manaus"),
      
      #selectInput("road",
      #           label="Road selection",
      #          choices=c("Vertical road at n=20" = "road1"),
      #         selected="road1"),
      
      sliderInput("years",
                  label="Years to elapse:",
                  min=1,max=50,value=30),
      
      textInput("pt1.lat", label = h5("Point 1 Lat Coords")), #, value = "Lat coords"
      textInput("pt1.long", label = h5("Point 1 Long Coords")),
      textInput("pt2.lat", label = h5("Point 2 Lat Coords")),
      textInput("pt2.long", label = h5("Point 2 Long Coords")),
      
      hr(), #horizontal rule (line)
      fluidRow(column(3, verbatimTextOutput("value"))),
      
      
      #checkboxInput("simulate", "Show simulation", value = TRUE), #show at different time steps
      
      helpText("Draw points to connect a road on the map.")
      
      #actionButton("redraw", "Redraw"),
      
      #submitButton("Calculate")
      
    ), #end sidebarPanel
    
    
    mainPanel(
      
      tabsetPanel(
        #tabPanel("Matrix", tableOutput("defPlot")) #assuming matrix form
        tabPanel("Plot", leafletOutput("map")),
        tabPanel("Summary Statistics", tableOutput("summary")), #uiOutput("summary")),
        tabPanel("Binary Matrix...", tableOutput("binary")) #output matrix or df? #verbatimTextOutput("info")
      )
      
    ) #end mainPanel
    
  ) #end sidebarLayout
  
) #end ui


server <- function(input, output){ #session arg?
  
  #want to be able to store multiple user clicks (coords)
  #draw lines onto UI
  #user can select "redraw" or "submit"
  #"submit" sends coords to server
  
  
  # create a reactive value that will store the click position
  click_dat <- reactiveValues(clickedPoint=NULL)
  
  # store the click
  observeEvent(input$map_click,
               {
                 click_dat$clickedPoint <- input$map_click
               })
  
  output$summary <- renderTable ({ #renderUI({
    click_dat <- unlist(click_dat$clickedPoint)
    lat <- click_dat[3]
    long <- click_dat[2]
    
    points_df <- rbind(points_df, c(lat, long))
    
    #new_point <- sprintf("<strong>New Location:</strong> <br> Lat: %f <br> Lon: %f",
    #                    lat, long) %>%
    #lapply(htmltools::HTML)
    
    points_df[-1,] #get rid of NA col
    #new_point
  })
  
  
  
  #points input works
  output$binary <- renderTable({
    #get user points and calculate
    #eventually will extract from map clicks...
    pt1 <- data.frame(lat=input$pt1.lat,long=input$pt1.long)
    pt2 <- data.frame(lat=input$pt2.lat,long=input$pt2.long)
    pts <- rbind(pt1,pt2)
    pts
  })
  
  
  
  #output map!
  output$map <- renderLeaflet({
    which(coords$city_id==input$plot)
    leaflet() %>%
      setView(lat = coords$city_lat[which(coords$city_id==input$plot)], 
              lng = coords$city_long[which(coords$city_id==input$plot)], 
              zoom = 8) %>% #8 is closest zoom w topo
      #addProviderTiles(provider = providers$Thunderforest.SpinalMap) #%>%
      #addProviderTiles(provider = providers$Esri.WorldPhysical) %>% 
      #addProviderTiles(provider = providers$Esri.WorldTopoMap,
      #options = providerTileOptions(opacity = .80)) #%>%
      addProviderTiles(provider = providers$Esri.WorldTopoMap)
    
    
  })
  
} #end server

shinyApp(ui=ui, server=server)
