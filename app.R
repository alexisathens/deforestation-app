library(shiny)
library(tidyverse)
library(leaflet)
library(maptools)
library(Rcpp)
#old app.R
source('def_functions.R') #auxiliary R functions
sourceCpp('rcpp_functions.cpp') #auxiliary C++ functions
#library(lubridate)
#library(gridExtra)

coords <- data.frame(city_num=1:2,
                     city_name=c("Manuas","Porto Velho"),
                     city_id=c("manuas","velho"),
                     city_lat=c(-3.117034,-8.7666667),
                     city_long=c(-60.025780,-63.9))
points <- list()

ui <- fluidPage(
  
  titlePanel("Deforestation in the Amazon Basin"),
  headerPanel(h4("An interactive tool that predicts deforestation with respect to new infrastructure")),
  
  sidebarLayout(
    
    sidebarPanel(
      h4("Input:"),
      
      #selectInput("state",
       #           label="State",
        #          choices=c("Amazonas", #"Ghana 2014 (DHS)" = "GH6", 
         #                   "Rondônia")), #Brazilian states
      
      selectInput(inputId = "plot",
                  label="Plot selection",
                  choices=c("Manuas" = "manuas", "Porto Velho" = "velho"),
                  selected="manuas"),
      
      #selectInput("road",
       #           label="Road selection",
        #          choices=c("Vertical road at n=20" = "road1"),
         #         selected="road1"),
      
      #take clicks here!
      
      sliderInput("years",
                  label="Years to elapse:",
                  min=1,max=50,value=30) 
      
      #add comma back!!
      
      #checkboxInput("simulate", "Show simulation", value = TRUE), #show at different time steps
      
      #helpText("Please click on the map twice."),
      
      #actionButton("redraw", "Redraw"),
      
      #submitButton("Calculate...")
      
    ), #end sidebarPanel
    
    
    mainPanel(
      
      tabsetPanel(
        #tabPanel("Matrix", tableOutput("defPlot")) #assuming matrix form
        tabPanel("Plot", leafletOutput("map")),
        tabPanel("Summary Statistics", verbatimTextOutput("summary"))
      )
      
    ) #end mainPanel
    
  ) #end sidebarLayout
  
) #end ui


server <- function(input, output){ #session arg?
  
  # create a reactive value that will store the click position
  click_dat <- reactiveValues(clickedPoint=NULL)
  
  # store the click
  observeEvent(input$map_click,
               {
                 click_dat$clickedPoint <- input$map_click
               })
  
  output$map <- renderLeaflet({
    which(coords$city_id==input$plot)
    leaflet() %>%
      setView(lat = coords$city_lat[which(coords$city_id==input$plot)], 
              lng = coords$city_long[which(coords$city_id==input$plot)], 
              zoom = 8) %>% #8 is closest zoom w topo
      #addProviderTiles(provider = providers$Thunderforest.SpinalMap) #%>%
      addProviderTiles(provider = providers$Esri.WorldPhysical) %>% 
      addProviderTiles(provider = providers$Esri.WorldTopoMap,
                       options = providerTileOptions(opacity = .80)) #%>% 
     
    
  })
  
  output$summary <- renderTable( #renderText(
    isolate(reactiveValuesToList(click_dat))
      
    #c("Points: ")
    
  )
  
  
  #---------
  #eventReactive so that output is only updated when input$update is (user clicks the button)
 # datasetInput <- eventReactive(input$update, { })
  
  #output$defPlot <- renderTable({ #renderLeaflet
    #import data here
    
    #read shape files/csv
    #des_shape <- paste("data/", input$plot, "_shape.csv", sep = "")
    #shape <- read.csv(des_shape)
    
    #des_landscape <- paste("data/", input$plot, "_landscape.csv", sep = "")
    #landscape <- read.csv(des_landscape)
    
    ###user draws on road here
    #x=seq(from=1,to=30,length.out=20)
    #y=rep(15.5,20)
    #roads=data.frame(x=x,y=y) #20 points of the road
    
    #do calculations---
    #get distances from road, calculate once
    #n=nrow(shape)
    #distance=matrix(NA,n,n)
    #distance=distance_calculate(shape,roads)
    #i=matrix(NA,n,n)
    #b0=3
    #b1=-.05
    #b2=.02
    
    #for(t in 1:input$years)
    #{
      #get new neighboring probs
     # nbors=defnbors(shape,landscape)
      
      #get pi
    #  for(i in 1:n)
     # {
      #  for(j in 1:n)
       # {
        #  if(landscape[i,j]==0) #if forested
         # {
            #get deforestation probability
          #  tmp=exp(b0+b1*distance[i,j]+b2*nbors[i,j])
           # pi=tmp/(1+tmp)
            #landscape[i,j]=rbinom(1,size=1,prob=pi)
          #}
          
      #  }
    #  }
      
    #}
    
    
    #construct map
  #}) 
  #----------
  
} #end server

shinyApp(ui=ui, server=server)
