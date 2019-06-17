#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(leaflet.extras)
library(dygraphs)
library(xts)
library(RColorBrewer)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  # set current directory to the script executing this code
  cwd <- dirname(rstudioapi::getActiveDocumentContext()$path)
  setwd(cwd)
  
  # read the dataset
  house <- read.csv('./housing.csv')
  

#  (UI) Suburb Selector ---------------------------------------------------------

  output$suburbSelector <- renderUI ({
    type <- switch(input$type,
                   'Unit'= 'u', 
                   'Townhouse' = 't', 
                   'House'= 'h',
                   'All' = levels(factor(house$Type)))

    
    data <- house %>% 
      filter(Type %in% c(type))
    
    # sorting suburbs by their price
    suburb <- house %>% 
      group_by(Suburb) %>% 
      summarize(count = n()) %>% 
      arrange(desc(count))
    
    multiInput(
      inputId = "suburb",
      label = "Click Here to Explore Suburbs", 
      choices = NULL,
      choiceNames = lapply(seq_along(suburb$Suburb), 
                           function(i) tagList(suburb$Suburb[i])),
      choiceValues = suburb$Suburb
    )
  })
  

#  (UI) Knob  -------------------------------------------------------------------

  
  output$Knobs <- renderUI({
    if (input$knobswitch == TRUE) 
      knobInput(
        inputId = "distance",
        label = "Within Distance(Km) from CBD:",
        value = 10,
        min = 1,
        max=max(house$Distance),
        displayPrevious = TRUE, 
        lineCap = "round",
        fgColor = "#428BCA",
        inputColor = "#428BCA"
      )
    else
      knobInput(
        inputId = "landsize",
        label = "With Landsize Greater than:",
        value = 0,
        min = 5,
        max=max(house$Landsize),
        displayPrevious = TRUE, 
        lineCap = "round",
        fgColor = "#428BCB",
        inputColor = "#428BCB"
      )
  })
  

# (UI)Region Title ------------------------------------------------------------
  output$suburbTitle <- renderUI({
    titlePanel(toString(input$region))
  })
  

 # (UI) Price Slider ------------------------------------------------------------

  output$priceSlider <- renderUI({
    sliderInput('slider',
                'Click to Explore Properties by Price Range',
                width='100%',
                min=100000,
                max=4000000,
                step=100000,
                value=c(100000, 500000))
  })
  

# (UI) Price Title -----------------------------------------------------------

  output$priceTitle <- renderUI({
    titlePanel(paste0('Properties Priced Between $', input$slider[1], '-$', input$slider[2]))
  })


# (Map) Region Map --------------------------------------------------------------
  
  # render map
  m <- leaflet(options = leafletOptions(preferCanvas = TRUE)) %>%
    addProviderTiles(providers$Esri.WorldTopoMap)
  
  output$map1 <- renderLeaflet({
    # get the suburb to filter the data
    type <- switch(input$type,
                   'Unit'= 'u', 
                   'Townhouse' = 't', 
                   'House'= 'h',
                   'All' = levels(factor(house$Type)))
  
    if (input$knobswitch == TRUE) 
      house <- house %>% 
        drop_na() %>%
        filter(Distance < input$distance) %>% 
        filter(Regionname %in% if(is.null(input$region)) levels(factor(house$Regionname)) else input$region) %>% 
        filter(Type %in% c(type))
    
    else
      house <- house %>% 
        filter(Regionname %in% if(is.null(input$region)) levels(factor(house$Regionname)) else input$region) %>% 
        filter(Type %in% c(type)) %>% 
        filter(Landsize > input$landsize)
        
    radius <- findInterval(house$Price,c(100000, 200000, 300000, 500000, 1000000, 1300000, 1700000, 2000000)) *10
    
    m %>% 
      setView(lng = 144.98, lat = -37.84 , zoom=11) %>% 
      addCircles(data=house, ~Longtitude, ~Lattitude,
                 radius, 
                 stroke = T, 
                 color = 'Green',
                 popup = ~paste(as.character(Address), ',',Suburb), 
                 label = ~paste0('$', as.character(Price))) %>% 
      addCircleMarkers(data=house, ~Longtitude, ~Lattitude,
                       ~(Price^(1/2))/10^2.95,
                       stroke = F,
                       color='Yellow',
                       popup = ~paste(as.character(Address), ',',Suburb), 
                       label = ~paste0('$', as.character(Price))) %>%
      fitBounds(lng1 = max(house$Longtitude)-0.001,lat1 = max(house$Lattitude)-0.001,
                lng2 = min(house$Longtitude)+0.001,lat2 = min(house$Lattitude)+0.001)

  })
  
# (Map) Suburb Map --------------------------------------------------------------
  
  output$suburbMap <- renderLeaflet({

    # get the suburb to filter the data
    type <- switch(input$type,
                   'Unit'= 'u', 
                   'Townhouse' = 't', 
                   'House'= 'h',
                   'All' = levels(factor(house$Type)))
    
    if(is.null(input$suburb)) 
      suburb_data <- house %>% 
        filter(Suburb %in% c('Richmond', 'Bentleigh East'))
    else
      suburb_data <- house %>% 
        filter(Suburb == as.character(input$suburb))
    
    radius <- findInterval(suburb_data$Price,c(300000, 1000000, 1300000, 1700000, 2000000)) *30
    
    m %>% 
      setView(lng = 144.98, lat = -37.84 , zoom=11) %>% 
      addCircles(data=suburb_data, ~Longtitude, ~Lattitude,
                 radius, 
                 stroke = T, 
                 color = 'Green',
                 popup = ~paste(as.character(Address), ',',Suburb), 
                 label = ~paste0('$', as.character(Price))) %>% 
      addCircleMarkers(data=suburb_data, ~Longtitude, ~Lattitude,
                       ~sqrt(Price)/10^2.4,
                       stroke = F,
                       color='Yellow',
                       popup = ~paste(as.character(Address), ',',Suburb), 
                       label = ~paste0('$', as.character(Price))) %>%
      fitBounds(lng1 = max(suburb_data$Longtitude),lat1 = max(suburb_data$Lattitude)+0.001,
                lng2 = min(suburb_data$Longtitude)+0.001,lat2 = min(suburb_data$Lattitude))
    
  })


# (Density Plot) Suburb Price Distribution -----------------------------------------------
  
  output$suburb_profile <- renderPlot({
    type <- switch(input$type,
                   'Unit'= 'u', 
                   'Townhouse' = 't', 
                   'House'= 'h',
                   'All' = levels(factor(house$Type)))
    
    if(is.null(input$suburb)) 
      data <- house %>% 
        filter(Suburb %in% c('Richmond', 'Bentleigh East'))
    else
      data <- house %>% 
        filter(Suburb == input$suburb)
    
    ggplot(data, aes(Price, group=Suburb, color=Suburb)) + 
      geom_density(alpha=0.2) +
      scale_x_continuous(labels = scales::comma) + 
      labs(title = paste("Distribution of Prices", toString(input$suburb)))
      
  })
  
# (Time Series) Suburb Time Series ------------------------------------------------------
  
  output$prop_count <- renderDygraph({
    type <- switch(input$type,
                   'Unit'= 'u', 
                   'Townhouse' = 't', 
                   'House'= 'h',
                   'All' = levels(factor(house$Type)))
    
    if(is.null(input$suburb)) 
      data <- house %>% 
        filter(Suburb == c('Richmond', 'Bentleigh East'))
      else
      data <- house %>% 
        filter(Suburb == as.character(input$suburb))
    
    priceTrend <- data %>% 
      group_by(Date, Date) %>% 
      summarize_at(vars(Price), funs(median(., na.rm=TRUE))) %>% 
      mutate(Date = as.Date(.$Date, '%Y-%m-%d')) %>% 
      xts(order.by = .$Date)
      
    # convert to date object.
    dygraph(priceTrend, paste('Median Price of Properties in', toString(ifelse(is.null(input$suburb), toString(c('Richmond', 'Bentleigh East')), toString(input$suburb))))) %>% 
      dySeries("Price", label = "Price") %>%
      dyOptions(drawPoints = TRUE) %>% 
      dyRangeSelector()
  })
  

# (Pie) Type Compo Piechat ------------------------------------------------------

  output$price_compo <- renderPlot({
    
    data <- house %>% 
      filter(between(Price, input$slider[1], input$slider[2])) %>% 
      group_by(Type) %>% 
      summarize(count=n())
      
    ggplot(data, aes('', y=count, fill=Type)) + 
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)  +
    labs(x = NULL, y = NULL, fill = NULL, title = "Type of Properties")+
      theme_classic() + theme(axis.line = element_blank(),
                              axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              plot.title = element_text(hjust = 0.5, color = "#666666"))
    
  })
  

# (Pie) Rooms Compo Piechart ----------------------------------------------------


  output$rooms_compo <- renderPlot({
    
    data <- house %>% 
      filter(between(Price, input$slider[1], input$slider[2])) %>% 
      mutate(Rooms = as.character(Rooms)) %>% 
      filter(Rooms %in% c('1', '2', '3', '4', '5', '6')) %>% 
      group_by(Rooms) %>% 
      summarize(count=n())
    
    ggplot(data, aes('', y=count, fill=Rooms)) + 
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0) + 
      labs(x = NULL, y = NULL, fill = NULL, title = "Numbers of Rooms They Have")+
      theme_classic() + theme(axis.line = element_blank(),
                              axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              plot.title = element_text(hjust = 0.5, color = "#666666"))
    
  })
  

# (Pie) Region Compo Piechart ---------------------------------------------------

  output$region_compo <- renderPlot({
    
    data <- house %>% 
      filter(between(Price, input$slider[1], input$slider[2])) %>% 
      group_by(Regionname) %>% 
      summarize(count=n())
    
    ggplot(data, aes('', y=count, fill=Regionname)) + 
      geom_bar(width = 1, stat = "identity", color = "white") +
      coord_polar("y", start = 0)  +
      labs(x = NULL, y = NULL, fill = NULL, title = "Regions They Are In")+
      theme_classic() + theme(axis.line = element_blank(),
                              axis.text = element_blank(),
                              axis.ticks = element_blank(),
                              plot.title = element_text(hjust = 0.5, color = "#666666"))
  })
})
