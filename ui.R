#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(maps)
library(shiny)
library(leaflet)
library(tidyverse)
library(shinyjs)
library(plotly)
library(dygraphs)
library(shinyWidgets)


# Define UI for application that draws a histogram
shinyUI(fluidPage(
  fluidRow(
    navlistPanel(
# Distance/Landsize and Map  ----------------------------------------------
      tabPanel(icon=icon("area-chart"),
        pickerInput(
          inputId = "region", 
          label = "Click Here to Explore Regions", 
          choices = c('Eastern Metropolitan',
                      'Southern Metropolitan',
                      'Northern Metropolitan',
                      'South-Eastern Metropolitan',
                      'Western Metropolitan'),
          options = list(
            `actions-box` = TRUE, 
            size = 10,
            `selected-text-format` = "count > 3"), 
          multiple = TRUE,
          selected=NULL
        ),
        ' ',
# Region Content ----------------------------------------------------------
        br(),
        h1('Region Comparison and Exploration'),
        prettyRadioButtons(
          inputId = "type",
          label = "Choose a Type of Property", 
          choices = c("All", "Unit", "Townhouse", 'House'),
          icon = icon("check"), 
          inline =TRUE,
          bigger = TRUE,
          status = "info",
          animation = "jelly"
        ),
        'Properties characteristics differ in each region in Melbourne. This section lets you explore the differences in price, distance and landsize by using an interactive map. 
        The more yellow the region, the more expensive that region is on aggregate. Select multiple region and zoom out to see which one is more yellow.',
        br(),
        'To investigate the property, hover over the points to see the price of the property and click to show the address',

        # the map
        uiOutput('suburbTitle'),
        leafletOutput('map1'),
        hr(),
        
        h2('Filter Properties'),
        'By default, the map shows properties within 10 kilometers radius of the CBD. 
        Adjust the knob by scrolling or draggin and see how the color of each regions change. ',
        br(),
        'Increasing the distance in kilometers should show that the further away the properties the less expensive they are.',
        column(8, offset=4, 
          uiOutput('Knobs'),
          materialSwitch(
            inputId = "knobswitch",
            label = "Filter with Distance",
            value = TRUE, 
            status = "info"
          )
        ),
        # knob for landsize or distance
        br()
      ),
# Suburb Comparison/Breakdown ---------------------------------------------
      tabPanel(icon=icon("filter"),
        # multi select suburbs 
        uiOutput('suburbSelector'),
        titlePanel('Suburb Exploration and Comparison'),
        'This panel allows you to explore and compare different suburbs. 
        Select different suburbs from the panel to the left. 
        One thing to note is that the yellow color on the map reflects 
        both how many expensive properties as well as how expensive a small subset of properties are.',
        br(),
        'The selected are Richmond and Bentleigh East, which suburb seems to be more expensive?',
        br(),
        leafletOutput('suburbMap'),
        br(),
        
        'The following graph shows the distribution of the prices of the properties in the suburbs you have selected. 
        It is clearer than the map to compare their price profile as the sparsity of the properties do not affect the intensity of color',
        br(),
        'Selected are Richmond and Bentleigh East, which one looks more expensive from the map and how does their price profile differ?',
        br(),
        plotOutput('suburb_profile'),
        br(),
        # timeline of year built
        
        'The median price of the suburbs selected are shown in the following time series,
        How have their prices change on aggregate?',
        dygraphOutput('prop_count'),
        br()
      ),
# Property Breakdown ------------------------------------------------------
      tabPanel(uiOutput('priceSlider'), icon=icon("money"),
      uiOutput('priceTitle'),
      'This section shows you the profile of properties within a price range.
      Different price range shows the different characteristics of properties.',
      br(),
      'The following piechart breaks down the type of properties that are
      in this price range. the default $100000-$500000 price range mainly consist of units.
      Adjust the price range using the slider on the left to see how property types change with price.',
      plotOutput('price_compo'),
      'The following piechart shows the number of rooms this price range will
      likely to have. The default range shows 1-3 rooms are most common within the price range.',
      plotOutput('rooms_compo'),
      'This piechart shows the regions in which properties within the selected price range are located.
      The regions from which you can get a property priced within that range changes according to your input.',
      plotOutput('region_compo')
      )
    )
  )
))
