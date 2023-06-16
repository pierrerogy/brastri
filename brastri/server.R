#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(viridis)
library(cowplot)
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(here)
source(here::here("brastri",
                  "functions.R"))


# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Read in the prepared data 
    ## Bromeliads
    bromeliads <-
      readr::read_csv(here::here("brastri", "data",
                                   "bromeliad_data.csv")) %>% 
      ## Keep experimental bromeliads only
      dplyr::filter(stringr::str_detect(string = bromeliad_id, patter = "E")) %>% 
      ## Remove the columns not needed
      dplyr::select(-contains(c("_g", "prop", "actual", "site", "_mm")))
    
    ## Water chemistry
    water <-
      readr::read_csv(here::here("brastri", "data",
                                 "water_data.csv")) %>% 
      ## Make day date
      dplyr::mutate(day = lubridate::dmy(day)) %>% 
      ## Remove tap
      dplyr::filter(bromeliad_id != "tap")
    
    ## Aquatic communities
    community <-
      readr::read_csv(here::here("brastri", "data",
                                 "community_data.csv")) %>% 
      ## Remove ci columns and convert biomass data
      dplyr::mutate(dry_mass_mg = ifelse(is.na(dry_mass_mg),
                                         biomass_mg, dry_mass_mg)) %>% 
      dplyr::select(-contains("ci"), - biomass_mg, -size_used_mm, -bwg_name)

    emergence <-
      readr::read_csv(here::here("brastri", "data",
                                 "emergence_data.csv"))

    # Make reactive datasets (subset data for each plot depending on selection) ---------------------------------------------------
    # Plot 1
    plot1_dats <- reactive({
        
        ## Get data
        ret <- 
            get_those_dats(
              x = input$x1, 
              bromeliads = bromeliads, 
              communities = communities, 
              water = water, 
              emergence = emergence)
        
        ## Return data
        return(ret)
        
    })
    
    # Plot 2
    plot2_dats <- reactive({
        
      ## Get data
      ret <- 
        get_those_dats(
          x = input$x2,  
          bromeliads = bromeliads, 
          communities = communities, 
          water = water, 
          emergence = emergence)
      
      ## Return data
      return(ret)
        
    })
    
    # Plot 3
    plot3_dats <- reactive({
      
      ## Get data
      ret <- 
        get_those_dats(
          y = input$y3, 
          bromeliads = bromeliads, 
          communities = communities, 
          water = water, 
          emergence = emergence)
      
      ## Return data
      return(ret)
      
        })
    
    # Plot 3
    plot4_dats <- reactive({
      
      ## Get data
      ret <- 
        get_those_dats(
          y = input$y4, 
          bromeliads = bromeliads, 
          communities = communities, 
          water = water, 
          emergence = emergence)
      
      ## Return data
      return(ret)
        })
    
    
    # Make plots --------------------------------------------------------------
    # Plot 1
    output$plot1 <- renderPlot({
        density1 <-
          ggplot() + 
          theme_void()

        # IF a data object exists, update the blank ggplot.
        # basically this makes it not mess up when nothing is selected
         if(nrow(plot1_dats()) > 1){
           density1 <-
             density_plot(dats = plot1_dats(),
                          x  = input$x1)

         }
        
        # Print the plot
        density1

    })
    
    # Plot 2
    output$plot2 <- renderPlot({
      density2 <-
        ggplot() + 
        theme_void()
      
      # IF a data object exists, update the blank ggplot.
      # basically this makes it not mess up when nothing is selected
      if(nrow(plot2_dats()) > 1){
        density2 <-
          density_plot(dats = plot2_dats(),
                       x  = input$x2)
        
      }
      
      # Print the plot
      density2
      
    })
    
    # Plot 3
    output$plot3 <- renderPlot({
      line3 <-
        ggplot() + 
        theme_void()
      
      # IF a data object exists, update the blank ggplot.
      # basically this makes it not mess up when nothing is selected
      if(nrow(plot3_dats()) > 1){
        line3 <-
          time_plot(dats = plot3_dats(),
                       y  = input$y3)
        
      }
      
      # Print the plot
      line3
      
    })
    
    # Plot 4
    output$plot4 <- renderPlot({
      line4 <-
        ggplot() + 
        theme_void()
      
      # IF a data object exists, update the blank ggplot.
      # basically this makes it not mess up when nothing is selected
      if(nrow(plot4_dats()) > 1){
        line4 <-
          time_plot(dats = plot4_dats(),
                    y  = input$y4)
        
      }
      
      # Print the plot
      line4
      
    })
    
    
    
    
})
