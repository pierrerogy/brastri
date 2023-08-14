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
      readr::read_csv(here::here("brastri", 
                                 "data",
                                   "bromeliad_data.csv")) %>% 
      ## Keep experimental bromeliads only
      dplyr::filter(stringr::str_detect(string = bromeliad_id, 
                                        pattern = "E")) %>% 
      ## Remove the columns not needed
      dplyr::select(-contains(c("_g", "prop", "actual", "site", "_mm")))
    
    ## Water chemistry
    water <-
      readr::read_csv(here::here("brastri", 
                                 "data",
                                 "water_data.csv")) %>% 
      ## Make day date
      dplyr::mutate(day = lubridate::dmy(day)) %>% 
      ## Remove tap
      dplyr::filter(bromeliad_id != "tap")
    
    ## Aquatic communities
    communities <-
      readr::read_csv(here::here("brastri", 
                                 "data",
                                 "community_data.csv")) %>% 
      ## Remove ci columns and convert biomass data
      dplyr::select(-contains("ci"),  -size_used_mm, -bwg_name)
    
    ## Emergence
    emergence <-
      readr::read_csv(here::here("brastri", 
                                 "data",
                                 "emergence_data.csv"))
    
    ## Table 1 with model water chemistry outputs
    table1 <-
      readr::read_csv(here::here("brastri", 
                          "data",
                                 "table_1.csv")) %>% 
      data.frame(row.names = 1) %>% 
      make_names_nicer()
  
    ## Table 2 with model outputs
    table2 <- 
      readr::read_csv(here::here("brastri", 
                                 "data",
                                 "table_2.csv")) %>% 
      data.frame(row.names = 1) %>% 
      make_names_nicer()
    
    ## Table 3 with number of individual seeded, caught as adult, number of larvae found
    ### a
    table3a <- 
      readr::read_csv(here::here("brastri", 
                                 "data",
                                 "table_3a.csv")) %>% 
      data.frame(row.names = 1) %>% 
      make_names_nicer()
    ### b
    table3b <- 
      readr::read_csv(here::here("brastri", 
                                 "data",
                                 "table_3b.csv")) %>% 
      data.frame(row.names = 1) %>% 
      make_names_nicer()
    
    ## Table 4 with model outputs
    table4 <- 
      readr::read_csv(here::here("brastri", 
        "data",
        "table_4.csv")) %>% 
      data.frame(row.names = 1) %>% 
      make_names_nicer()
    
    ## Table 5 with model outputs
    table5 <- 
      readr::read_csv(here::here("brastri", 
        "data",
        "table_5.csv")) %>% 
      data.frame(row.names = 1) %>% 
      make_names_nicer()
    
    ## Table 6 with model outputs
    table6 <- 
      readr::read_csv(here::here("brastri", 
        "data",
        "table_6.csv")) %>% 
      data.frame(row.names = 1) %>% 
      make_names_nicer()
    
    ## Table 7 with model outputs
    table7 <- 
      readr::read_csv(here::here("brastri", 
                                 "data",
                                 "table_7.csv")) %>% 
      data.frame(row.names = 1) %>% 
      make_names_nicer()
    
    ## Table 7 with model outputs
    table8 <- 
      readr::read_csv(here::here("brastri", 
                                 "data",
                                 "table_8.csv")) %>% 
      data.frame(row.names = 1) %>% 
      make_names_nicer()
    
    ## Table 7 with model outputs
    table9 <- 
      readr::read_csv(here::here("brastri", 
                                 "data",
                                 "table_9.csv")) %>% 
      data.frame(row.names = 1) %>% 
      make_names_nicer()
    
  

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
    
    # Plot 4
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
    
    # Plot 5
    plot5_dats <- reactive({
      
      ## Get data
      ret <- 
        get_those_dats(
          x = input$x5, 
          bromeliads = bromeliads, 
          communities = communities, 
          water = water, 
          emergence = emergence)
      
      ## Return data
      return(ret)
    })
    
    # Plot 6
    plot6_dats <- reactive({
      
      ## Get data
      ret <- 
        get_those_dats(
          x = input$x6, 
          bromeliads = bromeliads, 
          communities = communities, 
          water = water, 
          emergence = emergence)
      
      ## Return data
      return(ret)
    })
    
    
    # Make plots and tables --------------------------------------------------------------
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
    
    # Plot 5
    output$plot5 <- renderPlot({
      density5 <-
        ggplot() + 
        theme_void()
      
      # IF a data object exists, update the blank ggplot.
      # basically this makes it not mess up when nothing is selected
      if(nrow(plot5_dats()) > 1){
        density5 <-
          size_histogram(dats = plot5_dats(),
                       x  = input$x5)
      }
      
      # Print the plot
      density5
      
    })
    
    # Plot 6
    output$plot6 <- renderPlot({
      density6 <-
        ggplot() + 
        theme_void()
      
      # IF a data object exists, update the blank ggplot.
      # basically this makes it not mess up when nothing is selected
      if(nrow(plot6_dats()) > 1){
        density6 <-
          size_histogram(dats = plot6_dats(),
                       x  = input$x6)
      }
      
      # Print the plot
      density6
      
    })

    # Table 1
    output$table1 <- renderTable(
      {table1}, 
      striped = TRUE, bordered = TRUE,  
      hover = TRUE, spacing = 'l',  
      width = '100%', align = 'c',  
      rownames = TRUE,  
      digits = 3, na = '',
      sanitize.text.function=identity) ## last row to read html
    
    # Table 2
    output$table2 <- renderTable(
      {table2}, 
      striped = TRUE, bordered = TRUE,  
      hover = TRUE, spacing = 'l',  
      width = '100%', align = 'c',  
      rownames = TRUE,  na = '',
      digits = 3, 
      sanitize.text.function=identity) 
    
    # Table 3a
    output$table3a<- renderTable(
      {table3a}, 
      striped = TRUE, bordered = TRUE,  
      hover = TRUE, spacing = 'l',  
      width = '100%', align = 'c',  
      rownames = TRUE, na = '', digits =0,
      sanitize.text.function=identity)
    
    # Table 3b
    output$table3b<- renderTable(
      {table3b}, 
      striped = TRUE, bordered = TRUE,  
      hover = TRUE, spacing = 'l',  
      width = '100%', align = 'c',  
      rownames = TRUE, na = '', digits =0,
      sanitize.text.function=identity)
    
    # Table 4
    output$table4 <- renderTable(
      {table4}, 
      striped = TRUE, bordered = TRUE,  
      hover = TRUE, spacing = 'l',  
      width = '100%', align = 'c',  
      rownames = TRUE,  
      digits = 3, na = '',
      sanitize.text.function=identity) ## last row to read html
    
    # Table 5
    output$table5 <- renderTable(
      {table5}, 
      striped = TRUE, bordered = TRUE,  
      hover = TRUE, spacing = 'l',  
      width = '100%', align = 'c',  
      rownames = TRUE,  na = '',
      digits = 3, 
      sanitize.text.function=identity) 
    
    # Table 6
    output$table6 <- renderTable(
      {table6}, 
      striped = TRUE, bordered = TRUE,  
      hover = TRUE, spacing = 'l',  
      width = '100%', align = 'c',  
      rownames = TRUE,  na = '',
      digits = 3, 
      sanitize.text.function=identity) 
    
    # Table 7
    output$table7 <- renderTable(
      {table7}, 
      striped = TRUE, bordered = TRUE,  
      hover = TRUE, spacing = 'l',  
      width = '100%', align = 'c',  
      rownames = TRUE,  na = '',
      digits = 3, 
      sanitize.text.function=identity) 
    
    # Table 8
    output$table8 <- renderTable(
      {table8}, 
      striped = TRUE, bordered = TRUE,  
      hover = TRUE, spacing = 'l',  
      width = '100%', align = 'c',  
      rownames = TRUE,  na = '',
      digits = 3, 
      sanitize.text.function=identity) 
    
    # Table 9
    output$table9 <- renderTable(
      {table9}, 
      striped = TRUE, bordered = TRUE,  
      hover = TRUE, spacing = 'l',  
      width = '100%', align = 'c',  
      rownames = TRUE,  na = '',
      digits = 3, 
      sanitize.text.function=identity) 
    
    
    
})
