#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(tidyverse)
library(viridis)
library(cowplot)
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(here)
source(here::here("brastri",
                  "functions.R"))

# Define UI for application
shinyUI(navbarPage(

  # First do header
  title = "Data from Pierre's experiments",
  # Style -------------------------------------------------------------------
  tags$style(HTML(".box.box-solid.box-primary>.box-header{background:#f39c12}
                         .box.box-solid.box-primary{}
                         .box.box-primary{border-top-color:#f39c12;}
                    .navbar-default {background-color: #f39c12;border-color: #f39c12;}
                    .navbar-default.navbar-brand {color: #000;}
                    .navbar-default .navbar-nav > .active > a, .navbar-default 
                    .navbar-nav > .active > a:focus, .navbar-default .navbar-nav > 
                    .active > a:hover {color: #000;background-color: #e7e7e7;}
                    .navbar-default .navbar-brand {   color: #000; }
                    .navbar-default .navbar-nav > li > a {color: #000;}
                    .*{font-family: Sylfaen;}
                  
                      ")),
# First tab, methods and text ---------------------------------------------
      tabPanel("Background",
               mainPanel(
      ## First row has the methods image, only one column
      fluidRow(column(12, div(img(src = "img1.png",
                                  height = "75%", width = "85%"), 
                              style = "text-align: center;"))),
  
      
      ## Second row has the legend
      fluidRow(p(
        br(),
        h4(strong("Figure 1. Experimental design and predictions "),
           style = "display: inline-block; margin-left: 5px; font-family: Sylfaen"),
        span(strong("(a)"), "The first step of our experimental design consisted in seeding bromeliads
        under emergence traps with experimental (Exp.) communities based on a large census 
        of wild bromeliads in Trinidad-and-Tobago (TT) and Brasil (BR). The TT community was 
        composed of fives", em("Scirtes"), "sp. (Coleoptera: Scirtidae), three Tipulid sp. 
        (Diptera: Tipulidae), and one", em("Polypedilum"), "sp. (Diptera: Chironomidae), and, in BR, 
        of three", em("Trentepholia"), "sp. (Diptera: Tipulidae), one Tanypodinae sp. 
        (Diptera: Chironomidae), four", em("Polypedilum"), "sp. (Diptera: Chironomidae), and seven 
        Culicinae sp. (Diptera: Culicidae). We then randomly attributed bromeliads to a 
        resource treatment, where leaf litter of a local species was enriched or not with 
        phosphorus. In the BR site only, we also had a predator treatment, where we 
        controlled the presence/absence of a top predator (Odonata: Coenagrionidae).", 
        strong("(b)"), "After setting up the experiment, we monitored the emergence of wing insects 
        daily, and measured water chemistry parameters, such as temperature, pH, 
        chlorophyll-a and phosphorus concentration, weekly. We measured, weighted and 
        dried emerging insects and assessed their phosphorus content. After four weeks 
        of experiment, we collected the remaining invertebrates in the bromeliads, 
        and followed the same measuring, weighting and P-assessing procedures than for 
        the adults. Finally, we measured how much biomass was consumed from the leaf 
        litter bags.",
        strong("(c)"), "We predict that decomposition rates will be higher with enriched 
        leaf litter, and that predators will decrease them, but more so when leaf litter 
        is enriched. We also predict that, when there is no predator, enriched leaf litter 
        will increase biomass accumulation and P content of organisms with a faster growth 
        rate, while control leaf litter will have a similar effect on organisms with a 
        slower growth rate. In treatments with predators, patterns will be the same with 
        control leaf litter, while predators will deviate biomass accumulation of organisms 
        with a faster growth rate to their own. Finally, enriched leaf litter will increase 
        the growth rate of organisms with a fast growth rate, and decrease that of organisms
        with a slow growth rate. Predators will increase the growth rates of all other 
        organisms, without interactive effects with the resource treatment.",
        style = "display: inline-block; margin-left: 10px; margin-right: 10px; font-family: Sylfaen;"))),
      
      ## Third row has legend of pictures
      fluidRow(p(h5(strong("Pictures of experiment in BR (left) and TT (right)"),
                    style = "display: inline-block; margin-left: 5px; font-family: Sylfaen"))),
      
      ## Fourth row has pictures
      fluidRow(column(7, 
                      div(img(src = "regua.jpg",
                              height = "100%", width = "100%"), 
                          style = "text-align: center;")),
               column(5, 
                      div(img(src = "simla.jpg",
                              height = "75%", width = "100%"), 
                          style = "text-align: center;"))))),




# Second tab, raw bromeliad and temporal data -------------------------------------------
        tabPanel("Some raw data",
                 mainPanel(
        ## First row has paragraphs on bromeliads
        fluidRow(
          br(),
          p(span("At REGUA, we bought 32 organically-grown", em("Alcantarea imperialis"),
                   "(Carrière) Harms, while, as organically-grown bromeliads were not 
                   commercially available in SIMLA, we used 32", em("Guzmania"), "Ruiz & Pav.
                   and",  em("Vriesea"), "Lindl. bromeliads of similar size and shapes from our, 
                   community censuses. Two weeks before the experiment, bromeliads were thoroughly 
                   washed with water, and left to dry upside-down until the beginning of the 
                   experiment, thus ensuring that no previous detritus or organisms 
                   remained within the plants."),
                    style = "display: inline-block; margin-left: 10px; margin-right: 10px;font-family: Sylfaen")),
        ## Second row has inputs for bromeliad plots
        fluidRow(
          # Input for plot 1
          column(width = 6,
                 box(width = NULL, status = "primary", collapsible = F,
                     title  = "Inputs for plot 1", solidHeader = T,
                     ## Landscape
                     radioButtons(
                       inputId = "x1",
                       label = "Choose parameter",
                       choices = c("Bromeliad width", "Bromeliad height",
                                   "Water holding capacity"), 
                       inline = TRUE))), 
          # Input for plot 2
          column(width = 6,
                 box(width = NULL, status = "primary", collapsible = F,
                     title  = "Inputs for plot 2", solidHeader = T,
                     ## Landscape
                     radioButtons(
                       inputId = "x2",
                       label = "Choose parameter",
                       choices = c("Bromeliad width", "Bromeliad height",
                                   "Water holding capacity"), 
                       inline = TRUE)))), # end column 2
        ## Third row has the two bromeliad plots
        fluidRow(column(width = 6,
                        box(width = NULL, status = "primary",
                            solidHeader = F,
                            title = "Plot 1",
                            plotOutput("plot1", 
                                       height = 350))),
                 column(width = 6,
                        box(width = NULL, status = "primary",
                            solidHeader = F,
                            title = "Plot 2",
                            plotOutput("plot2", 
                                       height = 350)))),
        ## Fourth row has inputs for bromeliad plots
        fluidRow(
          # Input for plot 3
          column(width = 6,
                 box(width = NULL, status = "primary", collapsible = F,
                     title  = "Inputs for plot 3", solidHeader = T,
                     ## Landscape
                     radioButtons(
                       inputId = "y3",
                       label = "Choose parameter",
                       choices = c("pH", "Conductivity", "Temperature",
                                   "Chlorophyll-a", "Phosphorus"), 
                       inline = TRUE))), 
          # Input for plot 4
          column(width = 6,
                 box(width = NULL, status = "primary", collapsible = F,
                     title  = "Inputs for plot 4", solidHeader = T,
                     ## Landscape
                     radioButtons(
                       inputId = "y4",
                       label = "Choose parameter",
                       choices =  c("pH", "Conductivity", "Temperature",
                                    "Chlorophyll-a", "Phosphorus"), 
                       inline = TRUE)))), # end column 2
        ## Fifth row has the two bromeliad plots
        fluidRow(column(width = 6,
                        box(width = NULL, status = "primary",
                            solidHeader = F, 
                            title = "Plot 3",
                            plotOutput("plot3", 
                                       height = 350))),
                 column(width = 6,
                        box(width = NULL, status = "primary",
                            solidHeader = F,
                            title = "Plot 4",
                            plotOutput("plot4", 
                                       height = 350),
                            style = ".h3, h3 {font-size: 24px;font-family: Sylfaen}"))),
        ## Sixth row has biomass plot and some text
        fluidRow(column(width = 4,
                        p(h4(strong("Plot 5",
                                    style = "display: inline-block; margin-left: 5px;font-family: Sylfaen")),
                            span("Estimates of the initial biomasses for each organism
                                 at the beginning of the experiment. We computed these 
                                 estimates from a random sample of four of five bromeliads,
                                 in which we measured the larvae before seeding them into
                                 the bromeliad. We then used the an R package (`hellometry`,
                                 written by P. Rogy) to get the dry biomass estimates from 
                                 body length of the specimen, and other dry mass and body
                                 length measurements from the biomass working group.",
                                 style = "display: inline-block; margin-left: 10px; margin-right: 10px;font-family: Sylfaen")) ),
                 column(width = 8,
                        div(img(src = "biomass_plot_before.jpg",
                                height = "100%", width = "100%"), 
                            style = "text-align: center;"))))),




# Third tab - invertebrates -----------------------------------------------
  tabPanel("Preliminary analyses",
           mainPanel(
          ## First row has paragraphs on invertebrates
          fluidRow(h4(strong("Effect of treatments on water chemistry and decomposition"),
                      style = "display: inline-block; margin-left: 5px; font-family: Sylfaen"),
            p(span("As we have a complex, multilevel dataset, we computed Bayesian, 
                      multivel models using the “brms” R package (Bürkner, 2017) using un
                      iform priors and different random effect combinations depending on 
                      the specific model. When comparing values at the beginning and/or at 
                      the end of the experiments, such as biomass accumulation, emergence 
                      rate or biomass export, we used bromeliad species nested within 
                      country as random effect. When comparing values across time, such as 
                      phosphate/ total phosphorus concentration, conductivity, or pH, we 
                      included bromeliad individual nested within bromeliad species, which 
                      was still nested within country.", br(),
                   "Our approach differs from frequentist approaches in that we use two 
                      indices to analyse our model outputs, instead of the lone index of 
                      effect significance of the p-value. The first one, pd (probability of 
                      direction) is concerned with the statistical existence of an effect, 
                      and represents the proportion of the posterior distribution that is 
                      of the same sign that the median (Makowski et al., 2019). The second 
                      one, % in ROPE (percentage in the region of practical equivalence) is 
                      concerned with effect significance, and represents the percentage of 
                      the posterior distribution that belongs to the ROPE, region of effect 
                      sizes so small as to be practically irrelevant (Makowski et al., 2019). 
                      Here, we compute this region as twice the standard deviation of the 
                      dependent variable around 0 (Makowski et al., 2019). In this paper, 
                      we consider as fully significant an association with both pd > 95% and 
                      % in ROPE < 5%, and as non-significant associations for which with one 
                      or no such cut-off are met."),
              style = "display: inline-block; margin-left: 10px; margin-right: 10px;font-family: Sylfaen"))
          ))
        
        ))
    

