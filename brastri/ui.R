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
source(here::here(#"brastri",
                  "functions.R"))

# Define UI for application
shinyUI(navbarPage(

  UPDATE FIGURES
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
                    html {font-family: Sylfaen;} 
                    .h3, h3 {font-size: 24px; font-family: Sylfaen}
                    body {font-family: Sylfaen; horizontal-align: middle; }
                  
                      ")),
# First tab, methods and text ---------------------------------------------
      tabPanel("Background",
               mainPanel(
      ## First has title
      h3(strong("Background"),
         style = "display: inline-block; margin-left: 5px;"),
      ## Second has legend
      box(width = NULL, status = "primary", collapsible = F,
          title  = "Figure 1", solidHeader = T,
          p(
          h4(strong("Experimental design and predictions "),
             style = "display: inline-block; margin-left: 5px; font-family: Sylfaen"),
          span(strong("(a)"), "The first step of our experimental design consisted in seeding bromeliads
          under emergence traps with experimental (Exp.) communities based on a large census 
          of wild bromeliads in Trinidad-and-Tobago (SIMLA) and Brasil (REGUA). The SIMLA community was 
          composed of fives", em("Scirtes"), "sp. (Coleoptera: Scirtidae), three Tipulid sp. 
          (Diptera: Tipulidae), and one", em("Polypedilum"), "sp. (Diptera: Chironomidae), and, in REGUA, 
          of three", em("Trentepholia"), "sp. (Diptera: Tipulidae), one Tanypodinae sp. 
          (Diptera: Chironomidae), four", em("Polypedilum"), "sp. (Diptera: Chironomidae), and seven 
          Culicinae sp. (Diptera: Culicidae). We then randomly attributed bromeliads to a 
          resource treatment, where leaf litter of a local species was enriched or not with 
          phosphorus. In the BR site only, we also had a predator treatment, where we 
          controlled the presence/absence of a top predator (Odonata: Coenagrionidae).", 
               strong("(b)"), "After setting up the experiment, we monitored the emergence of wing insects 
          daily, and took weekkly measurements of water chemistry parameters, such as 
          temperature, pH, chlorophyll-a and phosphorus concentration. We measured, weighted 
          and  dried emerging insects and assessed their phosphorus content. After four weeks 
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
          br(),
          style = "display: inline-block; margin-left: 10px; margin-right: 10px;"))),
      ## Third has the methods image, only one column
      box(width = NULL, status = "primary", collapsible = F,
          column(12, 
               div(img(src = "fig1.png",
                       height = "75%", width = "85%"), 
                   style = "text-align: center;"))),
      ## Empty box
      box(width = NULL, status = "primary", collapsible = F,
          p(span(br(),
                 br(),
                 br(),
                 "And just to show you how it concretely looked like..."))),
      ## Fourth has legend of pictures
      box(width = NULL, status = "primary", collapsible = F,
                   title  = "Figure 2", solidHeader = T,
                   p(h4(strong("Pictures of experiment in BR (left) and TT (right)"),
                    style = "display: inline-block; margin-left: 5px;"))),
      ## Fifth has two pictures pictures
      box(width = NULL, status = "primary", collapsible = F,
          solidHeader = F,
          column(7, div(img(src = "regua.jpg",
                              height = "100%", width = "100%"), 
                          style = "text-align: center;")),
          column(5, 
             div(img(src = "simla.jpg",
                     height = "75%", width = "100%"), 
                 style = "text-align: center;"))),
      ## Sixth has some space on bottom
      p(br(),
        br(),
        br(),
        "~~~",
        style = "text-align: center;")
      )),

# Second tab, raw bromeliad and temporal data -------------------------------------------
        tabPanel("Raw data",
                 mainPanel(
        ## First has title
        h3(strong("Some raw data"),
           style = "display: inline-block; margin-left: 5px"),
        ## Second has paragraphs on bromeliads
        p(span("In BR, we bought 32 organically-grown", em("Alcantarea imperialis"),
               "(Carrière) Harms, while, as organically-grown bromeliads were not 
               commercially available in TT, we used 32", em("Guzmania"), "Ruiz & Pav.
               and",  em("Vriesea"), "Lindl. bromeliads of similar size and shapes from our, 
               community censuses. Two weeks before the experiment, bromeliads were thoroughly 
               washed with water, and left to dry upside-down until the beginning of the 
               experiment, thus ensuring that no previous detritus or organisms 
               remained within the plants."),
          br(),
          br(),
          span("Below, you can play with some interactive plots to compare the 
                 bromeliads at the two sites, and the patterns of water chemistry across
                 time. Just select the parameter you want to see plotted, and the magic
                 will happen!"),
                style = "display: inline-block; margin-left: 10px; margin-right: 10px;"),
        ## Third row has inputs for bromeliad plots
        fluidRow(
          # Input for plot 1
          column(width = 6,
                 box(width = NULL, status = "primary", collapsible = F,
                     title  = "Interactive plot 1", solidHeader = T,
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
                     title  = "Interactive plot 2", solidHeader = T,
                     ## Landscape
                     radioButtons(
                       inputId = "x2",
                       label = "Choose parameter",
                       choices = c("Bromeliad width", "Bromeliad height",
                                   "Water holding capacity"), 
                       inline = TRUE)))), # end column 2
        ## Fourth row has the two bromeliad plots
        fluidRow(column(width = 6,
                        box(width = NULL, status = "primary",
                            solidHeader = F,
                            plotOutput("plot1", 
                                       height = 350))),
                 column(width = 6,
                        box(width = NULL, status = "primary",
                            solidHeader = F,
                            plotOutput("plot2", 
                                       height = 350)))),
        ## Fifth row has inputs for bromeliad plots
        fluidRow(
          # Input for plot 3
          column(width = 6,
                 box(width = NULL, status = "primary", collapsible = F,
                     title  = "Interactive plot 3", solidHeader = T,
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
                     title  = "Interactive plot 4", solidHeader = T,
                     ## Landscape
                     radioButtons(
                       inputId = "y4",
                       label = "Choose parameter",
                       choices =  c("pH", "Conductivity", "Temperature",
                                    "Chlorophyll-a", "Phosphorus"), 
                       inline = TRUE)))), # end column 2
        ## Sixth has the two bromeliad plots
        fluidRow(column(width = 6,
                        box(width = NULL, status = "primary",
                            solidHeader = F, 
                            plotOutput("plot3", 
                                       height = 350))),
                 column(width = 6,
                        box(width = NULL, status = "primary",
                            solidHeader = F,
                            plotOutput("plot4", 
                                       height = 350)))),
        ## Seventh row has biomass plot and some text
        box(width = NULL, status = "primary", collapsible = F,
                     title  = "Figure 3", solidHeader = T,
                     column(width = 4,
                              p(span("Estimates of the initial biomasses for each organism
                                       at the beginning of the experiment. We computed these 
                                       estimates from a random sample of four of five bromeliads,
                                       in which we measured the larvae before seeding them into
                                       the bromeliad. We then used the an R package (`hellometry`,
                                       written by P. Rogy) to get the dry biomass estimates using allometric
                                       equations using the measured body length combined with other dry mass and body
                                       length measurements from the Bromeliad Working Group.",
                                       style = "display: inline-block; margin-left: 10px; margin-right: 10px;")) ),
                     column(width = 8,
                            div(img(src = "biomass_plot_before.jpg",
                                    height = "100%", width = "100%"), 
                                style = "text-align: center;"))))),




# Third tab - Water chemistry and decomposition analyses -----------------------------------------------
  tabPanel("Water chemistry and decomposition",
           mainPanel(
          ## First has paragraphs on analyses
          h3(strong("Effects of treatments on water chemistry and decomposition"),
             style = "display: inline-block; margin-left: 5px"),
             p(span("As we have a complex, multilevel dataset, we computed Bayesian, 
                      multivel models using the “brms” R package (Bürkner, 2017) using un
                      uniform priors and different random effect combinations depending on 
                      the specific model. When comparing values at the beginning and/or at 
                      the end of the experiments, such as biomass accumulation, emergence 
                      rate or biomass export, we used bromeliad species nested within 
                      country as random effect. When comparing values across time, such as 
                      phosphate/ total phosphorus concentration, conductivity, or pH, we 
                      included bromeliad individual nested within bromeliad species
                      nested within country, and crossed with day of collection.", br(),
                   "Our approach differs from frequentist approaches in that we use two 
                      indices to analyse our model outputs, instead of the lone index of 
                      effect significance represented by the p-value. The first one, pd 
                      (probability of 
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
              style = "display: inline-block; margin-left: 10px; margin-right: 10px;"),
          ## Second row has output table
          fluidRow((column(width = 12,
                           box(width = NULL, status = "primary",
                               solidHeader = T, 
                               title = "Table 1",
                               tableOutput("table1"))))),
          ## Third has legend of table
          h4(strong("Posterior tests of water chemistry models."),
             style = "display: inline-block; margin-left: 5px; "),
             p(span("The posterior median values compare to a reference state of Resource = control 
                     and Predator = absent. Empty cells mean that the specific independent variable, 
                     was not used in a given model. pd = probability of direction (proportion of the 
                     posterior distribution that is of the same sign that the median), ROPE = region of 
                     practical equivalence (effect size of no practical relevance, here twice the 
                     standard deviation of the dependent variable around 0). Values between parentheses 
                     to the right of the median represent the boundaries of the 95% confidence interval,
                     while under the dependent variable name represent the ROPE itself. 
                     Significant associations are in boldface."),
               style = "display: inline-block; margin-left: 10px; "),
          ## Fourth has plot legend
          column(width = 12,
                  box(width = NULL, status = "primary",
                      solidHeader = T, 
                      title = "Figure 4",
                      h4(strong("Associations of our treatments with different water chemistry variables"),
                         style = "display: inline-block; margin-left: 5px; "),
                         p(span("Y-axes of (e) and (f) are on a logarithmic scale, and the x axis of (e) 
                                 differs from that of the other plots, as we did not have a predator 
                                 treatment at the TT site (see methods).", br(),
                                "Here, we have two significant effects: in BR, the 
                                presence of damselfly predator reduces the total P 
                                concentration in the water, while in TT, enriching the 
                                resource in P lead to higher phosphate in the water."),
                           style = "display: inline-block; margin-left: 10px; "))),
          ## Fifth has figure 4
          box(width = NULL, status = "primary",
              solidHeader = F, 
              column(12, 
                     div(img(src = "figs1.jpg",
                             height = "100%", width = "100%"), 
                         style = "text-align: center;"))),
          ## Sixth row has table
          fluidRow((column(width = 12,
                           box(width = NULL, status = "primary",
                               solidHeader = T, 
                               title = "Table 2",
                               tableOutput("table2"))))),
          ## Seventh has legend
          h4(strong("Posterior tests of decomposition models."),
             style = "display: inline-block; margin-left: 5px; "),
             p(span("The posterior median values compare to a reference state of Resource = control 
                     and Predator = absent. pd = probability of direction (proportion of the 
                     posterior distribution that is of the same sign that the median), ROPE = region of 
                     practical equivalence (effect size of no practical relevance, twice the 
                     standard deviation of the dependent variable around 0). Values between parentheses 
                     to the right of the median represent the boundaries of the 95% confidence interval,
                     while under the dependent variable name represent the ROPE itself. 
                     Significant associations are in boldface."),
                 style = "display: inline-block; margin-left: 10px; "),
          ## Eigth has plot legend
          box(width = NULL, status = "primary",
              solidHeader = T, 
              title = "Figure 5",
              column(width = 12,
              h4(strong("Associations of our treatments with decomposition variables"),
                 style = "display: inline-block; margin-left: 5px; "),
              p(span("The two distinct louds of points represent the two different sites,
                      the TT site did not have a predator treatment. In the models, site
                      is included as a random variable, in which bromeliad species and
                      is nested. We have here large, overlapping confidence 
                     intervals, but the model still picked a negative effect of
                     resource enrichment on microbial decomposition."),
                style = "display: inline-block; margin-left: 10px; "))),
          ## Ninth row has figure 2
          column(12, 
                 div(img(src = "figs2.jpg",
                         height = "100%", width = "100%"), 
                     style = "text-align: center;")),
        )),
# Fourth tab - Problem with invertebrates -----------------------------------------------
tabPanel("A problem with invertebrates",
         mainPanel(
           ## First has presentation of the methods
           h3(strong("A problem with invertebrates"),
              style = "display: inline-block; margin-left: 5px;"),
              p(span("As described in the raw data panel, we have seeded our bromeliads
                       with a standardised community in each site. We then covered our
                       bromeliads with a mosquito netting, anchored in the ground. 
                       Unfortunately, this did not prevent tourists from making to our bromeliads,
                       probably by crawling under the net or emerging from the
                       ground. To ensure that organisms captured within the emergence traps
                       were indeed from the bromeliads, we raised additional larvae caught
                       in wild bromeliads, and collected the emerging adults to compare them
                       to organisms from the trap. We could not get adults from every larval
                       species, thus we also used taxonomy to determine if the caught organism
                       could be associated with bromeliads.In other words,  we were able
                       to differentiate organisms that emerged, or were likely to emerge,
                       from the bromeliads from those that did not. It is however impossible
                       to determine precisely which species are bromeliad-associated 
                       because of the lack of DNA data.",
                       style = "display: inline-block; margin-left: 5px; ")),
           ## Second has figure of what we found in the traps, all the families,
           ## highlighting the ones from the beginning
           box(width = NULL, status = "primary", collapsible = F,
               title  = "Figure 6", solidHeader = T,
               div(img(src = "emergence_everything.jpg",
                       height = "100%", width = "100%"), 
                   style = "text-align: center;")),
           ## Third has table legend
           h4(strong("Organisms collected in emergence traps"),
              style = "display: inline-block; margin-left: 5px; "),
           p(span("Arranged by decreasing abundance across both sites, bolded
                  names represent families that contain larvae common amongst bromeliad
                  organisms. "),
             style = "display: inline-block; margin-left: 5px; "),
           ## Fourth has text on the problem that we have with tourists that made it in
           box(width = NULL, status = "primary", collapsible = F,
               title  = "Figure 7", solidHeader = T,
               h4(strong("A little problem..."),
              style = "display: inline-block; margin-left: 5px; "),
              p(span("However we have a little problem. Beyond tourists organisms 
                       making it to the emergence, some also made it inside the 
                       bromeliads. Have a look at the graphs below. The one to the left
                       represent the estimated biomass of the seeded organisms at the 
                       beginning of the experiment, while the one to the right represent
                       that at the end of the experiment. At first sight, it would seem that
                       our larvae have grown well, especially scirtids and tipulids. However,
                       the number of larvae for scirtids and",  em("Polypedilum"), 
                       "was higher at the end of the experiement than at the beginning!"),
                  style = "display: inline-block; margin-left: 5px; ")),
           ## Fifth has two biomass plots
           box(width = NULL, status = "primary", collapsible = F, 
               column(6, 
                  div(img(src = "biomass_plot_before.jpg",
                          height = "100%", width = "100%"), 
                      style = "text-align: center;")),
            column(6, 
                   div(img(src = "biomass_plot_after.jpg",
                           height = "100%", width = "100%"), 
                       style = "text-align: center;"))),
           ## Sixth has introduction to interactive plots
           column(12,
             p(span("With the two interactive plots below, you can compare the size
                       distributions of larvae at the beginning and at the end of the
                       experiment."),
                 style = "margin-left: 5px; ")),
           
           ## Seventh row has inputs for plots
           fluidRow(
             # Input for plot 1
             column(width = 6,
                    box(width = NULL, status = "primary", collapsible = F,
                        title  = "Interactive plot 5", solidHeader = T,
                        ## Landscape
                        radioButtons(
                          inputId = "x5",
                          label = "Choose species",
                          choices = c("Scirtes", "Tipulidae",
                                      "Tanypodinae", "Polypedilum",
                                      "Wyeomyia", "Coenagrionidae"),  
                          inline = TRUE))), 
             # Input for plot 2
             column(width = 6,
                    box(width = NULL, status = "primary", collapsible = F,
                        title  = "Interactive plot 6", solidHeader = T,
                        ## Landscape
                        radioButtons(
                          inputId = "x6",
                          label = "Choose species",
                          choices = c("Scirtes", "Tipulidae",
                                      "Tanypodinae", "Polypedilum",
                                      "Wyeomyia", "Coenagrionidae"), 
                          
                          inline = TRUE)))), # end column 2
           # Eigth row has the two plots
           fluidRow(column(width = 6,
                           box(width = NULL, status = "primary",
                               solidHeader = F,
                               plotOutput("plot5",
                                          height = 350))),
                    column(width = 6,
                           box(width = NULL, status = "primary",
                               solidHeader = F,
                               plotOutput("plot6",
                                          height = 350)))),

           ## Ninth has short explanation before table
           box(width = NULL, status = "primary", collapsible = F,
               p(span("We can also look at this problem in terms of abundance. Here, 
                       there if an individual is unaccounted for, there is no way to 
                       confidently attribute this disappearance to death from starvation,
                       disease, predation, or if the organism simply avoided collection
                       at the end of the experiment or emerged and died on the ground
                       without making it to the emergence trap."),
                      style = "display: inline-block; margin-left: 5px;")),
           ## Tenth has table 3a and b
           box(width = NULL, status = "primary", collapsible = F,
               title  = "Table 3", solidHeader = T,
               column(width = 6,
                      tableOutput("table3a")),
               column(width = 6,
                      tableOutput("table3b"))),
           ## Eleventh has table legend
           h4(strong("Seeding and collection patterns for BR (left) and TT (right)"),
              style = "display: inline-block; margin-left: 5px; "),
           p(span("We compare here the number of individuals in each species seeded
                  at the beginning of the experiment to the number that emerged during the
                  experiment, and that remained as larvae at the end of the experiment"),
             style = "display: inline-block; margin-left: 10px; "),
           ## Ninth row has introduction to last plot
           fluidRow(p(span("Beyond these groups, other organisms that we did not seed inside
                           the bromeliad also made it, as shown in the plots below"),
                           style = "display: inline-block; margin-left: 5px; ")),
           ## Twelfth row has plot with extra groups of organisms
           fluidRow(column(12, 
                           box(width = NULL, status = "primary",
                               solidHeader = T, 
                               title = "Figure 8",
                               p(h4(strong("Biomass of organisms remaining within bromeliads at the end of the experiment."),
                                     style = "display: inline-block; margin-left: 5px; "),
                                 br(),
                                  p(span("Values represent a combination of biomass measured directly and estimated"),
                                    style = "display: inline-block; margin-left: 5px; ")),
                             div(img(src = "biomass_plot_after_tourists.jpg",
                                   height = "100%", width = "100%"), 
                               style = "text-align: center;")))),
           ## Thirteenth row has exposition of problem
           fluidRow(h4(strong("A little problem..."),
                       style = "display: inline-block; margin-left: 5px; "),
                    p(span("While the tourist communities in the emergence trap may have crawled
                           under the net, it is unclear how organisms, especially scirtids in 
                           TT made it inside the bromeliads. They could have crawled, but eggs 
                           and/or resistant forms could have escaped our washing, and survived
                           the two weeks of drying. It will thus be challenging to do any 
                           statistics to compare the biomass and the end of the experiment,
                           because of the uncontrolled environment.One option would be to exclude
                           larvae that we collected at the end of the experiment, and that
                           were smaller than the ones at the beginning of the experient, but that
                           only concerns a minority of specimens, as shown in the graph below", br(),
                           "At this stage, I feel reluctant to throw away this data, and would 
                           like to ask you if you have any ides/comments/suggestions on how to deal
                           with this problem. As I haven't figured out (yet), how to allow
                           people to write messages and shiny apps without making a whole website,
                           please include your thoughts and comments to this",
                           tags$a(href = "https://miro.com/welcomeonboard/VVlOblV4ekt0SVkwUTVpcmYwdzNRd25uak13eGhxYWJDOFRIRzBETUZucG1WQUlPRkc1Yzd1OFJqcTE4VXpvV3wzMDc0NDU3MzU2NjM0Nzc5Nzk1fDI=?share_link_id=858605805875",
                                  "Miro board", target="_blank"),
                           "You can draw post-its from the four main themes of feedback and write on them.
                           You can also create your own theme, make drawings. Since there is quite a team 
                           in many different places for this project, I am trying ways so that everyone who
                           wants to participate more can do it effectively. Please let me knwo if you have
                           a better way in mind!",
                      style = "display: inline-block; margin-left: 5px; "))),
         )),
# Fifth tab - Invertebrate analysis attempt -----------------------------------------------
tabPanel("Invertebrate analysis attempt",
         mainPanel(
           ## First explication of the attempts
           h3(strong("Attempt at analysing invertebrate data"),
              style = "display: inline-block; margin-left: 5px;"),
           p(span("I decided to proceed first with the emergence data, as the problem
                  was easier to solve. Here, I used a combined approach to decide if an
                  adult insect was likely to have emerged from a bromeliad. First, we
                  raised larvae from bromeliads, to compare the adult stages of these larvae
                  to those we may have found in the bromeliad. But some larva are tricky to raise,
                  so we were not able to get adults for all species, in particular for chironomids
                  and ceratopogonids, we thus considered all members of these families as having
                  emerged from the bromeliads. ", br(),
                  "I have divided the analysis concerning the biomass at emergence in two 
                  parts, one where I have summed up the biomasses of all individuals within a 
                  family (total biomass emerged), and the other where I have kept individual 
                  biomasses (individual biomass emerged). While the first part gives us an 
                  idea of how much biomass export varies amongst treatment, the second ones 
                  gives us an idea of how individuals may vary across treatments. Note that 
                  these analyses include two families that we did not seed at the beginning
                  of the experiment: Psychodidae and Ceratopogonidae.",
                  style = "display: inline-block; margin-left: 5px; ")),
           ## Second has total biomass emerged
           column(12, 
                 p(h4(strong("Outputs of models on total biomass emerged"),
                 style = "background: #f39c12;margin-left: 0px"))),
           column(12, 
                  p(span("The effect of our treatments on emergence 
                         differed amongst groups. First, there was no 
                         effect on the overall biomass of bromeliad-
                         associated insects captured within the emergence 
                         traps, nor on the total biomass of insects from 
                         groups seeded at the beginning of the experiment.
                         At the family level, the total emerged biomass of
                         chironomids responded to our treatments in an 
                         interactive manner. There was less chironomid
                         biomass emerging in the presence of a predator 
                         when the resource was not enriched in P, but, 
                         when the resource was enriched in P, there was 
                         more chironomid biomass emerging. On the other hand, 
                         the total biomass of culicids, tipulids and 
                         ceratopogonids did not vary across treatments.",
                         style = "display: inline-block; margin-left: 5px; "))),
           column(width = 12,
                      p(span(strong("Table 4."), "The posterior median values compare to 
                             a reference state of Resource = control and Predator = absent. pd = probability of direction 
                             (proportion of the posterior distribution that is of the same sign that the median), 
                             ROPE = region of practical equivalence (effect size of no practical relevance, here 
                             twice the standard deviation of the dependent variable around 0). 
                             Values between parentheses to the right of the median represent the boundaries of the 
                             95% confidence interval, while under the dependent variable name represent the ROPE itself. 
                             Significant associations are in boldface.",
                             style = "display: inline-block; margin-left: 5px; "))),
           column(width = 12,
                  tableOutput("table4")),
           column(12, 
                  p(span(strong("Figure 9"),
                  "Associations of our treatments with different 
                  total biomass emerged at the bromeliad scale"))),
           column(width = 12,
                  div(img(src = "fig2.jpg",
                          height = "100%", width = "100%"), 
                      style = "text-align: center;")),    
           ## Third has individual biomass emerged
           column(12, 
                  p(h4(strong("Outputs of models on individual body mass of emerged adults."),
                       style = "background: #f39c12;margin-left: 0px"))),
           column(12, 
                  p(span("In terms of individual body mass, chironomid 
                         individuals were also heavier when emerging from 
                         bromeliads with predators and having received 
                         enriched litter as resource. In the presence of 
                         predators, culicids emerged smaller if the 
                         resource was control, but larger if the resource 
                         was enriched in P. Tipulids showed a different 
                         pattern, as their body mass was smaller when 
                         emerging from bromeliads without predators and 
                         with enriched leaf litter. Finally, ceratopogonids
                         remained unaffected by either treatment."))),
           column(width = 12,
                  p(span(strong("Table 5."), 
                   "The posterior median values compare to 
                   a reference state of Resource = control and Predator = absent. pd = probability of direction 
                   (proportion of the posterior distribution that is of the same sign that the median), 
                   ROPE = region of practical equivalence (effect size of no practical relevance, here 
                   twice the standard deviation of the dependent variable around 0). 
                   Values between parentheses to the right of the median represent the boundaries of the 
                   95% confidence interval, while under the dependent variable name represent the ROPE itself. 
                   Significant associations are in boldface."))),
           column(width = 12,
                  tableOutput("table5")),
           column(12, 
                  p(span(strong("Figure 10."),
                  "Associations of our treatments with 
                  individual body mass of emerged adults"))),
           column(12, 
                  div(img(src = "fig4.jpg",
                          height = "100%", width = "100%"), 
                      style = "text-align: center;")),
         
           
           ## Fourth introduction of emergence rate analysis
            column(12,
                   p(h4(strong("Outputs of models on number of days until emergence."),
                        style = "background: #f39c12;margin-left: 0px"))),
         column(12, 
                p(span("Our treatments did not influence the rate at which 
                         organisms emerged from the bromeliads "))),
           column(width = 12,
                  p(span(strong("Table 6."), 
                  "The posterior median values compare to 
                  a reference state of Resource = control and Predator = absent. pd = probability of direction 
                  (proportion of the posterior distribution that is of the same sign that the median), 
                  ROPE = region of practical equivalence (effect size of no practical relevance, here 
                  twice the standard deviation of the dependent variable around 0). 
                  Values between parentheses to the right of the median represent the boundaries of the 
                  95% confidence interval, while under the dependent variable name represent the ROPE itself. 
                  Significant associations are in boldface."))),
           column(width = 12,
                  tableOutput("table6")),
           column(12, 
                  p(span(strong("Figure 11."),
                  "Associations of our treatments with 
                      number of days until emergence"))),
           column(12, 
                  div(img(src = "fig5.jpg",
                          height = "100%", width = "100%"), 
                      style = "text-align: center;")),

           ))
))
    

