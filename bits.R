# bits


# First row has inputs for plots 1 and 2 ----------------------------------------------------
fluidRow(
  # 1/2 of page (width = 6 of 12 columns)
  column(width = 6,
         box(width = NULL, status = "primary", collapsible = T,
             title  = "Inputs for plot 1", solidHeader = T,
             ## Landscape
             radioButtons(
               inputId = "landscape1",
               label = "Choose landscape",
               choices = c("Less habitat 1", "Less habitat 2",
                           "More habitat 1", "More habitat 2"), 
               inline = TRUE),
             ## Matrix type
             radioButtons(
               inputId = "web1",
               label = "Choose matrix type",
               choices = c("Cascadey", "Even", "Uncascadey"), 
               inline = TRUE),
             ## Cumulative sum or per cap
             radioButtons(
               inputId = "scale1",
               label = "Choose scale",
               choices = c("Per capita", "Cumulative sum"), 
               inline = TRUE),
             ## X-axis
             radioButtons(
               inputId = "x1",
               label = "X-axis",
               choices = c("Time", "Prey range of perception"), 
               inline = TRUE), 
             radioButtons(
               inputId = "range_selected1",
               label = "If you picked time, over which range of perception?",
               choices = c("HD01", "HD02", "HD03", "HD04", "HD05", "HD06", "HD07"), 
               inline = TRUE),
             ## Y-axis
             radioButtons(
               inputId = "y1",
               label = "Y-axis",
               choices = c("Abundance", "Births",
                           "Deaths", "Predation",
                           "Realised int. strength",
                           "Eaten", "Number of moves",
                           "Average dist. per move"),
               inline = TRUE),
             ## Error bars
             radioButtons(inputId = "errorbars1", 
                          label = "Display standard deviation?", 
                          choices = c("Yes", "No"),
                          selected = "No", 
                          inline = TRUE)
         ) # end box 1
  ), # end column 1
  # 1/2 of page (width = 6 of 12 columns)
  column(width = 6,
         box(width = NULL, status = "primary", collapsible = T,
             title  = "Inputs for plot 2", solidHeader = T,
             ## Landscape
             radioButtons(
               inputId = "landscape2",
               label = "Choose landscape",
               choices = c("Less habitat 1", "Less habitat 2",
                           "More habitat 1", "More habitat 2"), 
               inline = TRUE),
             ## Matrix type
             radioButtons(
               inputId = "web2",
               label = "Choose matrix type",
               choices = c("Cascadey", "Even", "Uncascadey"), 
               inline = TRUE),
             ## Cumulative sum or per cap
             radioButtons(
               inputId = "scale2",
               label = "Choose scale",
               choices = c("Per capita", "Cumulative sum"), 
               inline = TRUE),
             ## X-axis
             radioButtons(
               inputId = "x2",
               label = "X-axis",
               choices = c("Time", "Prey range of perception"), 
               inline = TRUE), 
             radioButtons(
               inputId = "range_selected2",
               label = "If you picked time, over which range of perception?",
               choices = c("HD01", "HD02", "HD03", "HD04", "HD05", "HD06", "HD07"), 
               inline = TRUE),
             ## Y-axis
             radioButtons(
               inputId = "y2",
               label = "Y-axis",
               choices = c("Abundance", "Births",
                           "Deaths", "Predation",
                           "Realised int. strength",
                           "Eaten", "Number of moves",
                           "Average dist. per move"),
               inline = TRUE),
             ## Error bars
             radioButtons(inputId = "errorbars2", 
                          label = "Display standard deviation?", 
                          choices = c("Yes", "No"),
                          selected = "No", 
                          inline = TRUE)
         ) # end box 1
  ) # end column 2
  # end column 4
), # end fluidrow
# Second row has plots 1 and 2 ----------------------------------------------------
fluidRow(column(width = 6,
                box(width = NULL, status = "primary",
                    solidHeader = TRUE, 
                    title = "Plot1",
                    plotOutput("plot1", 
                               height = 350))),
         column(width = 6,
                box(width = NULL, status = "primary",
                    solidHeader = TRUE, 
                    title = "Plot2",
                    plotOutput("plot2", 
                               height = 350)))
         
),

# Third row has plots 3 and 4 ----------------------------------------------------
fluidRow(column(width = 6,
                box(width = NULL, status = "primary",
                    solidHeader = TRUE, 
                    title = "Plot3",
                    plotOutput("plot3", 
                               height = 350))),
         column(width = 6,
                box(width = NULL, status = "primary",
                    solidHeader = TRUE, 
                    title = "Plot4",
                    plotOutput("plot4", 
                               height = 350)))
         
),

# Fourth row has inputs for plots 3 and 4 ---------------------------------
fluidRow(
  # 1/2 of page (width = 6 of 12 columns)
  column(width = 6,
         box(width = NULL, status = "primary", collapsible = T,
             title  = "Inputs for plot 3", solidHeader = T,
             ## Landscape
             radioButtons(
               inputId = "landscape3",
               label = "Choose landscape",
               choices = c("Less habitat 1", "Less habitat 2",
                           "More habitat 1", "More habitat 2"), 
               inline = TRUE),
             ## Matrix type
             radioButtons(
               inputId = "web3",
               label = "Choose matrix type",
               choices = c("Cascadey", "Even", "Uncascadey"), 
               inline = TRUE),
             ## Cumulative sum or per cap
             radioButtons(
               inputId = "scale3",
               label = "Choose scale",
               choices = c("Per capita", "Cumulative sum"), 
               inline = TRUE),
             ## X-axis
             radioButtons(
               inputId = "x3",
               label = "X-axis",
               choices = c("Time", "Prey range of perception"), 
               inline = TRUE), 
             radioButtons(
               inputId = "range_selected3",
               label = "If you picked time, over which range of perception?",
               choices = c("HD01", "HD02", "HD03", "HD04", "HD05", "HD06", "HD07"), 
               inline = TRUE),
             ## Y-axis
             radioButtons(
               inputId = "y3",
               label = "Y-axis",
               choices = c("Abundance", "Births",
                           "Deaths", "Predation",
                           "Realised int. strength",
                           "Eaten", "Number of moves",
                           "Average dist. per move"), 
               inline = TRUE),
             ## Error bars
             radioButtons(inputId = "errorbars3", 
                          label = "Display standard deviation?", 
                          choices = c("Yes", "No"),
                          selected = "No", 
                          inline = TRUE)
         ) # end box 1
  ), # end column 1
  # 1/2 of page (width = 6 of 12 columns)
  column(width = 6,
         box(width = NULL, status = "primary", collapsible = T,
             title  = "Inputs for plot 4", solidHeader = T,
             ## Landscape
             radioButtons(
               inputId = "landscape4",
               label = "Choose landscape",
               choices = c("Less habitat 1", "Less habitat 2",
                           "More habitat 1", "More habitat 2"), 
               inline = TRUE),
             ## Matrix type
             radioButtons(
               inputId = "web4",
               label = "Choose matrix type",
               choices = c("Cascadey", "Even", "Uncascadey"), 
               inline = TRUE),
             ## Cumulative sum or per cap
             radioButtons(
               inputId = "scale4",
               label = "Choose scale",
               choices = c("Per capita", "Cumulative sum"), 
               inline = TRUE),
             ## X-axis
             radioButtons(
               inputId = "x4",
               label = "X-axis",
               choices = c("Time", "Prey range of perception"), 
               inline = TRUE), 
             radioButtons(
               inputId = "range_selected4",
               label = "If you picked time, over which range of perception?",
               choices = c("HD01", "HD02", "HD03", "HD04", "HD05", "HD06", "HD07"), 
               inline = TRUE),
             ## Y-axis
             radioButtons(
               inputId = "y4",
               label = "Y-axis",
               choices = c("Abundance", "Births",
                           "Deaths", "Predation",
                           "Realised int. strength",
                           "Eaten", "Number of moves",
                           "Average dist. per move"), 
               inline = TRUE),
             ## Error bars
             radioButtons(inputId = "errorbars4", 
                          label = "Display standard deviation?", 
                          choices = c("Yes", "No"),
                          selected = "No", 
                          inline = TRUE)
         ) # end box 1
  ), # end column 2
), # end fluidrow

## Fifth row has footnotes ----------------------------------------------
### Each separated by a line break and indentation (padding-left)
fluidRow(p(span(strong(em("Notes on app:")),
                style='padding-left:5px;'),
           br(),
           span("- range of perception increases with number",
                style='padding-left:10px;'),
           br(),
           span("- More habitat 1 has all matrix types, other landscapes only have cascadey",
                style='padding-left:10px;'),
           br(),
           span("- Predation: number of predation event per herbivore species per time step/number of predators at that time step",
                style='padding-left:10px;'),
           br(),
           span("- Eaten: number of predation event per herbivore species per time step/number of herbivores of that species at that time step",
                style='padding-left:10px;'),
           br(),
           span("- Realised interaction strength: predation * proportion of pixels occupied by predators (NB: no cumulative sum for this one)",
                style='padding-left:10px;'),
           br(),
           span("- Data points taken every 10 time steps",
                style='padding-left:10px;')))


