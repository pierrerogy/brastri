# Functions for app

# Libraries
library(tidyverse)

# Select data -------------------------------------------------------------
get_those_dats <- function(y = "none", x = "none", 
                           bromeliads, communities, 
                           water, emergence){

  # Bromeliad width
  if(x == "Bromeliad width"){
    ret <- 
      bromeliads %>% 
      dplyr::rename(x = width_cm)
  }
  
  # Bromeliad height
  if(x == "Bromeliad height"){
      ret <- 
        bromeliads %>% 
        dplyr::rename(x = height_cm)
    }
  
  # Water holding capacity
  if(x == "Water holding capacity"){
        ret <- 
          bromeliads %>% 
          dplyr::rename(x = water_holding_capacity_mL)
  }
  
  # pH
  if(y == "pH"){
    ret <- 
      water %>% 
      dplyr::rename(y = pH,
                    x = day) 
  }

  # Conductivity
  if(y == "Conductivity"){
    ret <- 
      water %>% 
      dplyr::rename(y = conductivity_uScm,
                    x = day) 
  }
  
  # Conductivity
  if(y == "Temperature"){
    ret <- 
      water %>% 
      dplyr::rename(y = temp_C,
                    x = day) 
  }
  
  # Chlorophyll concentration
  if(y == "Chlorophyll-a"){
    ret <- 
      water %>% 
      dplyr::rename(y = chloro_ugL,
                    x = day) 
  }
  
  # Phosphate (Here the trick is to compile the two different columns)
  if(y == "Phosphorus"){
    ret <-
      water %>%
      dplyr::mutate(y = ifelse(!is.na(total_p_ugL),
                               total_p_ugL, phosphate_ppm)) %>%
      dplyr::rename(x = day)
  }
  
  # Return data
  return(ret)
  
}


# Make axis label ---------------------------------------------------------
axis_label <- function(parameter){
  # Just a giant list of if..else
  if(parameter == "Bromeliad width")
    ret <- 
      "Bromeliad width (cm)"
  
  if(parameter == "Bromeliad height")
    ret <- 
      "Bromeliad height (cm)"
  
  if(parameter == "Water holding capacity")
    ret <- 
      "Water holding capacity (mL)"
  
  if(parameter == "pH")
    ret <- 
      "pH"
  
  if(parameter == "Conductivity")
    ret <-  
      expression(paste("Conductivity (", mu, "S"*".cm"^"-2"*")"))
  
  if(parameter == "Temperature")
    ret <- 
      "Temperature (°C)"
  
  if(parameter == "Chlorophyll-a")
    ret <- 
      expression(paste("Chlorophyll-a concentration (", mu ,"g"*".L"^"-1"*")"))
  
  if(parameter == "Phosphorus")
    ret <-
      expression(paste("[BR] Total P (", mu, "mol"*".L"^"-1"*") --- [TT] PO"["4"]^"3-"*" (ppm)"))
  
  # Return
  return(ret)
  
  
}



# Make density plot -------------------------------------------------------
density_plot <- function(dats, x){
  # Get axis label
  x_label <- 
    axis_label(parameter = x)
  
  # Make plot
  ret <- 
    ggplot(dats, 
           aes(x = x)) + 
    geom_density(fill = "darkgoldenrod1") +
    facet_wrap(~ country,
               labeller = labeller(country = 
                                     c("bras" = "BR",
                                       "trini" = "TT"))) +
    xlab(x_label) +
    ylab("Frequency") + 
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) 
  
  # Return plot
  return(ret)
  
}


# Make line plot ----------------------------------------------------------
time_plot <- function(dats, y){
  # Get axis label
  y_label <- 
    axis_label(parameter = y)
  
  # Make initial plot
  ret <- 
    ggplot(dats,
           aes(x = x,
               y = y,
               group = bromeliad_id,
               lty = predator,
               col = resource))
  
  
  
  # Fork for phosphorus
  if(y %in% c("Phosphorus", "Chlorophyll-a")){
  ret <- 
    ret +
    facet_wrap(~ country,
               scales = "free",
               labeller = labeller(country = 
                                     c("bras" = "BR",
                                       "trini" = "TT")))} else
                 ret <- 
                   ret +
                   facet_wrap(~ country,
                              scales = "free_x",
                              labeller = labeller(country = 
                                                    c("bras" = "BR",
                                                      "trini" = "TT")))
  
  # Back to making the plot
  ret <- 
    ret +
    geom_line()  +
    xlab("Date (2022)") +
    ylab(y_label) + 
    scale_linetype_manual(name = "Predator",
                            labels = c("Absent", "Present"), 
                            values = c(1, 2)) +
    scale_colour_manual(name = "Resource",
                          labels = c("Enriched", "Control"), 
                          values = c("tan4", "tan1")) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) 
    
  # Return plot
  return(ret)
  
  
  
  
}


  


# Compute species name based on row taxonomy ------------------------------

get_specnames <- function(df, short = FALSE){
  
  # List of possible taxa
  taxa <- 
    c("domain", "kingdom", "phylum", "subphylum", "class", "subclass", 
      "ord", "subord", "family", "subfamily", "tribe", "genus", "species")
  
  # Loop in row by row
  for(i in 1:nrow(df)){
    ## Get row
    row <- 
      df[i,] %>% 
      ## Remove n
      dplyr::select(tidyselect::matches(taxa)) %>% 
      ## Make vector
      purrr::flatten_chr() %>% 
      ## Remove NAs
      na.exclude()
    
    ## Extract species name and last taxonomic level without NA
    taxo <- 
      row[length(row)-1]
    
    name <- 
      row[length(row)]
    
    ## Shorten name if asked
    if(short == TRUE){
      ### First remove mystery part from string
      name <- 
        stringr::str_replace(string = name,
                             pattern = "mystery",
                             replacement = "")
      
      ### Only keep first five characters of species name
      taxo <- 
        stringr::str_sub(string = taxo, 
                         start = 1, 
                         end = 5)
      
      
    }
    
    ## Paste together
    name <- 
      paste0(c(taxo, name),
             collapse = "_")
    
    ## Replace in df
    df[i, "species"] <- 
      name
    
  }
  
  ## Return
  return(df)
  
}


# Not in ------------------------------------------------------------------
# Not in
`%notin%` <- 
  Negate(`%in%`)


