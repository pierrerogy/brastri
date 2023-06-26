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
  
  # Total phosphorus
  if(y == "TP"){
    ret <-
      water %>%
      dplyr::mutate(y = total_p_ugL) %>%
      dplyr::rename(x = day)
  }
  
  # Phosphate ppm
  if(y == "PPPM"){
    ret <-
      water %>%
      dplyr::mutate(y = phosphate_ppm) %>%
      dplyr::rename(x = day)
  }
  
  # Coarse bag decomposition - dry
  if(y == "coarse_dry"){
    ret <-
      water %>%
      dplyr::mutate(y = prop_loss_coarse_dry)
  }
  
  # Fine bag decomposition - dry
  if(y == "fine_dry"){
    ret <-
      water %>%
      dplyr::mutate(y = prop_loss_fine_dry)
  }
  
  # Coarse bag decomposition - wet
  if(y == "coarse_normal"){
    ret <-
      water %>%
      dplyr::mutate(y = prop_loss_coarse_normal)
  }
  
  # Fine bag decomposition - wet
  if(y == "fine_normal"){
    ret <-
      water %>%
      dplyr::mutate(y = prop_loss_fine_normal)
  }
  
  # Scirtes
  if(x == "Scirtes"){
    ret <- 
      communities %>% 
      dplyr::filter(genus == "Scirtes") %>% 
      dplyr::rename(x = size_original)
  }
  
  # Tipulidae
  if(x == "Tipulidae"){
    ret <- 
      communities %>% 
      dplyr::filter(family == "Tipulidae") %>% 
      dplyr::rename(x = size_original)
  }
  
  # Polypedilum
  if(x == "Polypedilum"){
    ret <- 
      communities %>% 
      dplyr::filter(genus == "Polypedilum") %>% 
      dplyr::rename(x = size_original)
  }
  
  # Tanypodinae
  if(x == "Tanypodinae"){
    ret <- 
      communities %>% 
      dplyr::filter(subfamily == "Tanypodinae") %>% 
      dplyr::rename(x = size_original)
  }
  
  # Wyeomyia
  if(x == "Wyeomyia"){
    ret <- 
      communities %>% 
      dplyr::filter(genus == "Wyeomyia") %>% 
      dplyr::rename(x = size_original)
  }
  
  # Odonata
  if(x == "Coenagrionidae"){
    ret <- 
      communities %>% 
      dplyr::filter(ord == "Odonata") %>% 
      dplyr::rename(x = size_original)
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
  
  if(parameter == "TP")
    ret <-
      expression(paste("Total P (", mu, "mol"*".L"^"-1"*")"))
  
  if(parameter == "PPPM")
    ret <-
      expression(paste("PO"["4"]^"3-"*" (ppm)"))
  
  if(parameter == "coarse_dry")
    ret <- 
      "Mass loss proportion in coarse mesh bag (wet-dry)"
  
  if(parameter == "fine_dry")
    ret <- 
      "Mass loss proportion in fine mesh bag (wet-dry)"
  
  if(parameter == "coarse_normal")
    ret <- 
      "Mass loss proportion in coarse mesh bag (wet-wet)"
  
  if(parameter == "fine_normal")
    ret <- 
      "Mass loss proportion in fine mesh bag (wet-wet)"
  
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


# Make size histogram -----------------------------------------------------
size_histogram <- function(dats, x){
  
  # Prepare dats to always have full histogram
  dats <- 
    prep_dats_hist(dats)
  
  # Make plot
  ret <- 
    ggplot(dats, 
           aes(x = x)) + 
    geom_density(fill = "darkgoldenrod1") +
    facet_grid(cols = vars(when),
               rows = vars(country), 
               labeller = labeller(country = 
                                     c("bras" = "BR",
                                       "trini" = "TT"),
                                   when = 
                                     c("start" = "Beginning",
                                       "end" = "End"))) +
    ylab("Frequency") + 
    xlab("") +
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
                          labels = c("Control", "Enriched"), 
                          values = c("tan1", "tan4")) +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black")) 
    
  # Return plot
  return(ret)
  
  
  
  
}


# Make treatment plot -----------------------------------------------------
treatment_plot <- function(model, parameter, scale = "none",
                           bromeliads, communities, 
                           water, emergence){
  # Prepare data
  ## Effect
  model_effect <- 
    brms::conditional_effects(model,
                              method = "fitted")$`resource:predator`
  ### Rescale
  if(scale == "log"){
    model_effect <- 
      model_effect %>% 
      dplyr::mutate(estimate__ = exp(estimate__),
                    lower__ = exp(lower__),
                    upper__ = exp(upper__))
  }
  if(scale == "sqrt"){
    model_effect <- 
      model_effect %>% 
      dplyr::mutate(estimate__ = (estimate__)^2,
                    lower__ = (lower__)^2,
                    upper__ = (upper__)^2)
  } 
  ## Raw
  dats <- 
    get_those_dats(y = parameter,
                   bromeliads = bromeliads, 
                   communities = communities, 
                   water = water, 
                   emergence = emergence)

  # Get axes label
  ylab <- 
    axis_label(parameter = parameter)
  
  # Plot
  ## Little catch for log scale of y axis
  if(parameter == "Chlorophyll-a") {
    ret <- 
      ggplot(data = water,
             aes(x = predator,
                 y = y,
                 colour = resource)) + 
      geom_jitter(aes(alpha = 0.3)) +
      geom_point(size = 3,
                 data = model_effect,
                 aes(x = predator, 
                     y = estimate__,
                     colour = resource), 
                 position = position_dodge(0.5)) +
      geom_errorbar(data = model_effect,
                    aes(ymin = lower__, 
                        ymax = upper__,
                        colour = resource), 
                    width = 0.2,
                    position = position_dodge(0.5)) +
    ggtitle("") +
    xlab("Predator") +
    scale_x_discrete(labels = c("Absent", "Present")) +
    scale_y_continuous(trans = "log",
                       breaks = c(0.01, 0.5, 1))+  
    ylab(ylab) +
      scale_color_manual(name = "Resource",
                         labels = c("Control", "Enriched"), 
                         values = c("tan1", "tan4")) +
    guides(alpha="none") +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(), 
          axis.line = element_line(colour = "black"))} else
    {ret <- 
      ggplot(data = model_effect,
             aes(x = predator, 
                 y = estimate__), 
             colour = resource) + 
      geom_point(size = 4,
                 aes(colour = resource),
                 position = position_dodge(0.5)) +
      geom_errorbar(aes(ymin = lower__, 
                        ymax = upper__,
                        colour = resource), 
                    width = 0.2,
                    position = position_dodge(0.5)) +
      geom_jitter(data = dats,
                 aes(x = predator,
                     y = y,
                     colour = resource,
                     alpha = 0.3)) +
      ggtitle("") +
      xlab("Predator") +
      scale_x_discrete(labels = c("Absent", "Present")) +
      ylab(ylab) +
      scale_color_manual(name = "Resource",
                         labels = c("Control", "Enriched"), 
                         values = c("tan1", "tan4")) +
      guides(alpha="none") +
      theme(panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            panel.background = element_blank(), 
            axis.line = element_line(colour = "black"))}
    
    # Return 
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

# Make table names nicer ---------------------------------------------------
make_names_nicer <- function(table){
  # Make table ret
  ret <- 
    table
  
  # Convert . to space and add line break
  names(ret) <- 
    names(ret) %>% 
    stringr::str_replace_all(pattern = "\\.",
                         replacement = " ") %>% 
    stringr::str_replace(pattern = "x",
                         replacement = "  x<br> ")
  
  # Add line break 
  
  
  # Return
  return(ret)
  
  
  
}



# Prep data for histogram --------------------------------------------------
prep_dats_hist <- function(dats){
  
  ret <- 
    dats %>% 
    dplyr::filter(!is.na(x)) %>% 
    ## Select relevant columns and make empty rows
    dplyr::select(country, when, x) %>% 
    dplyr::bind_rows(tibble::tibble(country = c("trini", "trini", "bras", "bras"),
                                    when = c("start", "end", "start", "end"),
                                    x = rep(NA, 4))) %>%
    ## Male size numeric, remove NAs
    dplyr::mutate(when = factor(when, levels = c("start", "end")),
                                x = as.numeric(x))
  
  ## Return
  return(ret)
  
  
}

