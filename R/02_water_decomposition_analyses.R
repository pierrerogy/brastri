# Water and decomposition analyses

# Libraries
library(tidyverse)
library(here)
library(brms)
library(bayestestR)
source(here::here("brastri",
                  "functions.R"))

# Load data
## Water chemistry
water <-
  readr::read_csv(here::here("brastri", "data",
                             "water_data.csv")) %>% 
  ## Make day date and remove chlorophyll in trini
  dplyr::mutate(day = lubridate::dmy(day),
                chloro_ugL = ifelse(country == "trini",
                                    NA, chloro_ugL)) %>% 
  ## Remove tap
  dplyr::filter(bromeliad_id != "tap") %>% 
  ## Add decomposition values
  dplyr::left_join(readr::read_csv(here::here("brastri", "data",
                                              "bromeliad_data.csv")) %>% 
                     dplyr::select(country, bromeliad_id, contains(c("_dry", "_normal"))),
                   by = c("country", "bromeliad_id"))

## Aquatic communities
community <-
  readr::read_csv(here::here("brastri", "data",
                             "community_data.csv")) %>% 
  ## Remove ci columns
  dplyr::select(-contains("ci"), -size_used_mm, -bwg_name, -path, -size_original, -day,
                -stage, -abundance)
## Bromeliads
bromeliads <-
  readr::read_csv(here::here("brastri", "data",
                             "bromeliad_data.csv")) %>% 
  ## Keep experimental bromeliads only
  dplyr::filter(stringr::str_detect(string = bromeliad_id, patter = "E")) %>% 
  ## Remove the columns not needed
  dplyr::select(-contains(c("_g", "actual", "site", "mm")))

## Emergence data
emergence <- 
  readr::read_csv(here::here("brastri", "data",
                             "emergence_data.csv")) %>% 
  ## Make day date 
  dplyr::mutate(day = lubridate::dmy(day)) %>%
  ## Rename one column for the time being
  dplyr::rename(bromeliad = bromspecies,
                biomass_mg = dry_mass_mg) %>% 
  ## Make custom species name
  get_specnames() %>% 
  ## Back to original species name
  dplyr::rename(bromspecies = bromeliad)
## Keep those species for which we had at least one body mass measurement
emergence <- 
  emergence %>% 
  dplyr::filter(species %in% (emergence %>% 
                                dplyr::filter(!is.na(biomass_mg)) %>% 
                                dplyr::select(species) %>% 
                                dplyr::distinct() %>% 
                                dplyr::pull())) %>% 
  ## Now make length column numeric, add unknown to those without, and estimate biomass
  dplyr::mutate(size_mm = ifelse(is.na(size_mm) | size_mm %in% c("busted", "missing", "escaped"),
                                   "unknown", size_mm),
                bwg_name = NA,
                stage = "adult",
                biomass_type = "dry") %>% 
  ## Get biomass for missing values
  hellometry::add_taxonomy() %>% 
  hellometry::hello_metry()
  


# Treatments on water chemistry variables ---------------------------------
# Temperature
## Fit model
tempmodel <-
  brms::brm(log(temp_C)  ~
              resource*predator + (1|day) + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(tempmodel)
## Check effects
bayestestR::describe_posterior(tempmodel)
## Plot
figs1a <- 
  treatment_plot(model = tempmodel, 
                 parameter = "Temperature",  
                 scale = "log",
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence)

# pH
## Fit model
pHmodel <-
  brms::brm(log(pH)  ~
              resource*predator + (1|day) + (1|bromeliad_id/bromspecies/country),
            iter = 4000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.85,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(pHmodel)
## Check effects
bayestestR::describe_posterior(pHmodel)
## Plot
figs1b <- 
  treatment_plot(model = pHmodel, 
                 parameter = "pH", 
                 scale = "log",
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence)

# Conductivity
## Fit model
condmodel <-
  brms::brm(sqrt(conductivity_uScm)  ~
              resource*predator + (1|day) + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(condmodel)
## Check effects
bayestestR::describe_posterior(condmodel)
## Plot
figs1c <- 
  treatment_plot(model = condmodel, 
                 parameter = "Conductivity", 
                 scale = "sqrt", 
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence)

# Total P
## Fit model
tpmodel <-
  brms::brm(log(total_p_ugL)  ~
              resource*predator + (1|day) + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.88,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(tpmodel)
## Check effects
bayestestR::describe_posterior(tpmodel)
## Plot
figs1d <- 
  treatment_plot(model = tpmodel, 
                 parameter = "TP", 
                 scale = "log", 
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence)

# Phosphate ppm
## Fit model
pppmmodel <-
  brms::brm(log(phosphate_ppm + 0.001)  ~
              resource + (1|day) + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.85,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(pppmmodel)
## Check effects
bayestestR::describe_posterior(pppmmodel)
## Plot
# Prepare data
## Effect
model_effect <- 
  brms::conditional_effects(pppmmodel,
                            method = "fitted")$resource %>% 
  dplyr::mutate(estimate__ = exp(estimate__),
                lower__ = exp(lower__),
                upper__ = exp(upper__))
## Plot
figs1e <- 
  ggplot(data = water,
         aes(x = resource,
             y = phosphate_ppm + 0.0001,
             colour = resource)) + 
  geom_jitter(aes(alpha = 0.3)) +
  geom_point(size = 3,
             data = model_effect,
             aes(x = resource, 
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
  xlab("Resource") +
  scale_x_discrete(labels = c("Enriched", "Control")) +
  scale_y_continuous(trans = "log",
                     breaks = c(0.01, 1, 5, 10)) +
  ylab(axis_label(parameter = "PPPM")) +
  scale_color_manual(name = "Resource",
                     labels = c("Control", "Enriched"), 
                     values = c("tan1", "tan4")) +
  guides(alpha = "none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Chlorophyll
## Fit model
chloromodel <-
  brms::brm(log(chloro_ugL + 0.001)  ~
              resource*predator + (1|day) + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.95,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(chloromodel)
## Check effects
bayestestR::describe_posterior(chloromodel)
## Plot
figs1f <- 
  treatment_plot(model = chloromodel, 
                 parameter = "Chlorophyll-a", 
                 scale = "log", 
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence)


# Generate table with outputs
## Make table
table_1 <- 
  data.frame(
    ` ` = c("<strong>Temperature <br>ROPE =(-0.01, 0.01)</strong>", # temperature
           "<strong>pH <br>ROPE =(-0.01, 0.01)</strong>", # pH
           "<strong>Conductivity <br>ROPE =(-2.52, 2.52)</strong>", # cond
           "<strong>Total P <br>ROPE =(-0.01, 0.01)</strong>", # TP
           "<strong>Phosphate <br>ROPE =(-0.22, 0.22)</strong>", # PPPM
           "<strong>Chlorophyll-a <br>ROPE =(-0.01, 0.01)</strong>" # Chloro
    ),
    Intercept = c("<strong>3.22 (3.17, 3.28) <br>pd = 100% <br>% in ROPE = 0</strong>", # temperature
                  "<strong>1.78 (1.76, 1.81) <br>pd = 100% <br>% in ROPE = 0</strong>", # pH
                  "<strong>4.50 (3.05, 5.84) <br>pd = 100% <br>% in ROPE = 0</strong>", # cond
                  "<strong>4.70  (4.28,  5.14) <br>pd = 100% <br>% in ROPE = 0</strong>", # TP
                  "<strong> -3.52 (-5.52, -1.47) <br>pd = 95.40% <br>% in ROPE = 0.18</strong>", # PPPM
                  "<strong>-6.17 (-7.35, -5.09) <br>pd = 100% <br>% in ROPE = 0</strong>" # Chloro
    ),
    `Resource enriched` = c("<0.001 (-0.01, 0.01) <br>pd = 56.05% <br>% in ROPE = 94.34", # temperature
                 "-0.03 (-0.06, 0.00) <br>pd = 96.67% <br>% in ROPE = 10.7", # pH
                 "1.81 (0.89, 2.83) <br>pd = 99.9% <br>% in ROPE = 93.68", # cond
                 "-0.31 (-0.80,  0.18) <br>pd = 89.38% <br>% in ROPE =1.61", # TP
                 "<strong>1.82 (0.39,  3.32) <br>pd = 99.35% <br>% in ROPE = 0</strong>", # PPPM
                 "-0.18 (-1.22,  0.84) <br>pd = 64.40% <br>% in ROPE = 1.39" # Chloro
    ),
    `Predator present` = c("-0.002 (-0.02, 0.02) <br>pd = 51.58% <br>% in ROPE = 75.92", # temperature
                 "0.006 (-0.02, 0.01) <br>pd = 63.1% <br>% in ROPE = 38.74", # pH
                 "-0.44 (-1.40, 0.56) <br>pd = 82.03% <br>% in ROPE = 100", # cond
                 "<strong>-0.48  (-0.98,  0.00) <br>pd = 97.60% <br>% in ROPE = 0.11</strong>", # TP
                 "", # PPPM
                 "0.21  (-0.79,  1.21) <br>pd = 66.95% <br>% in ROPE = 1.58" # Chloro
    ),
    `Resource enrichedxPredator present` = c("0.003 (-0.02, 0.03) <br>pd = 60.45% <br>% in ROPE = 63.16", # temperature
                              "0.01 (-0.05, 0.07) <br>pd = 65.71% <br>% in ROPE = 26.21", # pH
                              "-0.31 (-1.72, 1.01) <br>pd = 69.2% <br>% in ROPE = 100",# cond
                              "0.05 (-0.65,  0.75) <br>pd = 55.97% <br>% in ROPE = 3",# TP
                              "", # PPPM
                              "1.04 (-0.37,  2.48]) <br>pd = 92.80% <br>% in ROPE = 0.32" # Chloro
    )
  )
## Save table
readr::write_csv(table_1,
                 here::here("brastri", "data",
                            "table_1.csv"))


# Generate figure
## Make figure
figs1 <- 
  cowplot::plot_grid(figs1a +
                       theme(legend.position = "none") +
                       ggtitle("a"),
                     figs1b +
                       theme(legend.position = "none") +
                       ggtitle("b"),
                     figs1c +
                       theme(legend.position = "none") +
                       ggtitle("c"),
                     figs1d +
                       theme(legend.position = "none") +
                       ggtitle("d"),
                     figs1e +
                       theme(legend.position = "none") +
                       ggtitle("e"),
                     figs1f +
                       theme(legend.position = "none") +
                       ggtitle("f"),
                     ncol = 2)
## Combine with legend
legend <- 
  cowplot::get_legend(figs1a)
figs1 <- 
  cowplot::plot_grid(legend,
                     figs1,
                     nrow = 2,
                     rel_heights = c(0.1, 0.9))
## Save figure
ggsave(here::here("brastri", "www",
                  "figs1.jpg"),
       figs1,
       width = 7,
       height = 11,
       bg = "white")


# Treatments on decomposition dry ---------------------------------
# Decomposition coarse dry
## Fit model
coarsemodel_dry <-
  brms::brm(log(prop_loss_coarse_dry)  ~
              resource*predator + (1|bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = water)
## Check assumptions
plot(coarsemodel_dry)
## Check effects
bayestestR::describe_posterior(coarsemodel_dry)
## Plot
figs2a <- 
  treatment_plot(model = coarsemodel_dry, 
                 scale = "log",
                 parameter = "coarse_dry", 
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence)

# Decomposition fine dry
## Fit model
finemodel_dry <-
  brms::brm(log(prop_loss_fine_dry)  ~
              resource*predator + (1|bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),        
            control = list(adapt_delta = 0.995,
                           max_treedepth = 15),
            data = water)

## Check assumptions
plot(finemodel_dry)
## Check effects
bayestestR::describe_posterior(finemodel_dry)
## Plot
figs2b <- 
  treatment_plot(model = finemodel_dry, 
                 parameter = "fine_dry", 
                 scale = "log",
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence)

# Generate table with outputs
## Make table
table_s2 <- 
  data.frame(
    ` ` = c("<strong>Coarse mesh decomposition <br>ROPE = (-0.01, 0.01)</strong>", # Coarse
            "<strong>Fine mesh decomposition <br>ROPE = (-0.01, 0.01)</strong>" # Fine
            ),
    Intercept = c("-0.19 (-0.71,  0.24) <br>pd = 91.57% <br>% in ROPE = 1.32", # Coarse
                  "-0.17 (-0.67,  0.39) <br>pd = 88.12% <br>% in ROPE = 1.37" # Fine
    ),
    `Resource enriched` = c("-0.009 (-0.01,  0.00) <br>pd = 100% <br>% in ROPE = 60.89", # Coarse
                            "<strong>-0.02 (-0.02, -0.01) <br>pd = 100% <br>% in ROPE = 4.37</strong>" # Fine
    ),
    `Predator present` = c("<0.001 (-0.01,  0.01) <br>pd = 51.324% <br>% in ROPE = 23.81", # Coarse
                           "<0.001 (-0.01,  0.01) <br>pd = 51.18% <br>% in ROPE = 100" # Fine
    ),
    `Resource enrichedxPredator present` = c("0.006  (0.00,  0.01) <br>pd = 92.22% <br>% in ROPE = 81.13", # Coarse
                                                 "0.009 (0.00,  0.02) <br>pd = 93.23% <br>% in ROPE = 51.92" # Fine
    )
  )
## Save table
readr::write_csv(table_s2,
                 here::here("brastri", "data",
                            "table_s2.csv"))

# Generate figure
## Make figure
figs2 <- 
  cowplot::plot_grid(figs2a +
                       theme(legend.position = "none") +
                       ggtitle("a"),
                     figs2b +
                       theme(legend.position = "none") +
                       ggtitle("b"),
                     legend,
                     ncol = 3)
## Save figure
ggsave(here::here("brastri", "www",
                  "figs2.jpg"),
       figs2,
       width = 11,
       height = 4,
       bg = "white")
 

# Treatments on decomposition dry - trying to reduce errorbars ---------------------------------
# Decomposition coarse dry
## Fit model
coarsemodel_dry <-
  brms::brm(log(prop_loss_coarse_dry)  ~
              resource + (1|bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.95,
                           max_treedepth = 15),
            data = water)
## Check assumptions
plot(coarsemodel_dry)
## Check effects
bayestestR::describe_posterior(coarsemodel_dry)
## Plot
figs1a <- 
  treatment_plot(model = coarsemodel_dry, 
                 scale = "log",
                 parameter = "coarse_dry", 
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence)

# Decomposition fine dry
## Fit model
finemodel_dry <-
  brms::brm(log(prop_loss_fine_dry)  ~
              resource*predator + (1|bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),        
            control = list(adapt_delta = 0.995,
                           max_treedepth = 15),
            data = water)

## Check assumptions
plot(finemodel_dry)
## Check effects
bayestestR::describe_posterior(finemodel_dry)
## Plot
figs2b <- 
  treatment_plot(model = finemodel_dry, 
                 parameter = "fine_dry", 
                 scale = "log",
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence)

# Generate table with outputs
## Make table
table_s2 <- 
  data.frame(
    ` ` = c("<strong>Coarse mesh decomposition <br>ROPE = (-0.01, 0.01)</strong>", # Coarse
            "<strong>Fine mesh decomposition <br>ROPE = (-0.01, 0.01)</strong>" # Fine
    ),
    Intercept = c("-0.19 (-0.71,  0.24) <br>pd = 91.57% <br>% in ROPE = 1.32", # Coarse
                  "-0.17 (-0.67,  0.39) <br>pd = 88.12% <br>% in ROPE = 1.37" # Fine
    ),
    `Resource enriched` = c("-0.009 (-0.01,  0.00) <br>pd = 100% <br>% in ROPE = 60.89", # Coarse
                            "<strong>-0.02 (-0.02, -0.01) <br>pd = 100% <br>% in ROPE = 4.37</strong>" # Fine
    ),
    `Predator present` = c("<0.001 (-0.01,  0.01) <br>pd = 51.324% <br>% in ROPE = 23.81", # Coarse
                           "<0.001 (-0.01,  0.01) <br>pd = 51.18% <br>% in ROPE = 100" # Fine
    ),
    `Resource enrichedxPredator present` = c("0.006  (0.00,  0.01) <br>pd = 92.22% <br>% in ROPE = 81.13", # Coarse
                                             "0.009 (0.00,  0.02) <br>pd = 93.23% <br>% in ROPE = 51.92" # Fine
    )
  )
## Save table
readr::write_csv(table_s2,
                 here::here("brastri", "data",
                            "table_s2.csv"))

# Generate figure
## Make figure
figs1 <- 
  cowplot::plot_grid(figs1a +
                       theme(legend.position = "none") +
                       ggtitle("a"),
                     figs1b +
                       theme(legend.position = "none") +
                       ggtitle("b"),
                     legend,
                     ncol = 3)
## Save figure
ggsave(here::here("brastri", "www",
                  "figs1.jpg"),
       figs1,
       width = 11,
       height = 4,
       bg = "white")



# Treatments on decomposition normal ----------------------------------------------------
# Decomposition coarse normal
## Fit model
coarsemodel_normal <-
  brms::brm(log(prop_loss_coarse_normal)  ~
              resource*predator + (1|day) + (1|bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = water)
## Check assumptions
plot(coarsemodel_normal)
## Check effects
bayestestR::describe_posterior(coarsemodel_normal)

# Decomposition coarse normal
## Fit model
finemodel_normal <-
  brms::brm(log(prop_loss_fine_normal)  ~
              resource*predator + (1|day) + (1|bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = water)
## Check assumptions
plot(finemodel_normal)
## Check effects
bayestestR::describe_posterior(finemodel_normal)


