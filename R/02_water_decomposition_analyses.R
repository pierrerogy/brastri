# Water and decomposition analyses

# Libraries
library(tidyverse)
library(here)
library(brms)
library(emmeans)
library(bayestestR)
source(here::here("R", 
                  "functions.R"))



# Load data ---------------------------------------------------------------
# Water chemistry
water <-
  readr::read_csv(here::here("data",
                             "water_data.csv")) %>% 
  ## Make day date and remove chlorophyll in trini
  dplyr::mutate(day = lubridate::dmy(day),
                chloro_ugL = ifelse(country == "trini",
                                    NA, chloro_ugL)) %>% 
  ## Remove tap
  dplyr::filter(bromeliad_id != "tap") %>% 
  ## Add decomposition values
  dplyr::left_join(readr::read_csv(here::here("data",
                                              "bromeliad_data.csv")) %>% 
                     dplyr::select(country, bromeliad_id, contains(c("_dry", "_normal"))),
                   by = c("country", "bromeliad_id")) %>% 
  ## Add new treatment
  dplyr::mutate(site_pred = factor(ifelse(country == "trini",
                                          "trini", ifelse(country == "bras" & predator == "present",
                                                          "bras_present", "bras_absent")),
                                   levels = c("bras_absent", "bras_present", "trini")))
## Do contrasts
contrasts(water$site_pred) <- 
  matrix(c(-0.5, -0.5, 1,
           -1, 1, 0),
         nrow = 3,
         dimnames = list(c("bras_absent", "bras_present", "trini"), 
                         c("bras_present", "trini")))

# Aquatic communities
community <-
  readr::read_csv(here::here("data",
                             "community_data.csv")) %>% 
  ## Remove ci columns
  dplyr::select(-contains("ci"), -size_used_mm, -bwg_name, -path, -size_original, -day,
                -stage, -abundance)

## Bromeliads
bromeliads <-
  readr::read_csv(here::here("data",
                             "bromeliad_data.csv")) %>% 
  ## Keep experimental bromeliads only
  dplyr::filter(stringr::str_detect(string = bromeliad_id, patter = "E")) %>% 
  ## Remove the columns not needed
  dplyr::select(-contains(c("_g", "actual", "site", "mm")))

# Emergence
emergence <- 
  readr::read_csv(here::here("data",
                             "emergence_data.csv"))
  


# Treatments on water chemistry variables ---------------------------------
# Temperature
## Fit model
tempmodel <-
  brms::brm(log(temp_C)  ~
              resource*site_pred + (1|day) + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(tempmodel)
## Check effects
t1 <- 
  pairwise_contrasts(tempmodel, 
                   both = T)
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
              resource*site_pred + (1|day) + (1|bromeliad_id/bromspecies/country),
            iter = 6000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.85,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(pHmodel)
## Check effects
t2 <- 
  pairwise_contrasts(pHmodel, 
                     both = T)
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
              resource*predator + (1|day) + (1|bromeliad_id),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.95,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(condmodel)
## Check effects
t3 <- 
  pairwise_contrasts(condmodel, 
                     bras = T)
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
              resource*predator + (1|day) + (1|bromeliad_id),
            iter = 4000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.95,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(tpmodel)
## Check effects
t4 <- 
  pairwise_contrasts(tpmodel, 
                     bras = T)
## Plot
fig2a <- 
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
  brms::brm(phosphate_ppm + 0.001  ~
              resource + (1|day) + (1|bromeliad_id/bromspecies),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.85,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(pppmmodel)
## Check effects
t5 <- 
  pairwise_contrasts(pppmmodel, 
                     trini = T)
## Plot
fig2b <- 
  treatment_plot(model = pppmmodel, 
                 parameter = "PPPM", 
                 scale = "none", 
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence,
                 trini = TRUE)
# Chlorophyll
## Fit model
chloromodel <-
  brms::brm(chloro_ugL + 0.001  ~
              resource*predator + (1|day) + (1|bromeliad_id),
            iter = 2000,
            family = lognormal(),    
            control = list(adapt_delta = 0.97,
                           max_treedepth = 10),
            data = water %>% 
              dplyr::filter(country == "bras"))
## Check assumptions
plot(chloromodel)
## Check effects
t6 <- 
  pairwise_contrasts(chloromodel, 
                     bras = T)
## Plot
fig2c <- 
  treatment_plot(model = chloromodel, 
                 parameter = "Chlorophyll-a", 
                 scale = "none", 
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence) +
  scale_y_continuous(breaks = c(-9, -7.5, -5, -2.5, 0),
                     labels = c(0, 0.001, 0.005, 0.1, 1))

# Treatments on decomposition  ---------------------------------
# Decomposition coarse dry BR
## Fit model
coarsemodeldry_BR <-
  brms::brm(prop_loss_coarse_dry  ~
              resource*predator,
            iter = 2000,
            family = gaussian(),      
            control = list(adapt_delta = 0.95,
                           max_treedepth = 15),
            data = water %>% 
              dplyr::select(prop_loss_coarse_dry, resource, predator, bromspecies, country, bromeliad_id) %>% 
              dplyr::distinct() %>% 
              dplyr::filter(country == "bras"))
## Check assumptions
plot(coarsemodeldry_BR)
## Check effects
pairwise_contrasts(coarsemodeldry_BR, 
                   bras = T)
## Plot
treatment_plot(model = coarsemodeldry_BR, 
               parameter = "coarse_dry",  
               scale = "none",
               bromeliads = bromeliads, 
               communities = communities, 
               water = water %>% 
                 dplyr::filter(country == "bras"), 
               emergence = emergence)

# Decomposition coarse dry TT
## Fit model
coarsemodeldry_TT <-
  brms::brm(prop_loss_coarse_dry  ~
              resource + (1|bromspecies),
            iter = 10000,
            family = gaussian(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = water %>% 
              dplyr::select(prop_loss_coarse_dry, resource, predator, bromspecies, country, bromeliad_id) %>% 
              dplyr::distinct() %>% 
              dplyr::filter(country == "trini"))
## Check assumptions
plot(coarsemodeldry_TT)
## Check effects
pairwise_contrasts(coarsemodeldry_TT, 
                   trini = T)
## Plot
treatment_plot(model = coarsemodeldry_TT, 
               parameter = "coarse_dry",  
               scale = "none",
               bromeliads = bromeliads, 
               communities = communities, 
               water = water %>% 
                 dplyr::select(prop_loss_coarse_dry, resource, predator, bromspecies, country, bromeliad_id) %>% 
                 dplyr::distinct() %>% 
                 dplyr::filter(country == "trini"), 
               emergence = emergence,
               trini = T)

# Decomposition fine dry BR
## Fit model
finemodeldry_BR <-
  brms::brm(prop_loss_fine_dry  ~
              resource*predator,
            iter = 2000,
            family = gaussian(link = "identity"),        
            control = list(adapt_delta = 0.95,
                           max_treedepth = 15),
            data = water %>% 
              dplyr::select(prop_loss_fine_dry, resource, predator, bromspecies, country, bromeliad_id) %>% 
              dplyr::distinct() %>% 
              dplyr::filter(country == "bras"))
## Check assumptions
plot(finemodeldry_BR)
## Check effects
t7a <- 
  pairwise_contrasts(finemodeldry_BR, 
                     bras = T)
## Plot
figs1d <- 
  treatment_plot(model = finemodeldry_BR, 
                 parameter = "fine_dry_BR",  
                 scale = "none",
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water %>% 
                   dplyr::select(prop_loss_fine_dry, resource, predator, bromspecies, country, bromeliad_id) %>% 
                   dplyr::distinct() %>% 
                   dplyr::filter(country == "bras"), 
                 emergence = emergence)

# Decomposition fine dry TT
## Fit model
finemodeldry_TT <-
  brms::brm(prop_loss_fine_dry  ~
              resource + (1|bromspecies),
            iter = 10000,
            family = gaussian(link = "identity"),        
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = water %>% 
              dplyr::select(prop_loss_fine_dry, resource, predator, bromspecies, country, bromeliad_id) %>% 
              dplyr::distinct() %>% 
              dplyr::filter(country == "trini"))
## Check assumptions
plot(finemodeldry_TT)
## Check effects
t7b <- 
  pairwise_contrasts(finemodeldry_TT, 
                     trini = T)
## Plot
fig2d <- 
  treatment_plot(model = finemodeldry_TT, 
                 parameter = "fine_dry_TT",  
                 scale = "none",
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water %>% 
                   dplyr::select(prop_loss_fine_dry, resource, predator, bromspecies, country, bromeliad_id) %>% 
                   dplyr::distinct() %>% 
                   dplyr::filter(country == "trini"), 
                 emergence = emergence,
                 trini = T)


# Decomposition inverts dry BR
## Fit model
invertmodeldry_BR <-
  brms::brm(prop_loss_invert_dry  ~
              resource*predator,
            iter = 2000,
            family = gaussian(),      
            control = list(adapt_delta = 0.95,
                           max_treedepth = 15),
            data = water %>% 
              dplyr::mutate(prop_loss_invert_dry = ifelse(prop_loss_coarse_dry - prop_loss_fine_dry > 0,
                                                          prop_loss_coarse_dry - prop_loss_fine_dry, NA)) %>% 
              dplyr::select(prop_loss_invert_dry, resource, predator, bromspecies, country, bromeliad_id) %>% 
              dplyr::distinct() %>% 
              dplyr::filter(country == "bras"))
## Check assumptions
plot(invertmodeldry_BR)
## Check effects
t8a <- 
  pairwise_contrasts(invertmodeldry_BR, 
                     bras = T)
## Plot
fig2e <- 
  treatment_plot(model = invertmodeldry_BR, 
                 parameter = "invert_dry",  
                 scale = "none",
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water %>% 
                   dplyr::mutate(prop_loss_invert_dry = ifelse(prop_loss_coarse_dry - prop_loss_fine_dry > 0,
                                                               prop_loss_coarse_dry - prop_loss_fine_dry, NA)) %>% 
                   dplyr::select(prop_loss_invert_dry, resource, predator, bromspecies, country, bromeliad_id) %>% 
                   dplyr::distinct() %>% 
                   dplyr::filter(country == "bras"), 
                 emergence = emergence)

# Decomposition inverts dry TT
## Only three non-NA values
water %>% 
  dplyr::mutate(prop_loss_invert_dry = ifelse(prop_loss_coarse_dry - prop_loss_fine_dry > 0,
                                              prop_loss_coarse_dry - prop_loss_fine_dry, NA)) %>% 
  dplyr::select(prop_loss_invert_dry, resource, predator, bromspecies, country, bromeliad_id) %>% 
  dplyr::distinct() %>% 
  dplyr::filter(country == "trini")

# Combine figures ---------------------------------------------------------
# Figure 2 - sig associations
## Get legend
legend <- 
  cowplot::get_legend(fig2a +
                        theme(legend.text = element_text(size = rel(1.2)),
                              legend.title = element_text(size = rel(1.2))))
## Generate figure
fig2 <- 
  cowplot::plot_grid(fig2a +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1.2)),
                             axis.text.x = element_text(size = rel(1.2)),
                             axis.text.y = element_text(size = rel(1.2)),
                             axis.title.y = element_text(size = rel(1.2))) +
                       ggtitle("(a)"),
                     fig2b +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1.2)),
                             axis.text.x = element_text(size = rel(1.2)),
                             axis.text.y = element_text(size = rel(1.2)),
                             axis.title.y = element_text(size = rel(1.2))) +
                       ggtitle("(b)"),
                     fig2c +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1.2)),
                             axis.text.x = element_text(size = rel(1.2)),
                             axis.text.y = element_text(size = rel(1.2)),
                             axis.title.y = element_text(size = rel(1.2))) +
                       ggtitle("(c)"),
                     fig2d +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1.2)),
                             axis.text.x = element_text(size = rel(1.2)),
                             axis.text.y = element_text(size = rel(1.2)),
                             axis.title.y = element_text(size = rel(1.2))) +
                       ggtitle("(d)"),
                     fig2e +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1.2)),
                             axis.text.x = element_text(size = rel(1.2)),
                             axis.text.y = element_text(size = rel(1.2)),
                             axis.title.y = element_text(size = rel(1.2))) +
                       ggtitle("(e)"),
                     legend,
                     ncol = 2)
## Save figure
ggsave(here::here("plots",
                  "fig2.jpg"),
       fig2,
       width = 10,
       height = 11,
       bg = "white")

  
# Generate figure
## Make figure
figs1 <- 
  cowplot::plot_grid(figs1a +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1.2)),
                             axis.text.x = element_text(size = rel(1.2)),
                             axis.text.y = element_text(size = rel(1.2)),
                             axis.title.y = element_text(size = rel(1.2))) +
                       ggtitle("(a)"),
                     figs1b +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1.2)),
                             axis.text.x = element_text(size = rel(1.2)),
                             axis.text.y = element_text(size = rel(1.2)),
                             axis.title.y = element_text(size = rel(1.2))) +
                       ggtitle("(b)"),
                     figs1c +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1.2)),
                             axis.text.x = element_text(size = rel(1.2)),
                             axis.text.y = element_text(size = rel(1.2)),
                             axis.title.y = element_text(size = rel(1.2))) +
                       ggtitle("(c)"),
                     figs1d +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1.2)),
                             axis.text.x = element_text(size = rel(1.2)),
                             axis.text.y = element_text(size = rel(1.2)),
                             axis.title.y = element_text(size = rel(1.2))) +
                       ggtitle("(d)"),
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
ggsave(here::here("plots",
                  "figs1.jpg"),
       figs1,
       width = 10,
       height = 11,
       bg = "white")

