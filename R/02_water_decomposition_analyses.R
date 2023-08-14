# Water and decomposition analyses

# Libraries
library(tidyverse)
library(here)
library(brms)
library(bayestestR)
source(here::here("brastri",
                  "functions.R"))



# Load data ---------------------------------------------------------------
# Water chemistry
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

# Emergence
emergence <- 
  readr::read_csv(here::here("brastri", "data",
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
              resource*site_pred + (1|day) + (1|bromeliad_id/bromspecies/country),
            iter = 6000,
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
              resource*predator + (1|day) + (1|bromeliad_id),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.95,
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
              resource*predator + (1|day) + (1|bromeliad_id),
            iter = 4000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.95,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(tpmodel)
## Check effects
bayestestR::describe_posterior(tpmodel)
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
  brms::brm(log(phosphate_ppm + 0.001)  ~
              resource + (1|day) + (1|bromeliad_id/bromspecies),
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
fig2b <- 
  treatment_plot(model = pppmmodel, 
                 parameter = "PPPM", 
                 scale = "log", 
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence,
                 trini = TRUE)

# Chlorophyll
## Fit model
chloromodel <-
  brms::brm(log(chloro_ugL + 0.001)  ~
              resource*predator + (1|day) + (1|bromeliad_id),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.97,
                           max_treedepth = 10),
            data = water %>% 
              dplyr::filter(country == "bras"))
## Check assumptions
plot(chloromodel)
## Check effects
bayestestR::describe_posterior(chloromodel)
## Plot
figs1d <- 
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
    ` ` = c("<strong>Temperature <br>ROPE = (-0.01, 0.01)</strong>", # temperature
           "<strong>pH <br>ROPE = (-0.01, 0.01)</strong>", # pH
           "<strong>Conductivity <br>ROPE = (-2.52, 2.52)</strong>", # cond
           "<strong>Total P <br>ROPE = (-0.01, 0.01)</strong>", # TP
           "<strong>Phosphate <br>ROPE = (-0.1, 0.1)</strong>", # PPPM
           "<strong>Chlorophyll-a <br>ROPE = (-0.01, 0.01)</strong>" # Chloro
    ),
    `Intercept` = c("<strong>3.22 (3.17, 3.28) <br>pd = 100% <br>% in ROPE = 0</strong>", # temperature
                  "<strong>1.78 (1.76, 1.81) <br>pd = 100% <br>% in ROPE = 0</strong>", # pH
                  "<strong>4.49 (3.10, 5.85) <br>pd = 100% <br>% in ROPE = 0</strong>", # cond
                  "<strong>4.69  (4.14,  5.08) <br>pd = 100% <br>% in ROPE = 0</strong>", # TP
                  "<strong> -3.52 (-5.48, -1.49) <br>pd = 99.88% <br>% in ROPE = 0.18</strong>", # PPPM
                  "<strong>-6.18 (-7.29, -5.12]) <br>pd = 100% <br>% in ROPE = 0</strong>" # Chloro
    ),
    `Resource Enriched` = c("0.002 (-0.01, 0.01) <br>pd = 64.92% <br>% in ROPE = 96.03", # temperature
                           "-0.02 (-0.03, 0.04) <br>pd = 94.78% <br>% in ROPE = 16.75", # pH
                           "1.81 (0.95, 2.70) <br>pd = 100% <br>% in ROPE = 96.42", # cond
                           "-0.31 (-0.75,  0.13) <br>pd = 91.00% <br>% in ROPE = 1.66", # TP
                           "<strong>1.82 (0.42,  3.28) <br>pd = 99.48% <br>% in ROPE = 0</strong>", # PPPM
                           "-0.20 (-1.16,  0.74) <br>pd = 66.17% <br>% in ROPE = 1.47" # Chloro
    ),
    `Present Regua` = c("-0.002 (-0.01, 0.01) <br>pd = 62.68% <br>% in ROPE = 98.11", # temperature
                        "-0.002 (-0.03, 0.02) <br>pd = 58.14% <br>% in ROPE = 58.77", # pH
                        "-0.31 (-1.27, 0.43) <br>pd = 83.83% <br>% in ROPE = 100",# cond
                        "<strong>-0.47 (-0.92, -0.04) <br>pd = 98.05% <br>% in ROPE = 0</strong>",# TP
                        "", # PPPM
                        "0.20 (-0.75,  1.16]) <br>pd = 66.88% <br>% in ROPE = 1.53" # Chloro
    ),
    `Simla` = c("0.02 (-0.05, 0.11) <br>pd = 78.38% <br>% in ROPE = 17.39", # temperature
                "0.001 (-0.02, 0.01) <br>pd = 54.28% <br>% in ROPE = 50.09", # pH
                "", # cond
                "", # TP
                "", # PPPM
                "" # Chloro
    ),
    `Present ReguaxResource Enriched` = c("0.003 (-0.02, 0.03) <br>pd = 60.45% <br>% in ROPE = 87.71", # temperature
                                           "0.003 (-0.03, 0.04) <br>pd = 56.74% <br>% in ROPE = 44.39", # pH
                                           "-0.37 (-1.58, 0.82) <br>pd = 72.62% <br>% in ROPE = 100",# cond
                                           "0.05 (-0.65,  0.75) <br>pd = 55.97% <br>% in ROPE = 3",# TP
                                           "", # PPPM
                                           "1.06 (-0.34,  2.43) <br>pd = 93.38%% <br>% in ROPE = 0.32" # Chloro
    ),
    `SimlaxResource Enriched` = c("<0.001 (-0.01, 0.01) <br>pd = 50.05% <br>% in ROPE = 88.89", # temperature
                                  "-0.008 (-0.04, 0.03) <br>pd = 68.47% <br>% in ROPE = 41.23", # pH
                                  "",# cond
                                  "",# TP
                                  "", # PPPM
                                  "" # Chloro
    )
  )
## Save table
readr::write_csv(table_1,
                 here::here("brastri", "data",
                            "table_1.csv"))

# Treatments on decomposition  ---------------------------------
# Decomposition coarse dry
## Fit model
coarsemodeldry <-
  brms::brm(log(prop_loss_coarse_dry)  ~
              resource*site_pred,
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.85,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(coarsemodeldry)
## Check effects
bayestestR::describe_posterior(coarsemodeldry)
## Plot
fig2c <- 
  treatment_plot(model = coarsemodeldry, 
                 parameter = "coarse_dry",  
                 scale = "log",
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence)

# Decomposition fine dry BR
## Fit model
finemodeldry <-
  brms::brm(log(prop_loss_fine_dry)  ~
              resource*site_pred,
            iter = 2000,
            family = gaussian(link = "identity"),        
            control = list(adapt_delta = 0.85,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(finemodeldry)
## Check effects
bayestestR::describe_posterior(finemodeldry)
## Plot
fig2d <- 
  treatment_plot(model = finemodeldry, 
                 parameter = "fine_dry",  
                 scale = "log",
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence)

# Generate table with outputs
## Make table
table_2 <- 
  data.frame(
    ` ` = c("<strong>Coarse mesh decomposition<br>ROPE = (-0.01, 0.01)</strong>", # Coarse
            "<strong>Fine mesh decomposition<br>ROPE = (-0.01, 0.01)</strong>" # Fine
            ),
    `Intercept` = c("<strong>-0.26 (-0.26, -0.26) <br>pd = 100% <br>% in ROPE = 0.</strong>", # Coarse
                    "<strong>-0.26 (-0.27, -0.26) <br>pd = 100% <br>% in ROPE = 0.</strong>" # Fine
    ),
    `Resource Enriched` = c("-0.008 (-0.02, -0.004) <br>pd = 100% <br>% in ROPE = 80.67", # Coarse
                            "-0.01 (-0.02, -0.01) <br>pd = 100% <br>% in ROPE = 32.26" # Fine
                            ),
    `Present Regua` = c("<strong>0.11 (0.11,  0.12) <br>pd = 100% <br>% in ROPE = 0</strong>",# Coarse
                        "<strong>0.13 (0.13,  0.14) <br>pd = 100% <br>% in ROPE = 0</strong>" # Fine
    ),
    `Simla` = c("-0.003 (-0.01,  0.001) <br>pd = 100% <br>% in ROPE = 100", # Coarse
                "-0.001 (-0.006,  0.003) <br>pd = 92.88% <br>% in ROPE = 100" # Fine
    ),
    `Present ReguaxResource Enriched` = c("0.006 (0.002, 0.01) <br>pd = 92.88% <br>% in ROPE = 94.18", # Coarse
                                          "0.002 (0.002, 0.02) <br>pd = 65.05% <br>% in ROPE = 100" # Fine
    ),
    `SimlaxResource Enriched` = c("0.008 (0.003, 0.01) <br>pd = 99.92% <br>% in ROPE = 71.84", # Coarse
                                  "0.007 (0.003, 0.01) <br>pd = 98.02% <br>% in ROPE = 79.92" # Fine
    )
  )
## Save table
readr::write_csv(table_2,
                 here::here("brastri", "data",
                            "table_2.csv"))




# Combine figures ---------------------------------------------------------
# Figure 2 - sig associations
## Generate figure
fig2 <- 
  cowplot::plot_grid(fig2a +
                       theme(legend.position = "none") +
                       ggtitle("a"),
                     fig2b +
                       theme(legend.position = "none") +
                       ggtitle("b"),
                     fig2c +
                       theme(legend.position = "none") +
                       ggtitle("c"),
                     fig2d +
                       theme(legend.position = "none") +
                       ggtitle("d"),
                     ncol = 2)
## Combine with legend
legend <- 
  cowplot::get_legend(fig2a)
fig2 <- 
  cowplot::plot_grid(legend,
                     fig2,
                     nrow = 2,
                     rel_heights = c(0.1, 0.9))
## Save figure
ggsave(here::here("brastri", "www",
                  "fig2.jpg"),
       fig2,
       width = 10,
       height = 11,
       bg = "white")

  
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
       width = 10,
       height = 11,
       bg = "white")

