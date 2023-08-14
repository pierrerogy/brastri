# Invertebrate analyses

# Libraries
library(tidyverse)
library(ggtext)
library(here)
library(brms)
library(bayestestR)
source(here::here("brastri",
                  "functions.R"))

# remove.packages(c("rstan","StanHeaders"))
# install.packages("StanHeaders", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
# install.packages("rstan", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))

# Load data ---------------------------------------------------------------
# Aquatic communities
community <-
  readr::read_csv(here::here("brastri", "data",
                             "community_data.csv")) %>% 
  ## Remove ci columns
  dplyr::select(-contains("ci_"), -path,  -day,
                -abundance) %>% 
  ## Add new treatment
  dplyr::mutate(site_pred = factor(ifelse(country == "trini",
                                          "trini", ifelse(country == "bras" & predator == "present",
                                                          "bras_present", "bras_absent")),
                                   levels = c("bras_absent", "bras_present", "trini")))
## Do contrasts
contrasts(community$site_pred) <- 
  matrix(c(-0.5, -0.5, 1,
           -1, 1, 0),
         nrow = 3,
         dimnames = list(c("bras_absent", "bras_present", "trini"), 
                         c("bras_present", "trini")))

# Bromeliads
bromeliads <-
  readr::read_csv(here::here("brastri", "data",
                             "bromeliad_data.csv")) %>% 
  ## Keep experimental bromeliads only
  dplyr::filter(stringr::str_detect(string = bromeliad_id, 
                                    pattern = "E")) %>% 
  ## Remove the columns not needed
  dplyr::select(-contains(c("_g", "actual", "site", "mm"))) %>% 
  ## Add new treatment
  dplyr::mutate(site_pred = factor(ifelse(country == "trini",
                                          "trini", ifelse(country == "bras" & predator == "present",
                                                          "bras_present", "bras_absent")),
                                   levels = c("bras_absent", "bras_present", "trini")))
## Do contrasts
contrasts(community$site_pred) <- 
  matrix(c(-0.5, -0.5, 1,
           -1, 1, 0),
         nrow = 3,
         dimnames = list(c("bras_absent", "bras_present", "trini"), 
                         c("bras_present", "trini")))

# Emergence data
emergence <- 
  readr::read_csv(here::here("brastri", "data",
                             "emergence_data.csv")) %>% 
  ## Rename one column for the time being
  dplyr::rename(bromeliad = bromspecies,
                biomass_mg = dry_mass_mg) %>% 
  ## Make custom species name
  get_specnames() %>% 
  ## Back to original species name
  dplyr::rename(bromspecies = bromeliad) %>% 
  ## Get day number of emergence
  dplyr::mutate(day = lubridate::yday(lubridate::dmy(day))) %>% 
  ## Convert to nday to emergence by substracting day number of day of set up
  dplyr::mutate(ndays = ifelse(country == "bras",
                               day - 91, day -  280)) %>% 
## Add new treatment
dplyr::mutate(site_pred = factor(ifelse(country == "trini",
                                        "trini", ifelse(country == "bras" & predator == "present",
                                                        "bras_present", "bras_absent")),
                                 levels = c("bras_absent", "bras_present", "trini")))
## Do contrasts
contrasts(community$site_pred) <- 
  matrix(c(-0.5, -0.5, 1,
           -1, 1, 0),
         nrow = 3,
         dimnames = list(c("bras_absent", "bras_present", "trini"), 
                         c("bras_present", "trini")))

## Keep those species for which we had at least one body mass measurement
emergence_selected <- 
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

# Models on total emerged biomass -----------------------------------------------
# Overall biomass
## Fit model
emergencemodel_all <-
  brms::brm(biomass_mg ~
              resource*site_pred + (1|bromspecies/country),
            iter = 2000,
            family = skew_normal(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_emergence(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "all") %>% 
              dplyr::filter(biomass_mg <10))
## Check assumptions
plot(emergencemodel_all)
## Check effects
bayestestR::describe_posterior(emergencemodel_all)
## Plot
figs2a <- 
  treatment_plot(model = emergencemodel_all, 
                 scale = "none",
                 parameter = "emergence_all", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = summarise_emergence(dats = emergence_selected,
                                                bromeliads = bromeliads,
                                                group = "all") %>% 
                   dplyr::filter(biomass_mg <10))

# Seeded biomass
## Fit model
emergencemodel_seed <-
  brms::brm(biomass_mg ~
              resource*site_pred + (1|bromspecies/country),
            iter = 2000,
            family = skew_normal(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_emergence(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "seed") %>% 
              dplyr::filter(biomass_mg <10))
## Check assumptions
plot(emergencemodel_seed)
## Check effects
bayestestR::describe_posterior(emergencemodel_seed)
## Plot
figs2b <- 
  treatment_plot(model = emergencemodel_seed, 
                 scale = "none",
                 parameter = "emergence_seed", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = summarise_emergence(dats = emergence_selected,
                                                 bromeliads = bromeliads,
                                                 group = "seed") %>% 
                   dplyr::filter(biomass_mg <10))

# Chironomidae
## Fit model
emergencemodel_chiro <-
  brms::brm(biomass_mg ~
              resource*site_pred + (1|bromspecies/country),
            iter = 2000,
            family = skew_normal(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_emergence(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "Chiro"))
## Check assumptions
plot(emergencemodel_chiro)
## Check effects
bayestestR::describe_posterior(emergencemodel_chiro)
## Plot
fig3a <- 
  treatment_plot(model = emergencemodel_chiro, 
                 scale = "none",
                 parameter = "emergence_chir_tot", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = summarise_emergence(dats = emergence_selected,
                                                 bromeliads = bromeliads,
                                                 group = "Chiro"))

# Culicidae
## Fit model
emergencemodel_culi <-
  brms::brm(biomass_mg ~
              resource*predator,
            iter = 2000,
            family = skew_normal(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_emergence(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "Culi") %>% 
              dplyr::filter(country == "bras"))
## Check assumptions
plot(emergencemodel_culi)
## Check effects
bayestestR::describe_posterior(emergencemodel_culi)
## Plot
figs2c <- 
  treatment_plot(model = emergencemodel_culi, 
                 scale = "none",
                 parameter = "emergence_culi_tot", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = summarise_emergence(dats = emergence_selected,
                                                 bromeliads = bromeliads,
                                                 group = "Culi") %>% 
                   dplyr::filter(country == "bras"))

# Tipulidae
## Fit model
emergencemodel_tipu <-
  brms::brm(biomass_mg~
              resource*site_pred + (1|bromspecies/country),
            iter = 2000,
            family = skew_normal(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_emergence(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "Tipu"))
## Check assumptions
plot(emergencemodel_tipu)
## Check effects
bayestestR::describe_posterior(emergencemodel_tipu)
## Plot
figs2d <- 
  treatment_plot(model = emergencemodel_tipu, 
                 scale = "none",
                 parameter = "emergence_tipu_tot", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = summarise_emergence(dats = emergence_selected,
                                                 bromeliads = bromeliads,
                                                 group = "Tipu"))

# Ceratopogonidae
## Fit model
emergencemodel_cera <-
  brms::brm(biomass_mg ~
              resource*predator,
            iter = 2000,
            family = skew_normal(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_emergence(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "Cera") %>% 
              dplyr::filter(country == "bras"))
## Check assumptions
plot(emergencemodel_cera)
## Check effects
bayestestR::describe_posterior(emergencemodel_cera)
## Plot
figs2e <- 
  treatment_plot(model = emergencemodel_cera, 
                 scale = "none",
                 parameter = "emergence_cera_tot", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = summarise_emergence(dats = emergence_selected,
                                                 bromeliads = bromeliads,
                                                 group = "Cera") %>% 
                   dplyr::filter(country == "bras"))

# Table summarising results
## Make table 
table_4 <- 
  data.frame(
    ` ` = c("<strong>Total emerged biomass <br>ROPE = (-0.6, 0.6)</strong>", # total
            "<strong>Emerged biomass from <br> seeded groups <br>ROPE = (-0.06, 0.06)</strong>", # seed
            "<strong>Emerged chironomid biomass<br>ROPE = (-0.01, 0.01)</strong>", # chiro
            "<strong>Emerged culicid biomass<br>ROPE = (-0.03, 0.03)</strong>", # culi
            "<strong>Emerged tipulid biomass<br>ROPE = (-0.05, 0.05)</strong>", # tipu
            "<strong>Emerged ceratopogonid biomass<br>ROPE = (-0.005, 0.005)</strong>" # cera
    ),
    `Intercept` = c("0.75 (-2.56, 3.50) <br>pd = 81.850% <br>% in ROPE = 1.63", # total
                    "0.68 (-2.39, 3.36) <br>pd = 80.17% <br>% in ROPE = 2.92", # seed
                    "0.14 (-1.45, 2.38) <br>pd = 74.62% <br>% in ROPE = 0.92", # chiro
                    "<strong>0.41 (0.21, 0.67) <br>pd = 100% <br>% in ROPE = 0</strong>", # culi
                    "0.28 (-2.80, 2.89) <br>pd = 69.12% <br>% in ROPE = 4.79", # tipu
                    "0.04 (-1.50, 1.47) <br>pd = 64.04% <br>% in ROPE = 2.17</strong>" # cera
    ),
    `Resource Enriched` = c("-0.08 (-0.52, 0.30) <br>pd = 66.95% <br>% in ROPE = 25.84", # total
                            "-0.08 (-0.51, 0.29) <br>pd = 68.85% <br>% in ROPE = 26.37", # seed
                            "-0.05 (-1.45,  2.38) <br>pd = 95.58% <br>% in ROPE = 6.21", # chiro
                            "-0.09 (-0.51, 0.16) <br>pd = 75.78% <br>% in ROPE = 18.32", # culi
                            "-0.006 (-0.30, 0.26) <br>pd = 51.55% <br>% in ROPE = 32.08", # tipu
                            "-0.003 (-0.02, 0.02) <br>pd = 63.44% <br>% in ROPE = 37.25" # cera
    ),
    `Present Regua` = c("-0.05 (-0.43, 0.33) <br>pd = 60.17% <br>% in ROPE = 29.26", # total
                        "-0.08 (-0.45, 0.29]) <br>pd = 67.88% <br>% in ROPE = 25.95", # seed
                        "<strong>-0.07 (-0.14, -0.02) <br>pd = 99.30%% <br>% in ROPE = 0</strong>",# chiro
                        "-0.08 (-0.42, 0.18) <br>pd = 70.62% <br>% in ROPE = 19.21",# culi
                        "0.02 (-0.22, 0.29) <br>pd = 58.38% <br>% in ROPE = 34.42", # tipu
                        "-0.007 (-0.03, 0.02) <br>pd = 73.14% <br>% in ROPE = 31.58" # cera
    ),
    `Simla` = c("-0.33 (-3.82, 3.50) <br>pd = 68.65% <br>% in ROPE = 6.16", # total
                "-0.28 (-4.02, 3.23) <br>pd = 66.07%% <br>% in ROPE = 7.26", # seed
                "-0.09 (-2.61, 1.74) <br>pd = 68.58% <br>% in ROPE = 2.58", # chiro
                "", # culi
                "-0.006 (-3.35, 3.69) <br>pd = 50.40% <br>% in ROPE = 7.29", # tipu
                "" # cera
    ),
    `Present ReguaxResource Enriched` = c("0.04 (-0.53, 0.60) <br>pd = 54.52% <br>% in ROPE = 19.42", # total
                                          "0.06 (-0.47, 0.63) <br>pd = 59.98% <br>% in ROPE = 20.87", # seed
                                          "<strong>0.09 (0.01,  0.19) <br>pd = 98.75% <br>% in ROPE = 0</strong>",# chiro
                                          "0.09 (-0.29, 0.62) <br>pd = 67.60% <br>% in ROPE = 13.76",# culi
                                          "-0.02 (-0.39, 0.38) <br>pd = 54.30% <br>% in ROPE = 21.61", # tipu
                                          "0.004 (-0.03, 0.04) <br>pd = 60.66% <br>% in ROPE = 26.27" # cera
    ),
    `SimlaxResource Enriched` = c("0.05 (-0.42, 0.58) <br>pd = 57.55% <br>% in ROPE = 21.95", # total
                                  "0.05 (-0.42, 0.56) <br>pd = 59.03% <br>% in ROPE = 22.11", # seed
                                  "0.05 (0.02, 0.12) <br>pd = 90.70% <br>% in ROPE = 9.76",# chiro
                                  "",# culi
                                  "-0.02 (-0.35, 0.32) <br>pd = 55.6% <br>% in ROPE = 24.97", # tipu
                                  "" # cera
    )
  )

## Save table
readr::write_csv(table_4,
                 here::here("brastri", "data",
                            "table_4.csv"))

# Models on individual emerged body mass ----------------------------------
# Chironomidae
## Fit model
indemergencemodel_chiro <-
  brms::brm(biomass_mg~
              resource*site_pred + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = emergence_selected %>% 
              dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
              dplyr::filter(stringr::str_detect(string = species,
                                                pattern = "Chiro")))
## Check assumptions
plot(indemergencemodel_chiro)
## Check effects
bayestestR::describe_posterior(indemergencemodel_chiro)
## Plot
fig3b <- 
  treatment_plot(model = indemergencemodel_chiro, 
                 scale = "none",
                 parameter = "emergence_chir_ind", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = emergence_selected %>% 
                   dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
                   dplyr::filter(stringr::str_detect(string = species,
                                                     pattern = "Chiro")))

# Culicidae
## Fit model
indemergencemodel_culi <-
  brms::brm(biomass_mg~
              resource*predator + (1|bromeliad_id),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = emergence_selected %>% 
              dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
              dplyr::filter(stringr::str_detect(string = species,
                                                pattern = "Culi")))
## Check assumptions
plot(indemergencemodel_culi)
## Check effects
bayestestR::describe_posterior(indemergencemodel_culi)
## Plot
fig3c <- 
  treatment_plot(model = indemergencemodel_culi, 
                 scale = "none",
                 parameter = "emergence_culi_ind", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = emergence_selected %>% 
                   dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
                   dplyr::filter(stringr::str_detect(string = species,
                                                     pattern = "Culi")))

# Tipulidae
## Fit model
indemergencemodel_tipu <-
  brms::brm(biomass_mg~
              resource*site_pred + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.95,
                           max_treedepth = 10),
            data = emergence_selected %>% 
              dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
              dplyr::filter(stringr::str_detect(string = species,
                                                pattern = "Tipu")))
## Check assumptions
plot(indemergencemodel_tipu)
## Check effects
bayestestR::describe_posterior(indemergencemodel_tipu)
## Plot
figs2f <- 
  treatment_plot(model = indemergencemodel_tipu, 
                 scale = "none",
                 parameter = "emergence_tipu_ind", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = emergence_selected %>% 
                   dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
                   dplyr::filter(stringr::str_detect(string = species,
                                                     pattern = "Tipu")))

# Ceratopogonidae
## Fit model
indemergencemodel_cera <-
  brms::brm(biomass_mg~
              resource*predator + (1|bromeliad_id),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = emergence_selected %>% 
              dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
              dplyr::filter(stringr::str_detect(string = species,
                                                pattern = "Cera")))
## Check assumptions
plot(indemergencemodel_cera)
## Check effects
bayestestR::describe_posterior(indemergencemodel_cera)
## Plot
figs2g <- 
  treatment_plot(model = indemergencemodel_cera, 
                 scale = "none",
                 parameter = "emergence_cera_ind", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = emergence_selected %>% 
                   dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
                   dplyr::filter(stringr::str_detect(string = species,
                                                     pattern = "Cera")))

# Table summarising results
## Make table
table_5 <- 
  data.frame(
    ` ` = c("<strong>Emerged chironomid body mass<br>ROPE = (-0.005, 0.005)</strong>", # chiro
            "<strong>Emerged culicid body mass<br>ROPE = (-0.01, 0.01)</strong>", # culi
            "<strong>Emerged tipulid body mass<br>ROPE = (-0.04, 0.04)</strong>", # tipu
            "<strong>Emerged ceratopogonid body mass<br>ROPE = (-0.005, 0.005)</strong>" # cera
    ),
    `Intercept` = c("<strong>0.07 (0.04, 0.09) <br>pd = 100% <br>% in ROPE = 0</strong>", # chiro
                    "<strong>0.26 (0.18, 0.34) <br>pd = 100% <br>% in ROPE = 0</strong>", # culi
                    "<strong>0.85 (-0.13, 1.89) <br>pd = 95.78% <br>% in ROPE = 1.37</strong>", # tipu
                    "<strong>0.04 (0.02, 0.06) <br>pd = 100% <br>% in ROPE = 0</strong>" # cera
    ),
    `Resource Enriched` = c("'0.002 (-0.04, 0.03) <br>pd = 55.33% <br>% in ROPE = 19.53", # chiro
                            "-0.08 (-0.21, 0.05) <br>pd = 89.95% <br>% in ROPE = 7.45", # culi
                            "-0.57 (-1.72, 0.61) <br>pd = 84.82% <br>% in ROPE = 3.26", # tipu
                            "0.02 (-0.01, 0.05) <br>pd = 94.03% <br>% in ROPE = 4.32" # cera
    ),
    `Present Regua` = c("-0.02 (-0.06, 0.02) <br>pd = 87.10% <br>% in ROPE = 9.29",# chiro
                        "-0.08 (-0.21, 0.04) <br>pd = 91.00% <br>% in ROPE = 7.87",# culi
                        "-0.23 (-1.45, 1.03) <br>pd = 65.42% <br>% in ROPE = 5.87", # tipu
                        "-0.004 (-0.03, 0.02) <br>pd = 62.12% <br>% in ROPE = 14" # cera
    ),
    `Simla` = c("-0.01 (-0.05, 0.02) <br>pd = 76.00% <br>% in ROPE = 14.08", # chiro
                "", # culi
                "0.02 (-1.08, 1.13) <br>pd = 51.88% <br>% in ROPE = 6.76", # tipu
                "" # cera
    ),
    `Present ReguaxResource Enriched` = c("<strong>0.06 (0.00, 0.11) <br>pd = 97.88% <br>% in ROPE = 0.11</strong>",# chiro
                                          "<strong>0.17 (-0.02, 0.36) <br>pd = 95.55% <br>% in ROPE = 2.68</strong>",# culi
                                          "0.72 (-0.85, 2.33) <br>pd = 83.75% <br>% in ROPE = 2.42", # tipu
                                          "-0.02 (-0.06, 0.02) <br>pd = 87.92% <br>% in ROPE = 5.16" # cera
    ),
    `SimlaxResource Enriched` = c("0.001 (-0.06, 0.05) <br>pd = 51.32% <br>% in ROPE = 11.82",# chiro
                                  "",# culi
                                  "0.20 (-1.15, 1.55) <br>pd = 61.22% <br>% in ROPE = 4.79", # tipu
                                  "" # cera
    )
  )

## Save table
readr::write_csv(table_5,
                 here::here("brastri", "data",
                            "table_5.csv"))


# Models on growth rate ---------------------------------------------------
# Chironomidae
## Fit model
indgrowthmodel_chiro <-
  brms::brm(ndays~
              resource*site_pred + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = emergence_selected %>% 
              dplyr::filter(stringr::str_detect(string = species,
                                                pattern = "Chiro")))
## Check assumptions
plot(indgrowthmodel_chiro)
## Check effects
bayestestR::describe_posterior(indgrowthmodel_chiro)
## Plot
figs3a <- 
  treatment_plot(model = indgrowthmodel_chiro, 
                 scale = "none",
                 parameter = "growth_chir_ind", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = emergence_selected %>% 
                   dplyr::filter(stringr::str_detect(string = species,
                                                     pattern = "Chiro")))

# Culicidae
## Fit model
indgrowthmodel_culi <-
  brms::brm(ndays~
              resource*predator + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = emergence_selected %>% 
              dplyr::filter(stringr::str_detect(string = species,
                                                pattern = "Culi")))
## Check assumptions
plot(indgrowthmodel_culi)
## Check effects
bayestestR::describe_posterior(indgrowthmodel_culi)
## Plot
figs3b <- 
  treatment_plot(model = indgrowthmodel_culi, 
                 scale = "none",
                 parameter = "growth_culi_ind", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = emergence_selected %>% 
                   dplyr::filter(stringr::str_detect(string = species,
                                                     pattern = "Culi")))

# Tipulidae
## Fit model
indgrowthmodel_tipu <-
  brms::brm(ndays~
              resource*site_pred + (1|bromeliad_id/bromspecies/country),
            iter = 4000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.95,
                           max_treedepth = 10),
            data = emergence_selected %>% 
              dplyr::filter(stringr::str_detect(string = species,
                                                pattern = "Tipu")))
## Check assumptions
plot(indgrowthmodel_tipu)
## Check effects
bayestestR::describe_posterior(indgrowthmodel_tipu)
## Plot
figs3c <- 
  treatment_plot(model = indgrowthmodel_tipu, 
                 scale = "none",
                 parameter = "growth_tipu_ind", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = emergence_selected %>% 
                   dplyr::filter(stringr::str_detect(string = species,
                                                     pattern = "Tipu")))

# Ceratopogonidae
## Fit model
indgrowthmodel_cera <-
  brms::brm(ndays~
              resource*predator + (1|bromeliad_id),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = emergence_selected %>% 
              dplyr::filter(stringr::str_detect(string = species,
                                                pattern = "Cera")))
## Check assumptions
plot(indgrowthmodel_cera)
## Check effects
bayestestR::describe_posterior(indgrowthmodel_cera)
## Plot
figs3d <- 
  treatment_plot(model = indgrowthmodel_cera, 
                 scale = "none",
                 parameter = "growth_cera_ind", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = emergence_selected %>% 
                   dplyr::filter(stringr::str_detect(string = species,
                                                     pattern = "Cera")))


# Generate figure
## Make figure
figs3 <- 
  cowplot::plot_grid(figs3a +
                       theme(legend.position = "none") +
                       ggtitle("a"),
                     figs3b +
                       theme(legend.position = "none") +
                       ggtitle("b"),
                     figs3c +
                       theme(legend.position = "none") +
                       ggtitle("c"),
                     figs3d +
                       theme(legend.position = "none") +
                       ggtitle("d"),
                     ncol = 2)
## Combine with legend
legend <- 
  cowplot::get_legend(figs3a)
figs3 <- 
  cowplot::plot_grid(legend,
                     figs3,
                     nrow = 2,
                     rel_heights = c(0.1, 0.9))

## Save figure
ggsave(here::here("brastri", "www",
                  "figs3.jpg"),
       figs3,
       width = 7,
       height = 9,
       bg = "white")


# Table summarising results
## Make table
table_6 <- 
  data.frame(
    ` ` = c("<strong>Chironomid time to emergence<br>ROPE = (-0.74, 0.74)</strong>", # chiro
            "<strong>Culicid time to emergence<br>ROPE = (-0.77, 0.077)</strong>", # culi
            "<strong>Tipulid time to emergence<br>ROPE = (-0.77, 0.77)</strong>", # tipu
            "<strong>Ceratopogonid time to emergence<br>ROPE = (-0.01, 0.01)</strong>" # cera
    ),
    `Intercept` = c("<strong>16.13 (11.47, 20.80) <br>pd = 100% <br>% in ROPE = 0</strong>", # chiro
                    "<strong>13.24 (7.76, 18.69) <br>pd = 100% <br>% in ROPE = 0</strong>", # culi
                    "<strong>17.36 (-0.81, 35.87) <br>pd = 97.08% <br>% in ROPE = 1.01</strong>", # tipu
                    "<strong>18.14 (13.76, 22.41) <br>pd = 100% <br>% in ROPE = 0</strong>" # cera
    ),
    `Resource Enriched` = c("0.12 (-7.05,  7.03) <br>pd = 51.65% <br>% in ROPE = 18.3", # chiro
                            "-0.58 (-8.70,  7.82) <br>pd = 54.97% <br>% in ROPE = 13.68", # culi
                            "-2.49 (-24.27, 19.37) <br>pd = 59.26% <br>% in ROPE = 5.66", # tipu
                            "-1.16 (-7.53,  5.96) <br>pd = 62.85% <br>% in ROPE = 14.32" # cera
    ),
    `Present Regua` = c("-2.98 (-10.59,  4.25) <br>pd = 80.03% <br>% in ROPE = 11.95",# chiro
                        "4.19 (-3.67, 11.99) <br>pd = 85.62% <br>% in ROPE = 9.11",# culi
                        "1.56 (-21.85, 23.44) <br>pd = 55.84% <br>% in ROPE = 5.68", # tipu
                        "0.01 (-6.76,  7.16) <br>pd = 50.15% <br>% in ROPE = 16.18" # cera
    ),
    `Simla` = c("1.10 (-6.10,  8.32) <br>pd = 62.55% <br>% in ROPE = 16.03", # chiro
                "", # culi
                "-4.93 (-25.48, 15.08) <br>pd = 69.92% <br>% in ROPE = 5.79", # tipu
                "" # cera
    ),
    `Present ReguaxResource Enriched` = c("5.55 (-4.39, 15.72) <br>pd = 85.95% <br>% in ROPE = 6.92",# chiro
                                          "-3.70 (-15.54,  8.02) <br>pd = 74.25% <br>% in ROPE = 9.11",# culi
                                          "0.82 (-27.60, 29.62) <br>pd = 52.18% <br>% in ROPE = 4.38", # tipu
                                          "-1.35 (-11.23,  8.71) <br>pd = 61.22% <br>% in ROPE = 10.95" # cera
    ),
    `SimlaxResource Enriched` = c("-1.86 (-12.37,  8.55) <br>pd = 64.45% <br>% in ROPE = 10.47",# chiro
                                  "",# culi
                                  "2.96 (-21.42, 28.08) <br>pd = 60.08% <br>% in ROPE = 5.54", # tipu
                                  "" # cera
    )
  )

## Save table
readr::write_csv(table_6,
                 here::here("brastri", "data",
                            "table_6.csv"))



# Models on proportion emerging -------------------------------------------
# Seeded individuals
## Fit model
propemergencemodel_seed <-
  brms::brm(prop ~
              resource*site_pred + (1|bromspecies/country),
            iter = 2000,
            family = zero_inflated_beta(),    
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_proportion(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "seed"))
## Check assumptions
plot(propemergencemodel_seed)
## Check effects
bayestestR::describe_posterior(propemergencemodel_seed)
## Plot
figs4a <- 
  treatment_plot(model = propemergencemodel_seed, 
                 scale = "none",
                 parameter = "prop_seed", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = summarise_proportion(dats = emergence_selected,
                                                 bromeliads = bromeliads,
                                                 group = "seed"))


# Culicidae
## Fit model
propemergencemodel_culi <-
  brms::brm(prop ~
              resource*predator,
            iter = 2000,
            family = zero_inflated_beta(),    
            control = list(adapt_delta = 0.98,
                           max_treedepth = 15),
            data = summarise_proportion(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "Culi"))
## Check assumptions
plot(propemergencemodel_culi)
## Check effects
bayestestR::describe_posterior(propemergencemodel_culi)
## Plot
figs4b <- 
  treatment_plot(model = propemergencemodel_culi, 
                 scale = "none",
                 parameter = "prop_culi", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = summarise_proportion(dats = emergence_selected,
                                                 bromeliads = bromeliads,
                                                 group = "Culi"))

# Tipulidae
## Fit model
propemergencemodel_tipu <-
  brms::brm(prop ~
              resource*site_pred + (1|bromspecies/country),
            iter = 5000,
            family = zero_one_inflated_beta(),    
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_proportion(dats = emergence_selected,
                                        bromeliads = bromeliads,
                                        group = "Tipu"))
## Check assumptions
plot(propemergencemodel_tipu)
## Check effects
bayestestR::describe_posterior(propemergencemodel_tipu)
## Plot
fig3d <- 
  treatment_plot(model = propemergencemodel_tipu, 
                 scale = "none",
                 parameter = "prop_tipu", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = summarise_proportion(dats = emergence_selected,
                                                 bromeliads = bromeliads,
                                                 group = "Tipu"))


# Figure of all results
# Generate figure
## Make figure
figs4 <- 
  cowplot::plot_grid(figs4a +
                       theme(legend.position = "none") +
                       ggtitle("a"),
                     figs4b +
                       theme(legend.position = "none") +
                       ggtitle("b"),
                     ncol = 2)
## Combine with legend
legend <- 
  cowplot::get_legend(figs4a)
figs4 <- 
  cowplot::plot_grid(legend,
                     figs4,
                     nrow = 2,
                     rel_heights = c(0.1, 0.9))

## Save figure
ggsave(here::here("brastri", "www",
                  "figs4.jpg"),
       figs4,
       width = 10,
       height = 6,
       bg = "white")

# Table summarising results
## Make table
table_7 <- 
  data.frame(
    ` ` = c("<strong>Proportion of seeded<br>larvae emerged<br>ROPE = (-0.01, 0.01)</strong>", # seed
            "<strong>Proportion of culicid<br>larvae emerged<br>ROPE = (-0.18, 0.18)</strong>", # culi
            "<strong>Proportion of tipulid<br>larvae emerged<br>ROPE = (-0.18, 0.18)</strong>" # tipu
    ),
    `Intercept` = c("-1.18 (-5.76, 4.38) <br>pd = 73.55% <br>% in ROPE = 4.82", # seed
                    "-0.49 (-1.08, 0.11) <br>pd = 94.83% <br>% in ROPE = 13.2", # culi
                    "0.78 (-3.96,  6.07) <br>pd = 71.46% <br>% in ROPE = 8.60" # tipu
    ),
    `Resource Enriched` = c("-0.34 (-1.01, 0.30) <br>pd = 85.52% <br>% in ROPE = 26.82", # seed
                            "-0.62 (-1.46, 0.20) <br>pd = 92.77% <br>% in ROPE = 12.79", # culi
                            "<strong>-1.38 (-2.52, -0.40) <br>pd = 99.49% <br>% in ROPE = 0</strong>" # tipu
    ),
    `Present Regua` = c("-0.21 (-0.79, 0.43) <br>pd = 77.15% <br>% in ROPE = 38.47",# seed
                        "-0.32 (-1.14, 0.50) <br>pd = 77.98% <br>% in ROPE = 26.08",# culi
                        "<strong>-1.41 (-2.89, -0.18) <br>pd = 98.60% <br>% in ROPE = 0.02</strong>" # tipu
    ),
    `Simla` = c("-0.71 (-7.90, 6.68) <br>pd = 64.58% <br>% in ROPE = 7.05", # seed
                "", # culi
                "-1.40 (-9.03,  5.19) <br>pd = 77.16% <br>% in ROPE = 4.57" # tipu
    ),
    `Present ReguaxResource Enriched` = c("0.40 (-0.52, 1.28) <br>pd = 64.58% <br>% in ROPE = 21.95",# seed
                                          "0.52 (0.72, 1.74) <br>pd = 79.45% <br>% in ROPE = 17.68",# culi
                                          "<strong>1.40 (-0.09,  3.10) <br>pd = 96.85% <br>% in ROPE = 2.52</strong>" # tipu
    ),
    `SimlaxResource Enriched` = c("0.51 (-0.56, 1.56) <br>pd = 84.28% <br>% in ROPE = 17.00",# seed
                                  "",# culi
                                  "<strong>1.71 (0.60,  2.97) <br>pd = 99.72% <br>% in ROPE = 0</strong>" # tipu
    )
  )

## Save table
readr::write_csv(table_7,
                 here::here("brastri", "data",
                            "table_7.csv"))

# Models on total final biomass -------------------------------------------------
# Prepare data
community_end <- 
  community %>% 
  dplyr::filter(when == "end" & !(class == "Hexapoda" & stage == "adult")) %>% 
  ## Make a column for seeded or not
  dplyr::mutate(seeded = ifelse(family == "Tipulidae" | genus == "Polypedilum" |
                                  (country == "bras" & genus == "Wyeomyia") | 
                                  (country == "bras" & subfamily == "Tanypodinae") | 
                                  (country == "bras" & ord == "Odonata") |
                                  (country == "trini" & genus == "Scirtes"),
                                "seeded", "tourist")) %>% 
  dplyr::mutate(seeded = ifelse(is.na(seeded),
                                "tourist", seeded)) %>% 
  ## Convert biomass to numeric
  dplyr::mutate(biomass_mg = as.numeric(biomass_mg))

# Overall biomass
## Fit model
leftovermodel_all <-
  brms::brm(biomass_mg~
              resource*site_pred + (1|bromspecies/country),
            iter = 2000,
            family = skew_normal(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_leftover(dats = community_end,
                                       bromeliads = bromeliads,
                                       group = "all"))
## Check assumptions
plot(leftovermodel_all)
## Check effects
bayestestR::describe_posterior(leftovermodel_all)
## Plot
figs5a <- 
  treatment_plot(model = leftovermodel_all, 
                 scale = "none",
                 parameter = "leftover_all", 
                 bromeliads = bromeliads, 
                 communities = summarise_leftover(dats = community_end,
                                                   bromeliads = bromeliads,
                                                   group = "all"), 
                 water = water, 
                 emergence = emergence)

# Seeded biomass
## Fit model
leftovermodel_seed <-
  brms::brm(biomass_mg~
              resource*site_pred + (1|bromspecies/country),
            iter = 2000,
            family = skew_normal(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_leftover(dats = community_end,
                                       bromeliads = bromeliads,
                                       group = "seed"))
## Check assumptions
plot(leftovermodel_seed)
## Check effects
bayestestR::describe_posterior(leftovermodel_seed)
## Plot
figs5b <- 
  treatment_plot(model = leftovermodel_seed, 
                 scale = "none",
                 parameter = "leftover_seed", 
                 bromeliads = bromeliads, 
                 communities = summarise_leftover(dats = community_end,
                                                  bromeliads = bromeliads,
                                                  group = "seed"), 
                 water = water, 
                 emergence = emergence)


# Chironomidae
## Fit model
leftovermodel_chiro <-
  brms::brm(biomass_mg ~
              resource + (1|bromspecies),
            iter = 2000,
            family = skew_normal(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_leftover(dats = community_end,
                                       bromeliads = bromeliads,
                                       group = "Poly") %>% 
              dplyr::filter(country == "trini"))
## Check assumptions
plot(leftovermodel_chiro)
## Check effects
bayestestR::describe_posterior(leftovermodel_chiro)
## Plot
figs5c <- 
  treatment_plot(model = leftovermodel_chiro, 
                 scale = "none",
                 parameter = "leftover_chir_tot", 
                 bromeliads = bromeliads, 
                 communities = summarise_leftover(dats = community_end,
                                                  bromeliads = bromeliads,
                                                  group = "Chiro") %>% 
                   dplyr::filter(country == "trini"), 
                 water = water, 
                 emergence = emergence,
                 trini = T)

# Culicidae
## Fit model
leftovermodel_culi <-
  brms::brm(biomass_mg~
              resource*predator,
            iter = 2000,
            family = skew_normal(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_leftover(dats = community_end,
                                       bromeliads = bromeliads,
                                       group = "Culi") %>% 
              dplyr::filter(country == "bras"))
## Check assumptions
plot(leftovermodel_culi)
## Check effects
bayestestR::describe_posterior(leftovermodel_culi)
## Plot
figs5d <- 
  treatment_plot(model = leftovermodel_culi, 
                 scale = "none",
                 parameter = "leftover_culi_tot", 
                 bromeliads = bromeliads, 
                 communities = summarise_leftover(dats = community_end,
                                                  bromeliads = bromeliads,
                                                  group = "Culi"), 
                 water = water, 
                 emergence = emergence)

# Tipulidae
## Fit model
leftovermodel_tipu <-
  brms::brm(biomass_mg ~
              resource*predator,
            iter = 2000,
            family = skew_normal(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_leftover(dats = community_end,
                                       bromeliads = bromeliads,
                                       group = "Tipu"))
## Check assumptions
plot(leftovermodel_tipu)
## Check effects
bayestestR::describe_posterior(leftovermodel_tipu)
## Plot
figs5e <- 
  treatment_plot(model = leftovermodel_tipu, 
                 scale = "none",
                 parameter = "leftover_tipu_tot", 
                 bromeliads = bromeliads, 
                 communities = summarise_leftover(dats = community_end,
                                                  bromeliads = bromeliads,
                                                  group = "Tipu"), 
                 water = water, 
                 emergence = emergence)

# Scirtids
## Fit model
leftovermodel_scir <-
  brms::brm(biomass_mg~
              resource + (1|bromspecies),
            iter = 2000,
            family = skew_normal(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_leftover(dats = community_end,
                                       bromeliads = bromeliads,
                                       group = "Scir") %>% 
              dplyr::filter(country == "trini"))
## Check assumptions
plot(leftovermodel_scir)
## Check effects
bayestestR::describe_posterior(leftovermodel_scir)
## Plot
figs5f <- 
  treatment_plot(model = leftovermodel_chiro, 
                 scale = "none",
                 parameter = "leftover_scir_tot", 
                 bromeliads = bromeliads, 
                 communities = summarise_leftover(dats = community_end,
                                                  bromeliads = bromeliads,
                                                  group = "Scir") %>% 
                   dplyr::filter(country == "trini"), 
                 water = water, 
                 emergence = emergence,
                 trini = T)

# Table summarising results
## Make table
table_8 <- 
  data.frame(
    ` ` = c("<strong>Total leftover biomass<br>ROPE = (-0.49, 0.49)</strong>", # total
            "<strong>Leftover biomass from<br>seeded organisms<br>ROPE = (-0.01, 0.01)</strong>", # seed
            "<strong>Leftover chironomid biomass<br>ROPE = (-0.02, 0.02)</strong>", # chiro
            "<strong>Leftover culicid biomass<br>ROPE = (-0.02, 0.02)</strong>", # culi
            "<strong>Leftover tipulid biomass<br>ROPE = (-0.03, 0.03)</strong>", # tipu
            "<strong>Leftover culicid biomass<br>ROPE = (-0.11, 0.11)</strong>" # scir
    ),
    `Intercept` = c("2.69 (-3.79, 6.90) <br>pd = 86.30% <br>% in ROPE = 8.24", # total
                    "0.78 (-3.24, 4.28) <br>pd = 73.60% <br>% in ROPE = 5.71", # seed
                    "0.23 (-0.39, 0.99) <br>pd = 89.12% <br>% in ROPE = 2.26", # chiro
                    "<strong>0.18 (0.08, 0.27) <br>pd = 99.95% <br>% in ROPE = 0</strong>", # culi
                    "<strong>0.15 (0.08, 0.21) <br>pd = 100 <br>% in ROPE = 0</strong>", # tipu
                    "<strong>1.22 (0.11, 2.40) <br>pd = 97.85% <br>% in ROPE = 0.16</strong>" # scir
    ),
    `Resource Enriched` = c("0.12 (-3.06, 3.02) <br>pd = 53.33% <br>% in ROPE = 30.18", # total
                            "0.009 (-0.99, 0.94) <br>pd = 51.02% <br>% in ROPE = 25.34", # seed
                            "-0.05 (-0.16, 0.04) <br>pd = 87.72% <br>% in ROPE = 27.11", # chiro
                            "-0.02 (-0.16, 0.10) <br>pd = 64.98% <br>% in ROPE = 31.79", # culi
                            "-0.003 (-0.09, 0.07) <br>pd = 52.88% <br>% in ROPE = 53.58", # tipu
                            "-0.12 (-0.68, 0.46) <br>pd = 67.90% <br>% in ROPE = 30.76" # scir
    ),
    `Present Regua` = c("0.17 (-2.59, 3.25) <br>pd = 62.68% <br>% in ROPE = 31.26", # total
                        "0.40 (-0.22, 1.38) <br>pd = 89.30% <br>% in ROPE = 17.34", # seed
                        "",# chiro
                        "-0.04 (-0.20, 0.08) <br>pd = 77.58% <br>% in ROPE = 25.71",# culi
                        "0.02 (-0.08, 0.10) <br>pd = 68.47% <br>% in ROPE = 41.05", # tipu
                        "0.20 (-0.77,  1.21) <br>pd = 66.27% <br>% in ROPE = 1.61" # scir
    ),
    `Simla` = c("1.59 (-4.31, 8.88) <br>pd = 76.80% <br>% in ROPE = 13.03", # total
                "0.69 (-4.00, 5.90) <br>pd = 71.47% <br>% in ROPE = 6.58", # seed
                "", # chiro
                "", # culi
                "", # tipu
                "" # scir
    ),
    `Present ReguaxResource Enriched` = c("-0.01 (-4.29, 4.02) <br>pd = 50.45% <br>% in ROPE = 21.42", # total
                                          "-0.24 (-1.52, 0.95) <br>pd = 66.47% <br>% in ROPE = 17.87", # seed
                                          "",# chiro
                                          "0.01 (-0.20, 0.23) <br>pd = 55.97% <br>% in ROPE = 20.61",# culi
                                          "0.02 (-0.11, 0.15) <br>pd = 62.62% <br>% in ROPE = 31.63", # tipu
                                          "" # scir
    ),
    `SimlaxResource Enriched` = c("-0.19 (-3.70, 3.30) <br>pd = 54.43% <br>% in ROPE = 25.71", # total
                                  "-0.23 (-1.33, 0.92) <br>pd = 67.07% <br>% in ROPE = 19.37", # seed
                                  "",# chiro
                                  "",# culi
                                  "", # tipu
                                  "" # scir
    )
  )

## Save table
readr::write_csv(table_8,
                 here::here("brastri", "data",
                            "table_8.csv"))

# Models on individual leftover body mass ----------------------------------
# Chironomidae ONLY FROM SIMLA IN CONTROL BROMELIADS
## Fit model
community_end %>% 
              dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
              dplyr::filter(stringr::str_detect(string = genus,
                                                pattern = "Poly"))

# Culicidae
## Fit model
indleftovermodel_culi <-
  brms::brm(biomass_mg~
              resource*predator + (1|bromeliad_id),
            iter = 5000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.93,
                           max_treedepth = 10),
            data = community_end %>% 
              dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
              dplyr::filter(stringr::str_detect(string = family,
                                                pattern = "Culi") & 
                              seeded == "seeded"))
## Check assumptions
plot(indleftovermodel_culi)
## Check effects
bayestestR::describe_posterior(indleftovermodel_culi)
## Plot
fig4a <- 
  treatment_plot(model = indleftovermodel_culi, 
                 scale = "none",
                 parameter = "leftover_culi_ind", 
                 bromeliads = bromeliads, 
                 communities = community_end %>% 
                   dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
                   dplyr::filter(stringr::str_detect(string = family,
                                                     pattern = "Culi") & 
                                   seeded == "seeded"), 
                 water = water, 
                 emergence = emergence)

# Tipulidae
## Fit model
indleftovermodel_tipu <-
  brms::brm(log(biomass_mg) ~
              resource*predator + (1|bromeliad_id),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.93,
                           max_treedepth = 10),
            data = community_end %>% 
              dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
              dplyr::filter(stringr::str_detect(string = family,
                                                pattern = "Tipu")))
## Check assumptions
plot(indleftovermodel_tipu)
## Check effects
bayestestR::describe_posterior(indleftovermodel_tipu)
## Plot
figs5g <- 
  treatment_plot(model = indleftovermodel_tipu, 
                 scale = "log",
                 parameter = "leftover_tipu_ind", 
                 bromeliads = bromeliads, 
                 communities = community_end %>% 
                   dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
                   dplyr::filter(stringr::str_detect(string = family,
                                                     pattern = "Tipu")), 
                 water = water, 
                 emergence = emergence)

# Scirtidae
## Fit model
indleftovermodel_scir <-
  brms::brm(biomass_mg~
              resource + (1|bromeliad_id/bromspecies),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = community_end %>% 
              dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
              dplyr::filter(stringr::str_detect(string = genus,
                                                pattern = "Scir")))
## Check assumptions
plot(indleftovermodel_scir)
## Check effects
bayestestR::describe_posterior(indleftovermodel_scir)
## Plot
fig4b <- 
  treatment_plot(model = indleftovermodel_scir, 
                 scale = "none",
                 parameter = "leftover_scir_ind", 
                 bromeliads = bromeliads, 
                 communities = community_end %>% 
                   dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
                   dplyr::filter(stringr::str_detect(string = family,
                                                     pattern = "Scir")), 
                 water = water, 
                 emergence = emergence,
                 trini = T)
# Table summarising results
## Make table
table_9 <- 
  data.frame(
    ` ` = c("<strong>Leftover culicid body mass<br>ROPE = (-0.01, 0.01)</strong>", # culi
            "<strong>Leftover tipulid body mass<br>ROPE = (-0.01, 0.01)</strong>", # tipu
            "<strong>Leftover scirtid body mass<br>ROPE = (-0.1, 0.1)</strong>" # scir
    ),
    `Intercept` = c("<strong>0.14 (0.01,  0.27) <br>pd = 98.04% <br>% in ROPE = 0.02</strong>", # culi
                    "-1.08 (-3.09, 0.98) <br>pd = 6.00% <br>% in ROPE = 0.39", # tipu
                    "<strong>0.18 (0.11, 0.27) <br>pd = 100% <br>% in ROPE = 0</strong>" # scir
    ),
    `Resource Enriched` = c("<strong>0.19 (-0.02,  0.42) <br>pd = 96.59% <br>% in ROPE = 1.31</strong>", # culi
                            "0.74 (-2.80, 4.03) <br>pd = 67.38% <br>% in ROPE = 0.45", # tipu
                            "<strong>0.12 (-0.01, 0.24) <br>pd = 97.08% <br>% in ROPE = 2.45</strong>" # scir
    ),
    `Present Regua` = c("<strong>0.28 (-0.05,  0.61) <br>pd = 95.95% <br>% in ROPE = 0.92</strong>",# culi
                        "-1.20 (-3.64, 1.20) <br>pd = 85.42% <br>% in ROPE = 0.32",# tipu
                        "" # scir
    ),
    `Simla` = c("", # culi
                "", # tipu
                "" # scir
    ),
    `Present ReguaxResource Enriched` = c("<strong>-0.48 (-0.99, -0.02) <br>pd = 97.82% <br>% in ROPE = 0</strong>",# culi
                                          "0.03 (-3.66, 3.97) <br>pd = 50.78% <br>% in ROPE = 0.47",# tipu
                                          "" # scir
    ),
    `SimlaxResource Enriched` = c("",# culi
                                  "",# tipu
                                  "" # scir
    )
  )

## Save table
readr::write_csv(table_9,
                 here::here("brastri", "data",
                            "table_9.csv"))


# Compiling figures -------------------------------------------------------
# Figure 2, sig emerged biomass and proportion emerging
## Make figure
fig3 <- 
  cowplot::plot_grid(fig3a +
                       theme(legend.position = "none") +
                       ggtitle("a"),
                     fig3b +
                       theme(legend.position = "none") +
                       ggtitle("b"),
                     fig3c +
                       theme(legend.position = "none") +
                       ggtitle("c"),
                     fig3d +
                       theme(legend.position = "none") +
                       ggtitle("d"),
                     ncol = 2)
## Combine with legend
legend <- 
  cowplot::get_legend(fig3a)
fig3 <- 
  cowplot::plot_grid(legend,
                     fig3,
                     nrow = 2,
                     rel_heights = c(0.1, 0.9))
## Save figure
ggsave(here::here("brastri", "www",
                  "fig3.jpg"),
       fig3,
       width = 10,
       height = 11,
       bg = "white")


# Figure s2, unsig emerged biomass
## Make figure
figs2 <- 
  cowplot::plot_grid(figs2a +
                       theme(legend.position = "none") +
                       ggtitle("a"),
                     figs2b +
                       theme(legend.position = "none") +
                       ggtitle("b"),
                     figs2c +
                       theme(legend.position = "none") +
                       ggtitle("c"),
                     figs2d +
                       theme(legend.position = "none") +
                       ggtitle("d"),
                     figs2e +
                       theme(legend.position = "none") +
                       ggtitle("e"),
                     figs2f +
                       theme(legend.position = "none") +
                       ggtitle("f"),
                     figs2g +
                       theme(legend.position = "none") +
                       ggtitle("g"),
                     legend,
                     ncol = 2)
## Save figure
ggsave(here::here("brastri", "www",
                  "figs2.jpg"),
       figs2,
       width = 7,
       height = 11,
       bg = "white")


# Figure 3, sig leftover and P content
# Generate figure
## Get legend
legend <- 
  cowplot::get_legend(fig4a)
## Make figure
fig4 <- 
  cowplot::plot_grid(fig4a +
                       theme(legend.position = "none") +
                       ggtitle("a"),
                     fig4b +
                       theme(legend.position = "none") +
                       ggtitle("b"),
                     # fig4c +
                     #   theme(legend.position = "none") +
                     #   ggtitle("c"),
                     # fig4d +
                     #   theme(legend.position = "none") +
                     #   ggtitle("d"),
                     ncol = 2)
## Combine with legend
fig4 <- 
  cowplot::plot_grid(legend,
                     fig4,
                     nrow = 2,
                     rel_heights = c(0.1, 0.9))

## Save figure
ggsave(here::here("brastri", "www",
                  "fig4.jpg"),
       fig4,
       width = 10,
       height = 6,
       bg = "white")


# Figure s5, unsig leftover
## Make figure
figs5 <- 
  cowplot::plot_grid(figs5a +
                       theme(legend.position = "none") +
                       ggtitle("a"),
                     figs5b +
                       theme(legend.position = "none") +
                       ggtitle("b"),
                     figs5c +
                       theme(legend.position = "none") +
                       ggtitle("c"),
                     figs5d +
                       theme(legend.position = "none") +
                       ggtitle("d"),
                     figs5e +
                       theme(legend.position = "none") +
                       ggtitle("e"),
                     figs5f +
                       theme(legend.position = "none") +
                       ggtitle("f"),
                     figs5g +
                       theme(legend.position = "none") +
                       ggtitle("g"),
                     legend,
                     ncol = 2)
## Save figure
ggsave(here::here("brastri", "www",
                  "figs5.jpg"),
       figs5,
       width = 7,
       height = 9,
       bg = "white")

