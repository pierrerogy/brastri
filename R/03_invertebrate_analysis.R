# Invertebrate analyses

# Libraries
library(tidyverse)
library(ggtext)
library(here)
library(brms)
library(emmeans)
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

# P content
pcontent <-
  readr::read_csv(here::here("brastri", "data",
                             "pcontent.csv")) %>% 
  ## Add new treatment
  dplyr::mutate(site_pred = factor(ifelse(country == "trini",
                                          "trini", ifelse(country == "bras" & predator == "present",
                                                          "bras_present", "bras_absent")),
                                   levels = c("bras_absent", "bras_present", "trini")))
## Do contrasts
contrasts(pcontent$site_pred) <- 
  matrix(c(-0.5, -0.5, 1,
           -1, 1, 0),
         nrow = 3,
         dimnames = list(c("bras_absent", "bras_present", "trini"), 
                         c("bras_present", "trini")))

# Models on total emerged biomass -----------------------------------------------
# Overall biomass
## Fit model
emergencemodel_all <-
  brms::brm(biomass_mg ~
              resource*site_pred + (1|bromspecies/country),
            iter = 5000,
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
t9 <- 
  pairwise_contrasts(emergencemodel_all, 
                     both = T)
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
            iter = 5000,
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
t10 <- 
  pairwise_contrasts(emergencemodel_seed, 
                     both = T)
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
            iter = 5000,
            family = skew_normal(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_emergence(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "Chiro"))
## Check assumptions
plot(emergencemodel_chiro)
## Check effects
t11 <- 
  pairwise_contrasts(emergencemodel_chiro, 
                     both = T)
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
            iter = 5000,
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
t12 <- 
  pairwise_contrasts(emergencemodel_culi, 
                     bras = T)
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
            iter = 5000,
            family = skew_normal(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_emergence(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "Tipu"))
## Check assumptions
plot(emergencemodel_tipu)
## Check effects
t13 <- 
  pairwise_contrasts(emergencemodel_tipu, 
                     both = T)
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
            iter = 5000,
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
t14 <- 
  pairwise_contrasts(emergencemodel_cera, 
                     bras = T)
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
  t9 %>% 
  dplyr::bind_rows(list(t10,
                        t11,
                        t12,
                        t13,
                        t14))

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
            iter = 5000,
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
t15 <- 
  pairwise_contrasts(indemergencemodel_chiro, 
                   both = T)
## Plot
figs2f <- 
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
            iter = 5000,
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
t16 <- 
  pairwise_contrasts(indemergencemodel_culi, 
                     bras = T)
## Plot
figs2g <- 
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
            iter = 5000,
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
t17 <- 
  pairwise_contrasts(indemergencemodel_tipu, 
                     both = T)
## Plot
figs2h <- 
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
            iter = 5000,
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
t18 <- 
  pairwise_contrasts(indemergencemodel_cera, 
                     bras = T)
## Plot
fig3b <- 
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
  t15 %>% 
  dplyr::bind_rows(list(t16,
                        t17,
                        t18))

## Save table
readr::write_csv(table_5,
                 here::here("brastri", "data",
                            "table_5.csv"))


# Models on growth rate ---------------------------------------------------
# Culicidae
## Fit model
indgrowthmodel_culi <-
  brms::brm(ndays~
              resource*predator + (1|bromeliad_id/bromspecies/country),
            iter = 5000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = emergence_selected %>% 
              dplyr::filter(stringr::str_detect(string = species,
                                                pattern = "Culi")))
## Check assumptions
plot(indgrowthmodel_culi)
## Check effects
t19 <- 
  pairwise_contrasts(indgrowthmodel_culi, 
                     bras = T)
## Plot
figs3a <- 
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
t20 <- 
  pairwise_contrasts(indgrowthmodel_tipu, 
                   both = T)
## Plot
figs3b <- 
  treatment_plot(model = indgrowthmodel_tipu, 
                 scale = "none",
                 parameter = "growth_tipu_ind", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = emergence_selected %>% 
                   dplyr::filter(stringr::str_detect(string = species,
                                                     pattern = "Tipu")))


# Table summarising results
## Make table
table_6 <- 
  t19 %>% 
  dplyr::bind_rows(t20)

## Save table
readr::write_csv(table_6,
                 here::here("brastri", "data",
                            "table_6.csv"))



# Models on proportion emerging, or number emerged -------------------------------------------
# Seeded individuals
## Fit model
propemergencemodel_seed <-
  brms::brm(prop ~
              site_pred*resource + (1|bromspecies/country),
            iter = 5000,
            family = zero_inflated_beta(),    
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_proportion(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "seed"))


## Check assumptions
plot(propemergencemodel_seed)
## Check effects
t21 <- 
  pairwise_contrasts(propemergencemodel_seed, 
                     both = T)
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
# Get data quickly
dats <- 
  emergence_selected %>% 
  dplyr::filter(stringr::str_detect(string = species,
                                    pattern = "Culi")) %>% 
  dplyr::select(resource, predator, bromeliad_id, bromspecies, country) %>% 
  dplyr::group_by(resource, predator, bromeliad_id, bromspecies, country) %>% 
  dplyr::tally()
## Fit model
nemergencemodel_culi <-
  brms::brm(n ~
              resource*predator + (1|bromeliad_id),
            iter = 5000,
            family = poisson(),      
            control = list(adapt_delta = 0.95,
                           max_treedepth = 10),
            data = dats)
## Check assumptions
plot(nemergencemodel_culi)
## Check effects
t22 <- 
  pairwise_contrasts(nemergencemodel_culi, 
                     bras = T,
                     ROPE = sd(dats$n)*0.1)
## Plot
figs4b <- 
  treatment_plot(model = nemergencemodel_culi, 
                 scale = "none",
                 parameter = "nadult_culi", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = dats)


# Tipulidae
# Get data quickly
dats <- 
  emergence_selected %>% 
  dplyr::filter(stringr::str_detect(string = species,
                                    pattern = "Tipu")) %>% 
  dplyr::select(resource, site_pred, bromeliad_id, bromspecies, country) %>% 
  dplyr::group_by(resource, site_pred, bromeliad_id, bromspecies, country) %>% 
  dplyr::tally()
## Fit model
nemergencemodel_tipu <-
  brms::brm(n ~
              resource*site_pred + (1|bromeliad_id/bromspecies/country),
            iter = 5000,
            family = poisson(),      
            control = list(adapt_delta = 0.95,
                           max_treedepth = 10),
            data = dats)
## Check assumptions
plot(nemergencemodel_tipu)
## Check effects
t23 <- 
  pairwise_contrasts(nemergencemodel_tipu, 
                     both = T,
                     ROPE = sd(dats$n)*0.1)
## Plot
figs4c <- 
  treatment_plot(model = nemergencemodel_tipu, 
                 scale = "none",
                 parameter = "nadult_tipu", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = dats)

# Chironomidae
# Get data quickly
dats <- 
  emergence_selected %>% 
  dplyr::filter(stringr::str_detect(string = species,
                                    pattern = "Chiro")) %>% 
  dplyr::select(resource, site_pred, bromeliad_id, bromspecies, country) %>% 
  dplyr::group_by(resource, site_pred, bromeliad_id, bromspecies, country) %>% 
  dplyr::tally()
## Fit model
nemergencemodel_chiro <-
  brms::brm(n ~
              resource*site_pred + (1|bromeliad_id/bromspecies/country),
            iter = 5000,
            family = poisson(),      
            control = list(adapt_delta = 0.95,
                           max_treedepth = 10),
            data = dats)
## Check assumptions
plot(nemergencemodel_chiro)
## Check effects
t50 <- 
  pairwise_contrasts(nemergencemodel_chiro, 
                     both = T,
                     ROPE = sd(dats$n)*0.1)
## Plot
figs4d <- 
  treatment_plot(model = nemergencemodel_chiro, 
                 scale = "none",
                 parameter = "nadult_chiro", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = dats)

# Ceratopogonidae
# Get data quickly
dats <- 
  emergence_selected %>% 
  dplyr::filter(stringr::str_detect(string = species,
                                    pattern = "Cera")) %>% 
  dplyr::select(resource, predator, bromeliad_id, bromspecies, country) %>% 
  dplyr::group_by(resource, predator, bromeliad_id, bromspecies, country) %>% 
  dplyr::tally()
## Fit model
nemergencemodel_cera <-
  brms::brm(n ~
              resource*predator + (1|bromeliad_id),
            iter = 5000,
            family = poisson(),      
            control = list(adapt_delta = 0.95,
                           max_treedepth = 10),
            data = dats)
## Check assumptions
plot(nemergencemodel_cera)
## Check effects
t51 <- 
  pairwise_contrasts(nemergencemodel_cera, 
                     bras = T,
                     ROPE = sd(dats$n)*0.1)
## Plot
figs4e <- 
  treatment_plot(model = nemergencemodel_cera, 
                 scale = "none",
                 parameter = "nadult_cera", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = dats)

# Table summarising results
## Make table
table_7 <- 
  t21 %>% 
  dplyr::bind_rows(list(t22,
                        t23,
                        t50,
                        t51))
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
            iter = 5000,
            family = skew_normal(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_leftover(dats = community_end,
                                       bromeliads = bromeliads,
                                       group = "all"))
## Check assumptions
plot(leftovermodel_all)
## Check effects
t24 <- 
  pairwise_contrasts(leftovermodel_all, 
                     both = T)
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
            iter = 5000,
            family = skew_normal(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_leftover(dats = community_end,
                                       bromeliads = bromeliads,
                                       group = "seed"))
## Check assumptions
plot(leftovermodel_seed)
## Check effects
t25 <- 
  pairwise_contrasts(leftovermodel_seed, 
                     both = T)
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
            iter = 5000,
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
t26 <- 
  pairwise_contrasts(leftovermodel_chiro, 
                     trini = T)
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
            iter = 5000,
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
t27 <- 
  pairwise_contrasts(leftovermodel_culi, 
                     bras = T)
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
            iter = 5000,
            family = skew_normal(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = summarise_leftover(dats = community_end,
                                       bromeliads = bromeliads,
                                       group = "Tipu"))
## Check assumptions
plot(leftovermodel_tipu)
## Check effects
t28 <- 
  pairwise_contrasts(leftovermodel_tipu, 
                     bras = T)
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
            iter = 5000,
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
t29 <- 
  pairwise_contrasts(leftovermodel_scir, 
                     trini = T)
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
  t24 %>% 
  dplyr::bind_rows(list(t25,
                        t26,
                        t27,
                        t28,
                        t29))
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
t30 <- 
  pairwise_contrasts(indleftovermodel_culi, 
                     bras = T)
## Plot
fig3c <- 
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
            iter = 5000,
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
t31 <- 
  pairwise_contrasts(indleftovermodel_tipu, 
                     bras = T)
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
            iter = 5000,
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
t32 <- 
  pairwise_contrasts(indleftovermodel_scir, 
                     trini = T)
## Plot
fig3d <- 
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
  t30 %>% 
  dplyr::bind_rows(list(t31,
                        t32))

## Save table
readr::write_csv(table_9,
                 here::here("brastri", "data",
                            "table_9.csv"))



# Models on number of individuals remaining -------------------------------
# Chironomidae ONLY FROM SIMLA IN CONTROL BROMELIADS
## Fit model
community_end %>% 
  dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
  dplyr::filter(stringr::str_detect(string = genus,
                                    pattern = "Poly"))

# Culicidae
# Get data quickly
dats <- 
  community_end %>% 
  dplyr::filter(stringr::str_detect(string = family,
                                    pattern = "Culi") & 
                  seeded == "seeded") %>% 
  dplyr::select(resource, predator, bromeliad_id, bromspecies, country) %>% 
  dplyr::group_by(resource, predator, bromeliad_id, bromspecies, country) %>% 
  dplyr::tally()
## Fit model
nleftovermodel_culi <-
  brms::brm(n ~
              resource*predator + (1|bromeliad_id),
            iter = 5000,
            family = poisson(),      
            control = list(adapt_delta = 0.95,
                           max_treedepth = 10),
            data = dats)
## Check assumptions
plot(nleftovermodel_culi)
## Check effects
t33 <- 
  pairwise_contrasts(nleftovermodel_culi, 
                     bras = T,
                     ROPE = sd(dats$n)*0.1)
## Plot
figs6a <- 
  treatment_plot(model = nleftovermodel_culi, 
                 scale = "none",
                 parameter = "nremaining_culi", 
                 bromeliads = bromeliads, 
                 communities = dats, 
                 water = water, 
                 emergence = emergence)

# Tipulidae
# Get data quickly
dats <- 
  community_end %>% 
  dplyr::filter(stringr::str_detect(string = family,
                                    pattern = "Tipu")) %>% 
  dplyr::select(resource, predator, bromeliad_id, bromspecies, country) %>% 
  dplyr::group_by(resource, predator, bromeliad_id, bromspecies, country) %>% 
  dplyr::tally()
## Fit model
nleftovermodel_tipu <-
  brms::brm(n ~
              resource*predator + (1|bromeliad_id),
            iter = 5000,
            family = poisson(),      
            control = list(adapt_delta = 0.95,
                           max_treedepth = 10),
            data = dats)
## Check assumptions
plot(nleftovermodel_tipu)
## Check effects
t34 <- 
  pairwise_contrasts(nleftovermodel_tipu, 
                     bras = T,
                     ROPE = sd(dats$n)*0.1)
## Plot
figs6b <- 
  treatment_plot(model = nleftovermodel_tipu, 
                 scale = "none",
                 parameter = "nremaining_tipu", 
                 bromeliads = bromeliads, 
                 communities = dats, 
                 water = water, 
                 emergence = emergence)

# Scirtidae
# Get data quickly
dats <- 
  community_end %>% 
  dplyr::filter(stringr::str_detect(string = family,
                                    pattern = "Scir")) %>% 
  dplyr::select(resource, predator, bromeliad_id, bromspecies, country) %>% 
  dplyr::group_by(resource, predator, bromeliad_id, bromspecies, country) %>% 
  dplyr::tally()
## Fit model
nleftovermodel_scir <-
  brms::brm(n ~
              resource + (1|bromeliad_id),
            iter = 5000,
            family = poisson(),      
            control = list(adapt_delta = 0.95,
                           max_treedepth = 10),
            data = dats)
## Check assumptions
plot(nleftovermodel_scir)
## Check effects
t35 <- 
  pairwise_contrasts(nleftovermodel_scir, 
                     trini = T,
                     ROPE = sd(dats$n)*0.1)
## Plot
figs6c <- 
  treatment_plot(model = nleftovermodel_scir, 
                 scale = "none",
                 parameter = "nremaining_scirtid", 
                 bromeliads = bromeliads, 
                 communities = dats, 
                 water = water, 
                 emergence = emergence,
                 trini = T)
# Table summarising results
## Make table
table_10 <- 
  t33 %>% 
  dplyr::bind_rows(list(t34,
                        t35))

## Save table
readr::write_csv(table_10,
                 here::here("brastri", "data",
                            "table_10.csv"))

# Model on P content - adults ------------------------------------------------------
# All adults  
## Fit model
pcontentmodel_alladults <-
  brms::brm(log(p_prcnt) ~
              resource*site_pred + (1|species/bromspecies/country),
            iter = 5000,
            family = gaussian(),      
            control = list(adapt_delta = 0.98,
                           max_treedepth = 15),
            data = pcontent %>% 
              dplyr::filter(stage == "adult"))
## Check assumptions
plot(pcontentmodel_alladults)
## Check effects
t40 <- 
  pairwise_contrasts(pcontentmodel_alladults, 
                     both = T)
## Plot
fig4a <- 
  treatment_plot(model = pcontentmodel_alladults, 
                 scale = "log",
                 parameter = "p_prcnt_alladults", 
                 bromeliads = bromeliads, 
                 communities = pcontent %>% 
                   dplyr::filter(stage == "adult"), 
                 water = water, 
                 emergence = emergence) +
    scale_y_continuous(trans = "log",
                       breaks = c(0.01, 0.3, 1, 5))

# Mosquito adults  
## Fit model
pcontentmodel_culiadults <-
  brms::brm(log(p_prcnt) ~
              resource*predator,
            iter = 5000,
            family = gaussian(),      
            control = list(adapt_delta = 0.98,
                           max_treedepth = 15),
            data = pcontent %>% 
              dplyr::filter(stage == "adult" & taxon == "Culicidae" & country == "bras"))
## Check assumptions
plot(pcontentmodel_culiadults)
## Check effects
t41 <- 
  pairwise_contrasts(pcontentmodel_culiadults, 
                     bras = T)
## Plot
figs7a <- 
  treatment_plot(model = pcontentmodel_culiadults, 
                 scale = "log",
                 parameter = "p_prcnt_culiadults", 
                 bromeliads = bromeliads, 
                 communities = pcontent %>% 
                   dplyr::filter(stage == "adult" & taxon == "Culicidae" & country == "bras"), 
                 water = water, 
                 emergence = emergence) +
    scale_y_continuous(trans = "log",
                       breaks = c(0.05, 0.3, 1, 4))

# Tipulid adults
## Fit model
pcontentmodel_tipuadults<-
  brms::brm(log(p_prcnt) ~
              resource*site_pred + (1|species/bromspecies/country),
            iter = 5000,
            family = gaussian(),      
            control = list(adapt_delta = 0.98,
                           max_treedepth = 15),
            data = pcontent %>% 
              dplyr::filter(stage == "adult" & taxon == "Tipulidae"))
## Check assumptions
plot(pcontentmodel_tipuadults)
## Check effects
t42 <- 
  pairwise_contrasts(pcontentmodel_tipuadults, 
                     both = T)
## Plot
fig4b <- 
  treatment_plot(model = pcontentmodel_tipuadults, 
                 scale = "log",
                 parameter = "p_prcnt_tipuadults", 
                 bromeliads = bromeliads, 
                 communities = pcontent %>% 
                   dplyr::filter(stage == "adult" & taxon == "Tipulidae"), 
                 water = water, 
                 emergence = emergence) +
    scale_y_continuous(trans = "log",
                       breaks = c(0.01, 0.05, 1, 20))

# Model on P content - larvae ------------------------------------------------------
# All larvae  
## Fit model
pcontentmodel_alllarvae <-
  brms::brm(log(p_prcnt) ~
              log(invert_mass_mg) + resource*site_pred + (1|bromspecies/country),
            iter = 5000,
            family = gaussian(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = pcontent %>% 
              dplyr::filter(stage == "larva" & invert_mass_mg > 0))
## Check assumptions
plot(pcontentmodel_alllarvae)
## Check effects
t36 <- 
  pairwise_contrasts(model = pcontentmodel_alllarvae, 
                     both = T,
                     invert_mass_mg = T)
## Plot
fig4c <- 
  treatment_plot(model = pcontentmodel_alllarvae, 
                 scale = "log",
                 parameter = "p_prcnt_alllarvae", 
                 bromeliads = bromeliads, 
                 communities = pcontent %>% 
                   dplyr::filter(stage == "larva"), 
                 water = water, 
                 emergence = emergence) +
    scale_y_continuous(trans = "log",
                       breaks = c(0.05, 1, 20))

# Mosquito larvae  
## Fit model
pcontentmodel_culilarvae <-
  brms::brm(log(p_prcnt) ~
              log(invert_mass_mg) + resource*predator,
            iter = 5000,
            family = gaussian(),      
            control = list(adapt_delta = 0.98,
                           max_treedepth = 15),
            data = pcontent %>% 
              dplyr::filter(stage == "larva" & taxon == "Culicidae" & country == "bras"))
## Check assumptions
plot(pcontentmodel_culilarvae)
## Check effects
t37 <- 
  pairwise_contrasts(pcontentmodel_culilarvae, 
                     bras = T,
                     invert_mass_mg = T)
## Plot
figs7b <-
  treatment_plot(model = pcontentmodel_culilarvae, 
                 scale = "log",
                 parameter = "p_prcnt_culilarvae", 
                 bromeliads = bromeliads, 
                 communities = pcontent %>% 
                   dplyr::filter(stage == "larva" & taxon == "Culicidae" & country == "bras"), 
                 water = water, 
                 emergence = emergence) +
    scale_y_continuous(trans = "log",
                       breaks = c(0.05, 0.5, 5, 50))

# Tipulid larvae
## Fit model
pcontentmodel_tipularvae <-
  brms::brm(log(p_prcnt) ~
              log(invert_mass_mg) + resource*site_pred + (1|bromspecies/country),
            iter = 5000,
            family = gaussian(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = pcontent %>% 
              dplyr::filter(stage == "larva" & taxon == "Tipulidae"))
## Check assumptions
plot(pcontentmodel_tipularvae)
## Check effects
t38 <- 
  pairwise_contrasts(pcontentmodel_tipularvae, 
                     both = T,
                     invert_mass_mg = T)
## Plot
fig4d <- 
  treatment_plot(model = pcontentmodel_tipularvae, 
               scale = "log",
               parameter = "p_prcnt_tipularvae", 
               bromeliads = bromeliads, 
               communities = pcontent %>% 
                 dplyr::filter(stage == "larva" & taxon == "Tipulidae"), 
               water = water, 
               emergence = emergence) +
  scale_y_continuous(trans = "log",
                     breaks = c(0.01, 2, 50))

# Scirtid larvae
## Fit model
pcontentmodel_scirlarvae <-
  brms::brm(log(p_prcnt) ~
              log(invert_mass_mg) + resource + (1|bromspecies),
            iter = 5000,
            family = gaussian(),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 15),
            data = pcontent %>% 
              dplyr::filter(stage == "larva" & taxon == "Scirtes" & country == "trini"))
## Check assumptions
plot(pcontentmodel_tipularvae)
## Check effects
t39 <- 
  pairwise_contrasts(pcontentmodel_scirlarvae, 
                     trini = T,
                     invert_mass_mg = T)
## Plot
figs7c <-
  treatment_plot(model = pcontentmodel_scirlarvae, 
               scale = "log",
               parameter = "p_prcnt_scirlarvae", 
               bromeliads = bromeliads, 
               communities = pcontent %>% 
                 dplyr::filter(stage == "larva" & taxon == "Scirtes" & country == "trini"), 
               water = water, 
               emergence = emergence,
               trini = T) +
  scale_y_continuous(trans = "log",
                     breaks = c(0.1, 0.3, 1))


# Compiling figures -------------------------------------------------------
# Figure 3, sig and emerged remaining
## Make legend
legend <- 
  cowplot::get_legend(fig3a +
                        theme(legend.text = element_text(size = rel(1.1)),
                              legend.title= element_text(size = rel(1.1))))
## Make figure
fig3 <- 
  cowplot::plot_grid(fig3a +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1.1)),
                             axis.text.x = element_text(size = rel(1.1)),
                             axis.text.y = element_text(size = rel(1.1)),
                             axis.title.y = element_text(size = rel(1.1))) +
                       ggtitle("(a)"),
                     fig3b +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1.1)),
                             axis.text.x = element_text(size = rel(1.1)),
                             axis.text.y = element_text(size = rel(1.1)),
                             axis.title.y = element_text(size = rel(1.1))) +
                       ggtitle("(b)"),
                     fig3c +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1.1)),
                             axis.text.x = element_text(size = rel(1.1)),
                             axis.text.y = element_text(size = rel(1.1)),
                             axis.title.y = element_text(size = rel(1.1))) +
                       ggtitle("(c)"),
                     fig3d +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1.1)),
                             axis.text.x = element_text(size = rel(1.1)),
                             axis.text.y = element_text(size = rel(1.1)),
                             axis.title.y = element_text(size = rel(1.1))) +
                       ggtitle("(d)"),
                     ncol = 2)
## Combine with legend
fig3 <- 
  cowplot::plot_grid(legend,
                     fig3,
                     nrow = 2,
                     rel_heights = c(0.1, 0.9))
## Save figure
ggsave(here::here("brastri", "www",
                  "fig3.jpg"),
       fig3,
       width = 9,
       height = 9,
       bg = "white")

# Figure 4, sig re and P content
# Generate figure
## Get legend
legend <- 
  cowplot::get_legend(fig4a + 
                        theme(legend.text = element_text(size = rel(1.1)),
                              legend.title= element_text(size = rel(1.1))))
## Make figure
fig4 <- 
  cowplot::plot_grid(fig4a +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1.1)),
                             axis.text.x = element_text(size = rel(1.1)),
                             axis.text.y = element_text(size = rel(1.1)),
                             axis.title.y = element_text(size = rel(1.1))) +
                       ggtitle("(a)"),
                     fig4b +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1.1)),
                             axis.text.x = element_text(size = rel(1.1)),
                             axis.text.y = element_text(size = rel(1.1)),
                             axis.title.y = element_text(size = rel(1.1))) +
                       ggtitle("(b)"),
                     fig4c +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1.1)),
                             axis.text.x = element_text(size = rel(1.1)),
                             axis.text.y = element_text(size = rel(1.1)),
                             axis.title.y = element_text(size = rel(1.1))) +
                       ggtitle("(c)"),
                     fig4d +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1.1)),
                             axis.text.x = element_text(size = rel(1.1)),
                             axis.text.y = element_text(size = rel(1.1)),
                             axis.title.y = element_text(size = rel(1.1))) +
                       ggtitle("(d)"),
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
       width = 9,
       height = 9,
       bg = "white")


# Figure s2, unsig remaining biomass
## Make figure
figs2 <- 
  cowplot::plot_grid(figs2a +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1)),
                             axis.text.x = element_text(size = rel(1)),
                             axis.text.y = element_text(size = rel(1)),
                             axis.title.y = element_text(size = rel(1))) +
                       ggtitle("(a)"),
                     figs2b +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1)),
                             axis.text.x = element_text(size = rel(1)),
                             axis.text.y = element_text(size = rel(1)),
                             axis.title.y = element_text(size = rel(1))) +
                       ggtitle("(b)"),
                     figs2c +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1)),
                             axis.text.x = element_text(size = rel(1)),
                             axis.text.y = element_text(size = rel(1)),
                             axis.title.y = element_text(size = rel(1))) +
                       ggtitle("(c)"),
                     figs2d +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1)),
                             axis.text.x = element_text(size = rel(1)),
                             axis.text.y = element_text(size = rel(1)),
                             axis.title.y = element_text(size = rel(1))) +
                       ggtitle("(d)"),
                     figs2e +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1)),
                             axis.text.x = element_text(size = rel(1)),
                             axis.text.y = element_text(size = rel(1)),
                             axis.title.y = element_text(size = rel(1))) +
                       ggtitle("(e)"),
                     figs2f +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1)),
                             axis.text.x = element_text(size = rel(1)),
                             axis.text.y = element_text(size = rel(1)),
                             axis.title.y = element_text(size = rel(1))) +
                       ggtitle("(f)"),
                     figs2g +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1)),
                             axis.text.x = element_text(size = rel(1)),
                             axis.text.y = element_text(size = rel(1)),
                             axis.title.y = element_text(size = rel(1))) +
                       ggtitle("(g)"),
                     figs2h +
                       theme(legend.position = "none",
                             axis.title.x = element_text(size = rel(1)),
                             axis.text.x = element_text(size = rel(1)),
                             axis.text.y = element_text(size = rel(1)),
                             axis.title.y = element_text(size = rel(1))) +
                       ggtitle("(h)"),
                     legend,
                     ncol = 2)
## Save figure
ggsave(here::here("brastri", "www",
                  "figs2.jpg"),
       figs2,
       width = 7,
       height = 12,
       bg = "white")

# Fig s3, number of daays until emergence
## Make figure
figs3 <- 
  cowplot::plot_grid(figs3a +
                       theme(legend.position = "none") +
                       ggtitle("(a)"),
                     figs3b +
                       theme(legend.position = "none") +
                       ggtitle("(b)"),
                     legend,
                     ncol = 3)

## Save figure
ggsave(here::here("brastri", "www",
                  "figs3.jpg"),
       figs3,
       width = 10,
       height = 4,
       bg = "white")

# n insect emerged
## Make figure
figs4 <- 
  cowplot::plot_grid(figs4a +
                       theme(legend.position = "none") +
                       ggtitle("(a)"),
                     figs4b +
                       theme(legend.position = "none") +
                       ggtitle("(b)"),
                     figs4c +
                       theme(legend.position = "none") +
                       ggtitle("(c)"),
                     figs4d +
                       theme(legend.position = "none") +
                       ggtitle("(d)"),
                     figs4e +
                       theme(legend.position = "none") +
                       ggtitle("(e)"),
                     legend,
                     ncol = 2)
## Save figure
ggsave(here::here("brastri", "www",
                  "figs4.jpg"),
       figs4,
       width = 9,
       height = 11,
       bg = "white")



# Figure s5, unsig leftover bio/body mass
## Make figure
figs5 <- 
  cowplot::plot_grid(figs5a +
                       theme(legend.position = "none") +
                       ggtitle("(a)"),
                     figs5b +
                       theme(legend.position = "none") +
                       ggtitle("(b)"),
                     figs5c +
                       theme(legend.position = "none") +
                       ggtitle("(c)"),
                     figs5d +
                       theme(legend.position = "none") +
                       ggtitle("(d)"),
                     figs5e +
                       theme(legend.position = "none") +
                       ggtitle("(e)"),
                     figs5f +
                       theme(legend.position = "none") +
                       ggtitle("(f)"),
                     figs5g +
                       theme(legend.position = "none") +
                       ggtitle("(g)"),
                     legend,
                     ncol = 2)
## Save figure
ggsave(here::here("brastri", "www",
                  "figs5.jpg"),
       figs5,
       width = 7,
       height = 10,
       bg = "white")

# Figure s6, unsig leftover abundance
## Make figure
figs6 <- 
  cowplot::plot_grid(figs6a +
                       theme(legend.position = "none") +
                       ggtitle("(a)"),
                     figs6b +
                       theme(legend.position = "none") +
                       ggtitle("(b)"),
                     figs6c +
                       theme(legend.position = "none") +
                       ggtitle("(c)"),
                     legend,
                     ncol = 2)
## Save figure
ggsave(here::here("brastri", "www",
                  "figs6.jpg"),
       figs6,
       width = 9,
       height = 9,
       bg = "white")

# Make figure
figs7 <- 
  cowplot::plot_grid(figs7a +
                       theme(legend.position = "none") +
                       ggtitle("(a)"),
                     figs7b +
                       theme(legend.position = "none") +
                       ggtitle("(b)"),
                     figs7c +
                       theme(legend.position = "none") +
                       ggtitle("(c)"),
                     legend,
                     ncol = 2)
## Save figure
ggsave(here::here("brastri", "www",
                  "figs7.jpg"),
       figs7,
       width = 9,
       height = 9,
       bg = "white")

