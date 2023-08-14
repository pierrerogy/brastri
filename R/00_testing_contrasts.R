# Testing contrasts

# Libraries
library(brms)
library(performance)
library(emmeans)
library(ggeffects)
library(tidyverse)

# Analysing contrasts -----------------------------------------------------
# Original model with contrasts
## Fit model
indemergencemodel_chiro <-
  brms::brm(biomass_mg~
              resource*site_pred + (1|bromeliad_id/bromspecies),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = emergence_selected %>% 
              dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
              dplyr::filter(stringr::str_detect(string = species,
                                                pattern = "Chiro")))
## Check effects
bayestestR::describe_posterior(indemergencemodel_chiro)
## Performance stuff
mse <- 
  performance::mse(indemergencemodel_chiro)
r2 <- 
  performance::r2(indemergencemodel_chiro)
emm <- 
  emmeans(indemergencemodel_chiro, 
          "site_pred")
con <- 
  contrast(emm, 
           method = "eff")

plot(ggpredict(indemergencemodel_chiro,
               terms = c("site_pred", "resource")))

# Original model without contrasts
## Fit model
indemergencemodel_chiro0 <-
  brms::brm(biomass_mg~
              resource*site_pred1 + (1|bromeliad_id/bromspecies),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = emergence_selected %>% 
              dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
              dplyr::filter(stringr::str_detect(string = species,
                                                pattern = "Chiro")))
## Check assumptions
plot(indemergencemodel_chiro0)
## Check effects
bayestestR::describe_posterior(indemergencemodel_chiro0)
## Performance stuff
mse0 <- 
  performance::mse(indemergencemodel_chiro0)
r20 <- 
  performance::r2(indemergencemodel_chiro0)
emm0 <- 
  emmeans(indemergencemodel_chiro0, 
          "site_pred1")
con0 <- 
  contrast(emm0, 
           method = "eff")
plot(ggeffect(indemergencemodel_chiro0,
              terms = c("site_pred", "resource")))

# Model with country only
## Fit model
indemergencemodel_chiro1 <-
  brms::brm(biomass_mg~
              resource*country + (1|bromeliad_id/bromspecies),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = emergence_selected %>% 
              dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
              dplyr::filter(stringr::str_detect(string = species,
                                                pattern = "Chiro")))
## Check assumptions
plot(indemergencemodel_chiro1)
## Check effects
bayestestR::describe_posterior(indemergencemodel_chiro1)
## Performance stuff
mse1 <- 
  performance::mse(indemergencemodel_chiro1)
r21 <- 
  performance::r2(indemergencemodel_chiro1)
emm1 <- 
  emmeans(indemergencemodel_chiro1, 
          "country")
con1 <- 
  contrast(emm1, 
           method = "eff")
plot(ggeffect(indemergencemodel_chiro1,
              terms = c("country", "resource")))

# Model with predators only
## Fit model
indemergencemodel_chiro2 <-
  brms::brm(biomass_mg~
              resource*predator + (1|bromeliad_id/bromspecies),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = emergence_selected %>% 
              dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
              dplyr::filter(stringr::str_detect(string = species,
                                                pattern = "Chiro")) %>% 
              dplyr::filter(country == "bras"))
## Check assumptions
plot(indemergencemodel_chiro2)
## Check effects
bayestestR::describe_posterior(indemergencemodel_chiro2)
## Performance stuff
mse2 <- 
  performance::mse(indemergencemodel_chiro2)
r22 <- 
  performance::r2(indemergencemodel_chiro2)
emm2 <- 
  emmeans(indemergencemodel_chiro2, 
          "predator")
con2 <- 
  contrast(emm2, 
           method = "eff")
plot(ggeffect(indemergencemodel_chiro2,
              terms = c("site_pred", "resource")))

# Model with predators and wihtout any contrasts
## Fit model
indemergencemodel_chiro3 <-
  brms::brm(biomass_mg~
              resource*predator + (1|bromeliad_id/bromspecies),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = emergence_selected %>% 
              dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
              dplyr::filter(stringr::str_detect(string = species,
                                                pattern = "Chiro")))
## Check assumptions
plot(indemergencemodel_chiro3)
## Check effects
bayestestR::describe_posterior(indemergencemodel_chiro3)
## Performance stuff
mse3 <- 
  performance::mse(indemergencemodel_chiro3)
r23 <- 
  performance::r2(indemergencemodel_chiro3)
emm3 <- 
  emmeans(indemergencemodel_chiro3, 
          "predator")
con3 <- 
  contrast(emm3, 
           method = "eff")
plot(ggpredict(indemergencemodel_chiro3,
               terms = c("predator", "resource")))

