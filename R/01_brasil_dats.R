# Preparation of Brasil data

# Libraries
library(tidyverse)
library(ggplot2)
library(here)
library(hellometry)

# Load data
bichos <- 
  readr::read_csv(here::here("brasil",
                             "bichos.csv"))
bromeliads <- 
  readr::read_csv(here::here("brasil",
                             "bromeliads.csv"))
communities <- 
  readr::read_csv(here::here("brasil",
                             "communities.csv"))


# Get density of invertebrates --------------------------------------------
# Data
bichos_fam <- 
  bichos %>% 
  dplyr::filter(stringr::str_detect(bromeliad_id,
                                    "B")) %>% 
  dplyr::filter(Class != "Crustacea") %>% 
  dplyr::filter(Class != "Clitellata") %>% 
  dplyr::mutate(Family = ifelse(is.na(Family),
                                Order,
                                Family)) %>% 
  dplyr::mutate(Subfamily = ifelse(is.na(Subfamily),
                                   Family,
                                   Subfamily)) %>% 
  dplyr::select(bromeliad_id, Class, Order, Family, Subfamily, n) %>% 
  dplyr::group_by(bromeliad_id, Class, Order, Family, Subfamily) %>% 
  dplyr::summarise_all(sum) %>% 
  dplyr::ungroup() 

# Print most abundant family  
bichos_totabun <- 
  bichos_fam %>% 
  dplyr::select(-bromeliad_id) %>% 
  dplyr::group_by(Class, Order, Family, Subfamily) %>% 
  dplyr::summarise_all(sum) %>% 
  dplyr::ungroup() %>%
  dplyr::arrange(desc(n))

# Here is what I want in my foodwebs
subfams <- 
  c("Coenogronidae", "Tipulidae",
    "Culicidae",
    "Chironominae", "Tanypodinae")
# Make  small DF with how muchof each
objectives <- 
  data.frame(Subfamily = subfams,
             dens_chosen = c(1, 3, 
                             8, 
                             4, 1))

# Get density
bichos_fam_density <- 
  bichos_fam %>% 
  dplyr::left_join(bromeliads %>% 
                     dplyr::select(bromeliad_id, volume_mL),
                   by = "bromeliad_id") %>% 
  dplyr::mutate(density_nat = n/volume_mL) %>% 
  dplyr::select(Class, Order, Family, Subfamily, density_nat) %>% 
  dplyr::group_by(Class, Order, Family, Subfamily) %>% 
  dplyr::summarise_all(mean) %>% 
  dplyr::ungroup() %>% 
  # here just seeing  what we would have with the five
  # most abundant in 200mL water
  dplyr::filter(Subfamily %in% subfams) %>% 
  dplyr::mutate(density_nat = round(density_nat * 200),
                objective_dens = density_nat * 32 + 15) %>% 
  dplyr::rename(density_for_200mL_brom = density_nat) %>% 
  ## Lets not forget that objective for damselfly is half
  dplyr::mutate(objective_dens = ifelse(Subfamily == "Coenogronidae",
                                        objective_dens/2,
                                        objective_dens)) %>% 
  ## Add objectives
  dplyr::left_join(objectives,
                   by = "Subfamily") %>% 
  dplyr::mutate(objective = dens_chosen*32 + 15) %>% 
  ## Lets not forget that objective for damselfly is half
  dplyr::mutate(objective = ifelse(Subfamily == "Coenogronidae",
                                   objective/2,
                                   objective)) %>%
  ## Add how much we have
  dplyr::left_join(bichos_totabun %>% 
                     dplyr::select(Subfamily, n) %>% 
                     dplyr::rename(current = n))

# Save density  
readr::write_csv(bichos_fam_density,
                 here::here("brasil",
                            "objectives.csv")) 

# Prepare data on bromeliads and decomposition ---------------------------------------------
# Get list of bromeliads and treatments
bromeliad_stub <- 
  bromeliads %>% 
  ## Only keep experimental ones
  dplyr::filter(stringr::str_detect(bromeliad_id,
                                    "E")) %>% 
  ## Get percent difference in mass of leaf_litter
  dplyr::mutate(coarse_dec = (coarse_init_g - coarse_final_dry_g)/coarse_init_g,
                fine_dec = (fine_init_g - fine_final_dry_g)/fine_init_g) %>%
  ## Only keep relevant columns
  dplyr::select(-day, -coarse_det_g, -medium_det_g,
                -fine_det_g, -coarse_final_g, -fine_final_g,
                -coarse_init_g, -coarse_final_dry_g,
                -fine_init_g, -fine_final_dry_g)  

# Add missing bits of taxonomy --------------------------------------------
bichos <- 
  bichos %>% 
  ## Mosquito genus
  dplyr::mutate(Genus = ifelse(Family == "Culicidae" &
                                 Species == "sp1",
                               "Weomyia", Genus)) %>% 
  ## Chironomid subfamilies
  ### sp8 and sp9 are neither subfamily of interest
  dplyr::mutate(Subfamily = ifelse(Family == "Chironomidae" &
                                     Species %in% c("sp7", "sp11"),
                                   "Telmatogetoninae",
                                   ifelse(Family == "Chironomidae" &
                                            Species %in% c("sp2", "sp3", "sp5"),
                                          "Orthocladiinae",
                                          ifelse(Family == "Chironomidae" &
                                                   Species %in% c("sp4", "sp6", "sp10"),
                                                 "Chironominae",
                                                 ifelse(Family == "Chironomidae" &
                                                          Species == "sp12",
                                                        "Tanypodinae",
                                                        ifelse(Family == "Chironomidae" &
                                                                 Species == "sp1",
                                                               "Diamesinae", Subfamily
                                                        )))))) %>% 
  ## Ceratopogonid subfamilies
  dplyr::mutate(Subfamily = ifelse(Family == "Ceratopogonidae" & 
                                     Species %in% c("sp1","sp2", "sp5"),
                                   "Ceratopogoninae", 
                                   ifelse(Family == "Ceratopogonidae" & 
                                            Species == "sp3",
                                          "Forcipomyiinae",
                                          ifelse(Family == "Ceratopogonidae" & 
                                                   Species %in% c("sp4","sp6", "sp7"),
                                                 "Leptoconopinae", Subfamily)))) %>% 
  ## Tipulid genus
  dplyr::mutate(Genus = ifelse(Family == "Tipulidae",
                               "Trentepohlia", Genus))

# Preparing emergence data ---------------------
# Data
bichos_emerged <- 
  bichos %>% 
  ## Data held in experimental (E) bromeliads
  dplyr::filter(stringr::str_detect(bromeliad_id,
                                    "E")) %>% 
  ## Our Weomyia, tipulids, odonates, chironomids of interest
  dplyr::filter(Family == "Culicidae" &
                  Species == "sp1" |
                  Family %in% c("Tipulidae") |
                  Order == "Odonata" |
                  Subfamily %in% c("Chironominae", 
                                   "Tanypodinae",
                                   "Ceratopogoninae",
                                   "Forcipomyiinae")) %>% 
  ## Create date class
  dplyr::mutate(date = lubridate::dmy(date)) %>% 
  ## Convert to day number and remove day number of first day
  dplyr::mutate(days_emergence = lubridate::yday(date) - 
                  lubridate::yday(lubridate::as_date(lubridate::dmy("31/03/2022")))) %>% 
  ## Remove biomass of mosquito full of blood to replace it with estimation
  dplyr::mutate(dry_mass_mg = ifelse(stringr::str_detect(notes,
                                                         "blood"),
                                     NA, dry_mass_mg)) %>% 
  ## Remove the notes and empty columns
  dplyr::select(-notes, -'...13', -'...14', -'...15')
  
# Estimation of biomass using hellometry
bichos_biomass_est <- 
  bichos_emerged %>% 
  ## Rename columns
  dplyr::rename(size_mm = length_mm,
                abundance = n) %>% 
  ## Add columns needed by package
  dplyr::mutate(stage ="adult",
                bwg_name = NA,
                biomass_type = "dry") %>% 
  ## Add unknown to missing sizes
  dplyr::mutate(size_mm = ifelse(size_mm == "missing" |
                                   is.na(size_mm),
                                 "unknown", size_mm)) %>% 
  ## Rename column with biomass
  dplyr::rename(biomass_mg = dry_mass_mg) %>% 
  ## Add taxonomy
  hellometry::add_taxonomy() %>% 
  ## Remove columns we have already, then rename old ones
  dplyr::mutate(class = Class,
                ord = Order,
                family = Family,
                subfamily = Subfamily,
                genus = Genus,
                species = Species) %>% 
  dplyr::select(-Order, -Family, -Subfamily, -Genus, -Class, -Species) %>% 
  ## Get estimations
  hellometry::hello_metry()

# Trim data back into nice format
emergence <- 
  bichos_biomass_est %>% 
  ## Make column with taxon name, first 4 letters of string
  dplyr::mutate(taxon = ifelse(is.na(subfamily),
                               substr(genus,1,4), 
                               substr(subfamily,1,4))) %>% 
  ## Last NA left is Odonate
  dplyr::mutate(taxon = ifelse(is.na(taxon),
                               "Odon", taxon)) %>% 
  dplyr::select(taxon , species, bromeliad_id, date, 
                size_original, days_emergence,
                abundance, biomass_mg) %>% 
  ## Keep relevant columns
  dplyr::select(bromeliad_id, taxon, abundance, 
                biomass_mg, days_emergence) %>% 
  ## Make biomass numeric
  dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
  ## Add treatments
  dplyr::left_join(bromeliad_stub %>% 
                     dplyr::select(bromeliad_id, predator,
                                   resource),
                   by = "bromeliad_id")
  


# Preparing leftover communities -------------------------------------------------------------------
leftover <- 
  communities %>% 
  ## Keep those after the manipuation
  dplyr::filter(date == "04/05/2022") %>% 
  ## Remove extra column at end and date
  dplyr::select(-date, -'...11') %>% 
  ## Remove destroyed, unidentifiable specimen
  dplyr::filter(!is.na(Class)) %>% 
  ## Rename column
  dplyr::rename(size_mm = length_mm) %>% 
  ## Add columns needed by package
  dplyr::mutate(stage = ifelse(Species == "pupa",
                               "pupa", "larva"),
                bwg_name = NA,
                abundance = 1,
                biomass_type = "dry") %>% 
  ## Add unknown to missing sizes
  dplyr::mutate(size_mm = ifelse(size_mm == "missing" |
                                   is.na(size_mm),
                                 "unknown", size_mm)) %>% 
  ## Rename column with biomass
  dplyr::rename(biomass_mg = dry_mass_mg) %>% 
  ## Add taxonomy
  hellometry::add_taxonomy() %>% 
  ## Add subclass for leeches
  dplyr::mutate(subclass = ifelse(stringr::str_detect(Species, "leech"),
                                  "Hirudinea", subclass)) %>% 
  ## Remove columns we have already, then rename old ones
  dplyr::mutate(class = Class,
                ord = Order,
                family = Family,
                subfamily = Subfamily,
                genus = Genus,
                species = Species) %>% 
  dplyr::select(-Order, -Family, -Subfamily, -Genus, -Class, -Species) %>% 
  ## Get estimations
  hellometry::hello_metry() %>% 
  ## Make column with taxon name, first 4 letters of string
  dplyr::mutate(taxon = ifelse(!is.na(genus),
                               substr(genus,1,4), 
                               ifelse(!is.na(subfamily),
                                      substr(subfamily,1,4), 
                                      ifelse(!is.na(family),
                                             substr(family,1,4),
                                             ifelse(!is.na(ord),
                                                    substr(ord,1,4),
                                                    substr(class,1,4)))))) %>% 
  ## Keep relevant columns
  dplyr::select(bromeliad_id, taxon, abundance, 
                biomass_mg) %>% 
  ## Make biomass numeric
  dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
  ## Add treatments
  dplyr::left_join(bromeliad_stub %>% 
                     dplyr::select(bromeliad_id, predator,
                                   resource),
                   by = "bromeliad_id")



# Gather how many we were able to salvage ---------------------------------------------
salvaged <- 
  # Create blank tibble with the number of input larvae
  tibble::tibble(bromeliad_id = rep(bromeliad_stub$bromeliad_id,
                                    each = 5),
                 predator = rep(bromeliad_stub$predator,
                                each = 5),
                 resource = rep(bromeliad_stub$resource,
                                each = 5),
          taxon = rep(c("Tany", "Poly", 
                       "Weom", "Tipu",
                       "Odon"), 32),
          n_input = c(rep(c(1, 4, 7, 3, 1), 16),
                    rep(c(1, 4, 7, 3, 0), 16))) %>% 
  # Add emergence, sum by bromeliad only for relevant taxa
  dplyr::left_join(emergence %>%
                     dplyr::select(-biomass_mg, -days_emergence, 
                                   -abundance) %>% 
                     dplyr::filter(taxon %in% c("Tany", "Poly", 
                                                "Weom", "Tipu",
                                                "Odon")) %>% 
                     dplyr::group_by(dplyr::across()) %>% 
                     dplyr::tally() %>% 
                     dplyr::rename(n_emerged = n),
                   by = c("bromeliad_id", "taxon","predator", "resource")) %>% 
  # Add emergence, sum by bromeliad only for relevant taxa, also add treatments
  dplyr::left_join(leftover %>% 
                     dplyr::select(-biomass_mg, -abundance) %>% 
                     dplyr::filter(taxon %in% c("Tany", "Poly", 
                                                "Weom", "Tipu",
                                                "Odon")) %>% 
                     dplyr::group_by(dplyr::across()) %>% 
                     dplyr::tally() %>% 
                     dplyr::rename(n_leftover = n),
                   by = c("bromeliad_id", "taxon", "predator", "resource")) %>% 
  # Replace NAs by 0s in the joined columns
  tidyr::replace_na(list(n_emerged = 0,
                         n_leftover = 0)) %>% 
  # Do the sum of how much we gathered
  dplyr::mutate(prop_emerged = n_emerged/n_input,
                prop_leftver = n_leftover/n_input,
                prop_salvaged = (n_emerged + n_leftover)/n_input)
  
# models ------------------------------------------------------------------


  

# Model on survival rates

# Models on water parameters

# Models on biomass out

# Models on extra stuff in