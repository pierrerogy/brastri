# Clean and Combine data

# Load libraries
library(tidyverse)
library(here)
source(here::here("R",
                  "functions.R"))
library(hellometry)

# Bromeliad plant data ----------------------------------------------------
# Read data
temp1 <- 
  readr::read_csv(here::here("brasil",
                             "bromeliads.csv"))
temp2 <- 
  readr::read_csv(here::here("trini",
                             "bromeliads.csv"))
temp3 <- 
  readr::read_csv(here::here("trini",
                             "detritus.csv"))

# Clean Brasil data
## Check
str(temp1)
## Cleaning
temp1 <- 
  temp1 %>% 
  ## Rename columns
  dplyr::rename(actual_volume_mL = volume_mL,
                water_holding_capacity_mL = holding_capacity_mL,
                loose_det_g = coarse_det_g,
                coarse_det_g = medium_det_g) %>% 
  ## Get decomposition rates
  dplyr::mutate(prop_loss_coarse_normal = (coarse_init_g - coarse_final_g) / coarse_init_g,
                prop_loss_fine_normal = (fine_init_g - fine_final_g) / fine_init_g,
                prop_loss_coarse_dry = (coarse_init_g - coarse_final_dry_g) / coarse_init_g, 
                prop_loss_fine_dry = (fine_init_g - fine_final_dry_g) / fine_init_g) %>% 
  ## Remove old columns
  dplyr::select(-contains(c("init", "dry_g", "final_g"))) %>% 
  ## Add country, site and species column
  dplyr::mutate(country = "bras",
                site = "regua",
                bromspecies = "alcantarea_imperialis",
                distance_from_ground_cm = 0)

# Clean Trini data
## Check
str(temp2)
## Cleaning
temp2 <- 
  temp2 %>%   
  ## Rename bromeliad id column
  dplyr::rename(bromeliad_id = bromeliad_id_old) %>% 
  ## Remove empty rows
  dplyr::filter(!is.na(genus)) %>% 
  ## Make a species column
  dplyr::rename(species = genus) %>% 
  dplyr::mutate(species = paste0(species, "_sp")) %>% 
  ## Rename columns and make new ones
  dplyr::rename(day = date_collected,
                height_cm = height_mm,
                width_cm = width_mm,
                bromspecies = species) %>% 
  dplyr::mutate(country = "trini") %>% 
  ## Get decomposition rates
  dplyr::mutate(prop_loss_coarse_dry = ifelse(is.na(busted_coarse),
                                               (coarse_init_g - coarse_final_dry_g) / coarse_init_g, NA),
                prop_loss_fine_dry = ifelse(is.na(busted_fine),
                                             (fine_init_g - fine_final_dry_g) / fine_init_g, NA)) %>% 
  ## Remove old columns
  dplyr::select(-contains(c("init", "dry_g", "busted")),
                -`...19`) %>% 
  ## Add detrital content
  dplyr::left_join(temp3 %>% 
                     ## Get mass of litter
                     dplyr::mutate(mass_g = dry_mass_g - paper_mass_g) %>% 
                     ## Remove negative ones
                     dplyr::mutate(mass_g = ifelse(mass_g < 0,
                                                   NA, mass_g)) %>% 
                     ## Remove columns
                     dplyr::select(-bag_number, - paper_mass_g, -dry_mass_g) %>%
                     ## Make column data match that of temp1
                     dplyr::mutate(`mesh size` = paste0(`mesh size`, "_det_g")) %>% 
                     ## Filter empty rows
                     dplyr::filter(!is.na(bromeliad_id)) %>% 
                     ## Pivot wider
                     tidyr::pivot_wider(values_from = mass_g,
                                        names_from = `mesh size`),
                   by = "bromeliad_id") %>% 
  ## Replace id with experimental id
  dplyr::mutate(bromeliad_id = ifelse(!is.na(bromeliad_exp_id),
                                      bromeliad_exp_id, bromeliad_id)) %>% 
  dplyr::select(-bromeliad_exp_id)

# Combine data
bromeliad_data <- 
  temp1 %>% 
  dplyr::bind_rows(temp2) %>% 
  ## changing factor level name to not to have to create a factor later on
  dplyr::mutate(resource = ifelse(resource == "normal",
                                  "control", "enriched"))

# Explore data to confirm there is nothing wrong
hist(bromeliad_data$height_cm)
hist(bromeliad_data$width_cm)
hist(bromeliad_data$actual_volume_mL) ## remember that huge bromeliad full of nothing
hist(bromeliad_data$loose_det_g) ## same here
hist(bromeliad_data$coarse_det_g)
hist(bromeliad_data$fine_det_g)
hist(bromeliad_data$v_final_mL)
hist(bromeliad_data$prop_loss_coarse_normal)
hist(bromeliad_data$prop_loss_fine_normal)
hist(bromeliad_data$prop_loss_coarse_dry)
hist(bromeliad_data$prop_loss_fine_dry)
hist(bromeliad_data$distance_from_ground_cm)
hist(bromeliad_data$longest_leaf_length_mm)
unique(bromeliad_data$bromeliad_id)

# Save data
readr::write_csv(bromeliad_data %>% 
                   ## Rename one bromeliad to avoid confusion later on
                   dplyr::mutate(bromeliad_id = ifelse(bromeliad_id == "E",
                                                       "F", bromeliad_id)),
                 here::here("data",
                            "bromeliad_data.csv"))

# Water parameters --------------------------------------------------------
# Read data
temp1 <- 
  readr::read_csv(here::here("brasil",
                             "water.csv"))
temp2 <- 
  readr::read_csv(here::here("trini",
                             "water.csv"))

# Clean Brasil data
## Check
str(temp1)
## Cleaning step 1
temp1 <- 
  temp1 %>% 
  ## Make country column
  dplyr::mutate(country = "bras") %>% 
  ## Get chlorophyll, total and phosphate, nh4 concentrations
  dplyr::rowwise() %>% 
  dplyr::mutate(chloro_ugL = (29.6 * ((absorbance_665_a - absorbance_750_a) - (absorbance_665_b - absorbance_750_b)) * v_extracted) / v_water_filtered_ml,
                total_p_ugL = sum(po4_ugL_1, po4_ugL_2, na.rm = 2)/2,
                nh4_ugL = sum(nh4_ugl_2, nh4_ugL_2, na.rm = 2)/2) %>% 
  ## Remove old columns
  dplyr::select(country, bromeliad_id:conductivity_uScm, chloro_ugL, total_p_ugL, nh4_ugL)
## Cleaning step 2
temp1 <- 
  temp1 %>% 
  ## Summarise values across filters
  dplyr::select(-chloro_ugL) %>% 
  dplyr::distinct() %>% 
  dplyr::left_join(temp1 %>% 
                     dplyr::select(bromeliad_id, day, chloro_ugL) %>% 
                     dplyr:: group_by(bromeliad_id, day) %>% 
                     dplyr::summarise_all(mean),
                   by = c("bromeliad_id", "day")) %>% 
  ## Replace negative values for chlorophyll with 0, and 0 for P and N with NAs
  dplyr::mutate(chloro_ugL = ifelse(chloro_ugL < 0, 
                                    0, chloro_ugL),
                total_p_ugL = ifelse(total_p_ugL == 0,
                                     NA, total_p_ugL),
                nh4_ugL = ifelse(nh4_ugL == 0,
                                 NA, nh4_ugL))

# Clean Trini data
## Check
str(temp2)
## Cleaning
temp2 <- 
  temp2 %>% 
  ## Make country column
  dplyr::mutate(country = "trini") %>% 
  # Get chlorophyll values
  dplyr::mutate(chloro_ugL = chlorophyll_raw * (solvent_added / v_filtered)) %>% 
  ## Remove old columns
  dplyr::select(country, day, bromeliad_id, temperature_C, pH, ammonium_ppm, 
                phosphate_ppm, chloro_ugL) %>% 
  ## Remove empty rows
  dplyr::filter(!is.na(bromeliad_id))
## Cleaning step 2
temp2 <- 
  temp2 %>% 
  ## Summarise values across filters
  dplyr::select(-chloro_ugL) %>% 
  dplyr::distinct() %>% 
  dplyr::left_join(temp2 %>% 
                     dplyr::select(bromeliad_id, day, chloro_ugL) %>% 
                     dplyr:: group_by(bromeliad_id, day) %>% 
                     dplyr::summarise_all(mean),
                   by = c("bromeliad_id", "day"))

# Combine data
water_data <- 
  temp1 %>% 
  dplyr::bind_rows(temp2 %>% 
                     dplyr::rename(temp_C = temperature_C)) %>% 
  ## Join treatments
  dplyr::left_join(bromeliad_data %>% 
                     dplyr::select(country, bromeliad_id, bromspecies, predator, resource),
                   by = c("country", "bromeliad_id"))

# Explore data to confirm there is nothing wrong
hist(water_data$temp_C)
hist(water_data$pH) ## one value off likely a typo
hist(water_data$conductivity_uScm) ## one value off, not sure what it is though
hist(water_data$total_p_ugL)
hist(water_data$nh4_ugL)
hist(water_data$chloro_ugL) ## One value really really far, from the rest, from trini anyways
hist(water_data$ammonium_ppm) 
hist(water_data$phosphate_ppm) 

## Modify values
water_data <- 
  water_data %>% 
  dplyr::mutate(pH = ifelse(pH == 548,
                            5.48, pH),
                conductivity_uScm = ifelse(conductivity_uScm == 862.0,
                                           NA, conductivity_uScm))

## Save data
readr::write_csv(water_data,
                 here::here("data",
                            "water_data.csv"))


# Initial and final communities -----------------------------------------------------
# Read data
temp1 <- 
  readr::read_csv(here::here("brasil",
                             "communities.csv"))
temp2 <- 
  readr::read_csv(here::here("trini",
                             "communities.csv"))

# Clean Brasil data
## Check
str(temp1)
## Cleaning
temp1 <- 
  temp1  %>% 
  ## Select relevant columns
  dplyr::select(date:dry_mass_mg) %>% 
  ## Rename
  dplyr::rename(day = date,
                biomass_mg = dry_mass_mg) %>% 
  ## Make column for beginning and end
  dplyr::mutate(when = ifelse(day == "28/03/2022",
                              "start", "end")) %>% 
  ## Make country column
  dplyr::mutate(country = "bras")

# Clean Trini data
## Check
str(temp2)
## Cleaning
temp2 <- 
  temp2 %>% 
  ## Select relevant columns
  dplyr::select(date:dry_weight_mg) %>% 
  ## Rename
  dplyr::rename(day = date,
                biomass_mg = dry_weight_mg) %>% 
  ## Keep only beginning and end days
  dplyr::filter(day %in% c("06/10/2022", "04/11/2022")) %>% 
  ## Make column for beginning and end
  dplyr::mutate(when = ifelse(day == "06/10/2022",
                              "start", "end")) %>% 
  ## Make country column and put NAs for those insects whose weight was 0
  dplyr::mutate(country = "trini",
                biomass_mg = ifelse(biomass_mg == 0,
                                    NA, biomass_mg))

# Combine data
community_data <- 
  temp1 %>% 
  dplyr::bind_rows(temp2) %>% 
  ## Join treatments
  dplyr::left_join(bromeliad_data %>% 
                     dplyr::select(country, bromeliad_id, bromspecies, predator, resource),
                   by = c("country", "bromeliad_id")) %>% 
  ## Add a abundance of 1 at the beginning
  dplyr::mutate(n = ifelse(is.na(n), 
                           1, n)) %>% 
  ## Run hellometry to get biomass estimates
  ### Have to prepare data
  dplyr::rename_with(str_to_lower) %>% 
  dplyr::rename(abundance = n,
                size_mm = length_mm,
                ord = order) %>% 
  dplyr::mutate(bwg_name = NA,
                biomass_type = "dry",
                stage = ifelse(class == "Hexapoda", 
                               "larva", "adult"),
                size_mm = ifelse(is.na(size_mm),
                                 "unknown", size_mm)) %>% 
  dplyr::mutate(stage = ifelse(is.na(stage),
                               "adult", ifelse(stringr::str_detect(string = species, 
                                                                   pattern = "pupa") & 
                                                 !is.na(species),
                               "pupa", stage)))

## Add the missing columns
community_data <- 
  community_data %>% 
  # extra cols for everyone
  dplyr::mutate(domain = "Eukaryota",
                kingdom = "Animalia",
                subphylum = NA,
                tribe = NA) %>% 
  # extra cols that change
  dplyr::mutate(phylum = ifelse(class == "Hexapoda",
                                "Arthropoda", "Annelida"),
                subclass = ifelse(class == "Clitellata" & 
                                    stringr::str_detect(string = species,
                                                        pattern = "leech"),
                                  "Hirudinea", ifelse(
                                    class == "Clitellata" & 
                                      !stringr::str_detect(string = species,
                                                           pattern = "leech"),
                                    "Oligochaeta", ifelse(
                                      class == "Hexapoda",
                                      "Neoptera", NA))),
                subord = ifelse(family %in% c("Chironomidae", "Tipulidae",
                                              "Ceratopogonidae",
                                              "Culicidae", "Psychodidae"),
                                "Nematocera", NA)) %>% 
  # Relocate
  dplyr::relocate(domain, kingdom, phylum, subphylum,
                  .before = class) %>% 
  dplyr::relocate(subclass,
                  .after = class) %>% 
  dplyr::relocate(subord,
                  .after = ord) %>% 
  dplyr::relocate(tribe,
                  .after = subfamily)

## Modify values
community_data <- 
  community_data %>% 
  dplyr::mutate(genus = ifelse(genus == "Weomyia" & !is.na(genus),
                               "Wyeomyia", genus),
                class = ifelse(ord == "Clitellata"& !is.na(ord),
                               "Clitellata", class),
                ord = ifelse(ord == "Clitellata"& !is.na(ord),
                               NA, ord),
                ord = ifelse(ord == "Oligochaeta"& !is.na(ord),
                                NA, ord),
                family = ifelse(family == "Oligochaeta"& !is.na(family),
                                NA, family),
                family = ifelse(genus == "Wyeomyia" & !is.na(genus), 
                                "Culicidae", family),
                family = ifelse(subfamily %in% c("Tanypodinae", "Chironominae") & !is.na(subfamily),
                                "Chironomidae", family),
                family = ifelse(genus == "Trentepohlia" & !is.na(genus),
                                "Tipulidae", family))



# Get biomass
community_data <-
  community_data %>%
  hellometry::hello_metry()

  
# Explore data to confirm there is nothing wrong
hist(as.numeric(community_data$size_original))
hist(as.numeric(community_data$biomass_mg)) 
unique(community_data$class)
unique(community_data$ord)
unique(community_data$family)
unique(community_data$subfamily)
unique(community_data$genus)
unique(community_data$species)

## Save data
readr::write_csv(community_data,
                 here::here("community_data.csv"))

# Emerged -----------------------------------------------------------------
# Read data
temp1 <- 
  readr::read_csv(here::here("brasil",
                             "bichos.csv"))
temp2 <- 
  readr::read_csv(here::here("trini",
                             "communities.csv"))

# Clean Brasil data
## Check
str(temp1)
## Cleaning
temp1 <- 
  temp1 %>% 
  ## Make country column
  dplyr::mutate(country = "bras") %>% 
  ## Keep experimental bromeliads only
  dplyr::filter(stringr::str_detect(string = bromeliad_id, pattern = "E")) %>% 
  ## Keep important columns
  dplyr::select(country, bromeliad_id:dry_mass_mg)%>% 
  ## Rename
  dplyr::rename(day = date)
  

# Clean Trini data
## Check
str(temp2)
## Cleaning
temp2 <- 
  temp2 %>% 
  ## Make country column
  dplyr::mutate(country = "trini") %>% 
  ## Select relevant columns
  dplyr::select(country, date:dry_weight_mg) %>% 
  ## Rename
  dplyr::rename(day = date,
                dry_mass_mg = dry_weight_mg) %>% 
  ## Keep only beginning and end days
  dplyr::filter(day %notin% c("06/10/2022", "04/11/2022"))

# Combine data
emergence_data <- 
  temp1 %>% 
  dplyr::bind_rows(temp2) %>% 
  ## Join treatments
  dplyr::left_join(bromeliad_data %>% 
                     dplyr::select(country, bromeliad_id, bromspecies, predator, resource),
                   by = c("country", "bromeliad_id"))
# Explore data to confirm there is nothing wrong
hist(as.numeric(emergence_data$length_mm)) ## Stupid conversion error on excel
hist(emergence_data$dry_mass_mg) 
hist(emergence_data$n)
unique(emergence_data$Class)
unique(emergence_data$Order)
unique(emergence_data$Family)
unique(emergence_data$Subfamily)
unique(emergence_data$Genus)
unique(emergence_data$Species)

# Fix data
emergence_data <- 
  emergence_data %>% 
  dplyr::mutate(length_mm = ifelse(length_mm %in% c("44625", "44566"),
                                   NA, length_mm),
                Species = ifelse(Species == "flly3",
                                 "fly3", Species),
                Family = ifelse(Family == "Chrysomleidae",
                                "Chrysomelidae", ifelse(Family == "Formcidae",
                                                        "Formicidae", Family)))  %>% 
  ### Have to prepare data for hellometry
  dplyr::rename_with(str_to_lower) %>% 
  dplyr::rename(abundance = n,
                size_mm = length_mm,
                ord = order)

## Save data
readr::write_csv(emergence_data,
                 here::here("data",
                            "emergence_data.csv"))



# Phosphorus content ------------------------------------------------------
# Read data
## I put in in the brasil folder but it is for both sites
temp1 <- 
  readr::read_csv(here::here("brasil",
                             "pcontent.csv")) %>% 
  ## Remove the one mosquito with blood
  dplyr::filter(tin_ID != "E3_adult_CULI_with_blood") %>% 
  ## Make that both clearer
  dplyr::mutate(taxon = ifelse(taxon == "both",
                               "CulicidaeTipulidae", taxon)) %>% 
  ## Make one new column with taxon, old one will be species
  ## NB: only tipulidae has more than one species
  dplyr::rename(species = taxon) %>% 
  dplyr::mutate(taxon = ifelse(stringr::str_detect(string = species,
                                                   pattern = "Tipu"),
                                                   "Tipulidae", species)) %>% 
  dplyr::mutate(taxon = stringr::str_replace(string = taxon,
                                             pattern = "_.*",
                                             replacement = "")) %>% 
  
  ## Join treatments
  ### First need to change brasil to bras
  dplyr::mutate(country = ifelse(country == "brasil",
                                 "bras", country)) %>% 
  dplyr::left_join(bromeliad_data %>% 
                     dplyr::select(country, bromeliad_id, bromspecies, predator, resource),
                   by = c("country", "bromeliad_id")) %>% 
  # Rename p_content and mass analysed
  dplyr::rename(mass_measured = Nessel_invrt_mass,
                p_prcnt = `P%`) %>% 
  # Remove % in p_prcnt
  dplyr::mutate(p_prcnt = stringr::str_remove(string = p_prcnt,
                                              pattern = "%")) %>% 
  ## Select columns of interest
  dplyr::select(country, bromeliad_id, bromspecies, predator, 
                resource, taxon, species, stage, n_indiv, invert_mass_mg,
                mass_measured, p_prcnt)

# Save data
readr::write_csv(temp1,
                 here::here("data",
                            "pcontent.csv"))

