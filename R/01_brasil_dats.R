# Cleaning and exploring data from Brazil

# Libraries
library(tidyverse)
library(ggplot2)
library(here)

# Read data
bichos <- 
  readr::read_csv(here::here("brasil",
                             "bichos.csv"))
bromeliads <- 
  readr::read_csv(here::here("brasil",
                             "bromeliads.csv"))
communities <- 
  readr::read_csv(here::here("brasil",
                             "communities.csv"))


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
                                                 "Leptoconopinae", Subfamily))))
  



# Filtering relevant insects from the emergence traps ---------------------
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
                  lubridate::yday(lubridate::as_date(lubridate::dmy("31/03/2022"))))

# See how many we have in terms of families
bichos_final <- 
  bichos_emerged %>% 
  dplyr::select(Class, Order, Family, Subfamily, Genus, n) %>% 
  dplyr::group_by(Class, Order, Family, Subfamily, Genus) %>% 
  dplyr::summarise_all(sum) %>% 
  dplyr::ungroup()


# Summarising communities -------------------------------------------------
# Data
communities_end <- 
  communities %>% 
  ## Data from after the manipuation
  dplyr::filter(date == "04/05/2022")

# Sum at subfamilylevel
communities_final <- 
  communities_end %>% 
  ## remove unidentified debris
  dplyr::filter(!is.na(Class)) %>% 
  dplyr::select(Class, Order, Family, Subfamily, Genus) %>% 
  dplyr::group_by(Class, Order, Family, Subfamily, Genus) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup()


# How many did we end up with ---------------------------------------------
final <- 
  communities_final %>%
  dplyr::rename(n_in_water = n) %>%
  ## Attach emerged
  dplyr::left_join(bichos_final %>% 
                     dplyr::rename(n_emerged = n),
                   by = c("Class", "Order", 
                          "Family", "Subfamily", "Genus")) %>% 
  ## Make 0 for NAs in emerged
  dplyr::mutate(n_emerged = ifelse(is.na(n_emerged),
                                   0, n_emerged)) %>% 
  ## Add row for Polypedilum
  tibble::add_row(Class = "Hexapoda",
                  Order = "Diptera",
                  Family = "Chironomidae",
                  Subfamily = "Chironominae",
                  Genus = "Polypedilum",
                  n_in_water = NA,
                  n_emerged = NA) %>% 
  ## Make column with quantity of larvae in input 
  dplyr::mutate(input = ifelse(Family == "Culicidae",
                               7 * 32, ifelse(
                                 Family == "Coenagrionidae",
                                 1 * 16, ifelse(
                                   Family == "Tipulidae",
                                   3 * 16, ifelse(
                                     Subfamily == "Chironominae" & Genus == "Polypedilum",
                                                  4 * 32, ifelse(
                                                    Subfamily == "Tanypodinae",
                                                    1 * 32, NA)))))) %>% 
  ## Now get how many we salvaged at the end
  dplyr::mutate(prop_salvaged = (n_in_water + n_emerged)/input) %>% 
  ## Add 0 to the others
  dplyr::mutate(input = ifelse(is.na(input),
                               0, input)) 
  
## Save final data
readr::write_csv(final,
                 here::here("brasil",
                            "final.csv"))
