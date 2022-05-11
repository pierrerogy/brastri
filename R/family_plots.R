# Quick family plots for Diane

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

# Collected bromeliads ----------------------------------------------------
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
                             7, 
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


# Plot
family_plot <- 
  ggplot(data = bichos_fam ,
       aes(x = Subfamily,
           y = n + 1)) +
  geom_bar(stat = "identity") +
  ylab("Abundance") +
  facet_wrap(~bromeliad_id) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 45))

# Save
ggplot2::ggsave(plot = family_plot,
                filename = here::here("plots",
                           "family_plot.jpg"),
                width = 9,
                height = 6)


# Lets see how many mosquitoes emerged ------------------------------------
# Data
moz <- 
  bichos %>% 
  dplyr::filter(stringr::str_detect(bromeliad_id,
                                    "E")) %>% 
  dplyr::filter(Family == "Culicidae" &
                  Species == "sp1") %>% 
  dplyr::select(bromeliad_id, n) %>% 
  dplyr::group_by(bromeliad_id) %>% 
  dplyr::summarise_all(sum) %>% 
  dplyr::left_join(bromeliads %>% 
                     dplyr::select(bromeliad_id, resource, predator) %>% 
                     dplyr::filter(stringr::str_detect(bromeliad_id,
                                                       "E")),
                   by = "bromeliad_id")
# Mosquitoes out
32*7
sum(moz$n)/(32*7)
