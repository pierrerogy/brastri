# Preparation of Trinidad data

# Libraries
library(tidyverse)
library(ggplot2)
library(here)
library(hellometry)
source(here::here("R",
                  "functions.R"))

# Load data
inverts <- 
  readr::read_csv(here::here("trini",
                             "inverts.csv"))
bromeliads <- 
  readr::read_csv(here::here("trini",
                             "bromeliads.csv"))
sites <- 
  readr::read_csv(here::here("trini",
                             "sites.csv"))
communities <- 
  readr::read_csv(here::here("trini",
                             "communities.csv"))

detritus <- 
  readr::read_csv(here::here("trini",
                             "detritus.csv"))


# Get how many bromeliads per site ----------------------------------------
bromeliads %>% 
  dplyr::select(bromeliad_id_old, site) %>% 
  unique() %>% 
  dplyr::group_by(site) %>% 
  dplyr::tally()

# Get density of invertebrates --------------------------------------------
# Fix species names
inverts <- 
  inverts %>% 
  dplyr::mutate(Species = stringr::str_replace(string = Species,
                                               pattern = "mystery_",
                                               replacement = "mystery"))
# Compile names
inverts <- 
  get_specnames(inverts, short = TRUE)

# Data
inverts_sorted <- 
  inverts %>% 
  dplyr::filter(stringr::str_detect(bromeliad_id,
                                    "B")) %>% 
  ## Add site info 
  dplyr::left_join(bromeliads %>% 
                     dplyr::select(site, bromeliad_id_old) %>% 
                     dplyr::rename(bromeliad_id = bromeliad_id_old),
                   by = "bromeliad_id") %>% 
  dplyr::select(site, bromeliad_id, Class, Order, Family, Species, n) %>% 
  dplyr::group_by(site, bromeliad_id, Class, Order, Family, Species) %>% 
  dplyr::summarise_all(sum) %>% 
  dplyr::ungroup()

# Sum for all sites
inverts_sorted_site <- 
  inverts_sorted %>% 
  dplyr::select(site, Species, n) %>% 
  dplyr::group_by(site, Species) %>% 
  dplyr::summarise_all(sum) %>% 
  dplyr::mutate(n = n/sum(n)) %>% 
  dplyr::arrange(desc(n),.by_group = T) %>% 
  dplyr::ungroup()

# Print most abundant species 
inverts_sorted_site 

## LOOKS LIKE LAS LAPAS IS THE BEST SITE

# Get density per mL water
objectives <- 
  inverts_sorted %>% 
  dplyr::filter(site == "las_lapas") %>% 
  dplyr::left_join(bromeliads%>% 
                     dplyr::rename(bromeliad_id = bromeliad_id_old) %>%
                     dplyr::select(bromeliad_id, actual_volume_mL),
                   by = "bromeliad_id") %>% 
  dplyr::mutate(density_nat = n/actual_volume_mL) %>% 
  ## Replace the Inf by NA for the ones where there was no water
  dplyr::mutate(density_nat = ifelse(is.finite(density_nat),
                                     density_nat, NA )) %>% 
  dplyr::select(site, Class, Order, Family, Species, density_nat) %>% 
  dplyr::group_by(site, Class, Order, Family, Species) %>% 
  dplyr::summarise_all(~mean(., na.rm = T)) %>% 
  dplyr::ungroup() %>% 
  # here just seeing  what we would have with the five
  # most abundant in 100mL water
  dplyr::filter(stringr::str_detect(string= Species, 
                                    pattern = "Tipulidae_sp1|Scirtes_sp1|Polypedilum_sp1|Odonata_sp1|Dytiscidae_larva1|Tanypodinae_sp1")) %>% 
  dplyr::mutate(density_nat = round(density_nat * 100)) %>% 
  dplyr::rename(density_for_100mL_brom = density_nat) %>% 
  ## Target density is half the natural one
  dplyr::mutate(target_density = ceiling(density_for_100mL_brom/2),
                objective_total = ifelse(stringr::str_detect(string= Species, 
                                                             pattern = "Odonata|Dytiscidae_larva1"),
                                         20,
                                         target_density * 32 + 15))

# Save density  
readr::write_csv(objectives,
                 here::here("trini",
                            "objectives.csv")) 
# Compute detritus mass ---------------------------------------------------
detritus_computed <- 
  detritus[1:5] %>% 
  ## Get detritus mass
  dplyr::mutate(detritus_mass_g = dry_mass_g - paper_mass_g) %>% 
  ## Replace the negative values with 0
  dplyr::mutate(detritus_mass_g = ifelse(detritus_mass_g < 0,
                                         0, detritus_mass_g)) %>% 
  ## Remove bag number column
  dplyr::select(-bag_number, -dry_mass_g, -paper_mass_g) %>% 
  ## Remove those without names
  dplyr::filter(bromeliad_id != "") %>% 
  ## Make data wider
  tidyr::pivot_wider(names_from = "mesh size",
                     values_from = "detritus_mass_g") %>% 
  dplyr::rename(fine_det_g = fine,
                loose_det_g = loose,
                coarse_det_g = coarse)
  






# Communities -------------------------------------------------------------
communities_emerged <- 
  communities %>% 
  dplyr::select(date:length_mm ) %>% 
  get_specnames() %>% 
  dplyr::filter(Species %in% c("Tipulidae_adult1", "Tipulidae_adult3",
                               "Tipulidae_adult", "Tipulidae_adult7",
                               "Psychodidae_adult1") | 
                  stringr::str_detect(Species,"Chironomidae_adult")) %>% 
  dplyr::select(date, bromeliad_id) %>% 
  unique()


# Plot of morphospecies abundance in each site ----------------------------
# Make labels for facets
site_names <- 
  c(
  `arima_valley` = "Arima Valley (n = 37)",
  `brasso_seco` = "Brasso Seco (n = 48)",
  `la_laja` = "La Laja (n = 12)",
  `las_lapas` = "Las Lapas (n = 22)",
  `marianne_river` = "Marianne River (n = 11)",
  `morne_bleu` = "Morne Bleu (n = 9)")

# Make plots
abunplot <- 
  ggplot(data = inverts_sorted_site %>% 
           dplyr::filter(site != "F7"),
         aes(x = factor(Species, 
                        levels=names(sort(table(Species), 
                                          decreasing=TRUE))),
             y  = n)) +
  geom_bar(stat="identity") +
  ylab("Relative abundance") +
  xlab("Morphospecies") +
  facet_wrap(~site,
             scales = "free_y", 
             labeller = as_labeller(site_names)) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, vjust = 0.8, hjust = 0.4))

# Save
ggsave(here::here("plots",
                  "trini_site_plots.jpeg"),
       abunplot,
       height = 10,
       width = 20)
# draft -------------------------------------------------------------------

# Get density per mL water
  bromeliads %>%
  dplyr::filter(site == "las_lapas") %>% 
  dplyr::rename(bromeliad_id = bromeliad_id_old) %>%
  dplyr::select(bromeliad_id, actual_volume_mL) %>% 
  dplyr::left_join(inverts_sorted %>% 
                     dplyr::select(Species, n, bromeliad_id) %>% 
                     dplyr::filter(stringr::str_detect(string= Species, 
                                                       pattern = "Tipulidae_sp1|Scirtes_sp1|Polypedilum_sp1|Odonata_sp1|Dytiscidae_larva1|Tanypodinae_sp1")),
                     by = "bromeliad_id") %>% 
  tidyr::pivot_wider(names_from = "Species",
                      values_from = "n",
                     values_fill = 0) %>% 
  dplyr::mutate(across(.cols = c("Dytiscidae_larva1":"Odonata_sp1" ),
                       .fns = ~./actual_volume_mL)) %>% 
  dplyr::select(-bromeliad_id, - actual_volume_mL) %>% 
  ## Replace the Inf by NA for the ones where there was no water
  dplyr::mutate(across(.cols = c("Dytiscidae_larva1":"Odonata_sp1" ),
                .fns = ~ifelse(is.finite(.x),
                                     .x, NA ))) %>%
  summarise(across(everything(), 
                   mean,
                   na.rm = TRUE)) %>% 
  tidyr::pivot_longer(cols = c("Dytiscidae_larva1":"Odonata_sp1" ),
                      names_to = "Species",
                      values_to = "density_nat") %>% 
  dplyr::mutate(density_nat = round(density_nat * 100)) %>% 
  dplyr::rename(density_for_100mL_brom = density_nat) %>% 
  ## Target density is half the natural one
  dplyr::mutate(target_density = ceiling(density_for_100mL_brom/2),
                objective_total = ifelse(stringr::str_detect(string= Species, 
                                                             pattern = "Odonata|Dytiscidae_larva1"),
                                         20,
                                         target_density * 32 + 15))
  
