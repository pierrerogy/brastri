# Miscellaneous

# Libraries
library(tidyverse)
library(here)
library(brms)
library(bayestestR)
source(here::here("brastri",
                  "functions.R"))

# Load data
## Aquatic communities
community <-
  readr::read_csv(here::here("brastri", "data",
                             "community_data.csv")) %>% 
  ## Remove ci columns and convert biomass data
  dplyr::mutate(dry_mass_mg = ifelse(is.na(dry_mass_mg),
                                     biomass_mg, dry_mass_mg)) %>% 
  dplyr::select(-contains("ci"), - biomass_mg, -size_used_mm, -bwg_name, -path, -size_original, -day,
                -stage, -abundance)


# Plot of initial biomasses -----------------------------------------------
# Make dats
dats <- 
  community %>% 
  dplyr::filter(when == "start" & ord != "Odonata") %>% 
  dplyr::select(-when) %>%
  ## Make species name for plotting
  get_specnames() %>% 
  dplyr::mutate(species = ifelse(stringr::str_detect(string = species, pattern = "Tipu"),
                                                     "Tipulidae", species),
                dry_mass_mg = as.numeric(dry_mass_mg)) %>% 
  dplyr::select(country:species) %>% 
  ## Summarise by site
  group_by(country, species) %>% 
  dplyr::summarise(dplyr::across(dry_mass_mg,
                   list(sum = sum, sd = sd))) %>% 
  ## Reorder species name as factor, whose level follows growth rate
  dplyr::mutate(species = factor(species, levels = c("Scirtidae_Scirtes", "Tipulidae", 
                                                    "Chironomidae_Tanypodinae", "Chironominae_Polypedilum", 
                                                    "Culicidae_Weomyia")))

# Make list of picture labels
labels <- c(Chironomidae_Tanypodinae = "<img src='brastri/www/chir.jpg' width='25' /><br>",
            Chironominae_Polypedilum = "<img src='brastri/www/chir.jpg' width='25' /><br>",
            Culicidae_Weomyia = "<img src='brastri/www/culi.jpg' width='25' /><br>",
            Scirtidae_Scirtes = "<img src='brastri/www/scir.jpg' width='25' /><br>",
            Tipulidae = "<img src='brastri/www/tipu.jpg' width='25' /><br>") # replace with whatever

# Make plot
biomass_plot_before <- 
  ggplot(data = dats,
       aes(x = species,
           y = dry_mass_mg_sum)) +
  geom_point() +
  geom_errorbar(aes(ymin = dry_mass_mg_sum - dry_mass_mg_sd, 
                    ymax = dry_mass_mg_sum + dry_mass_mg_sd), width = 0.2) +
  facet_wrap(~country,
             labeller = labeller(country = 
                                   c("bras" = "BR",
                                     "trini" = "TT"))) +
  xlab("") +
  ylab("Estimated dry mass(mg)") +
  scale_x_discrete(labels = labels) +
  theme(axis.text.x = ggtext::element_markdown(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Save plot
ggsave(here::here("brastri", "www",
                 "biomass_plot_before.jpg"),
      biomass_plot_before,
      width = 7,
      height = 5,
      bg = "white")

