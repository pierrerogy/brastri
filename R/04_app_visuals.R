# Visuals for app

# Libraries
library(tidyverse)
library(ggtext)
library(here)
source(here::here("brastri",
                  "functions.R"))

# Plot of everything that emerged  ------------------
# Prepare data
dats <- 
  emergence %>% 
  ## Let's group species by family to make plot legible
  dplyr::mutate(family = ifelse(is.na(family),
                                "uk", family),
                ord = ifelse(is.na(ord),
                             "uk", ord)) %>% 
  dplyr::mutate(species = ifelse(family != "uk",
                                 family, ifelse(family == "uk" & ord != "uk",
                                                ord, species))) %>% 
  ## For those that didn't have family, keep the lowest taxonomic level we had
  dplyr::mutate(species = stringr::str_remove(string = species,
                                              pattern = "_.*$")) %>% 
  ## We only want to combine species in each country
  dplyr::select(country, species) %>% 
  dplyr::group_by(country, species) %>% 
  dplyr::tally() %>% 
  dplyr::ungroup() %>% 
  ## Bolden names of those potentially bromeliad associated
  dplyr::mutate(species = ifelse(species %in% c("Tipulidae", "Chironomidae",
                                                "Culicidae", "Dolichopodidae",
                                                "Ceratopogonidae", "Psychodidae",
                                                "Syrphidae", "Coenagrionidae"),
                                 paste0("<b>", species, "</b>"), species))

# Create ordered vector of "species" name to order bars
temp1 <- 
  dats %>% 
  dplyr::select(-country) %>% 
  dplyr::group_by(species) %>% 
  dplyr::summarise_all(sum) %>% 
  dplyr::arrange(dplyr::desc(n)) %>% 
  dplyr::select(species) %>% 
  dplyr::pull()

# Reorder factor in dats
dats <- 
  dats %>% 
  dplyr::mutate(species = factor(species, levels = temp1))

# Now plot data
figure6 <- 
  ggplot(data = dats,
         aes(x = species, 
             y = n)) +
  geom_bar(stat="identity") +
  facet_wrap(~country,
             labeller = labeller(country = 
                                   c("bras" = "BR",
                                     "trini" = "TT"))) +
  xlab("") +
  ylab("Amount collected") +
  theme(axis.text.x = element_markdown(angle = 90,
                                       size = rel(0.7)),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))
# Save plot
ggsave(here::here("brastri", "www",
                  "emergence_everything.jpg"),
       figure6,
       width = 9,
       height = 5,
       bg = "white")

# Plot of initial biomasses -----------------------------------------------
# Make dats
dats <- 
  community %>% 
  dplyr::filter(when == "start" & ord != "Odonata") %>% 
  dplyr::select(-when, -bromspecies) %>%
  ## Make species name for plotting
  get_specnames() %>% 
  dplyr::mutate(species = ifelse(stringr::str_detect(string = species, pattern = "Tipu"),
                                 "Tipulidae", species),
                biomass_mg = as.numeric(biomass_mg)) %>% 
  dplyr::select(country, species, biomass_mg) %>% 
  ## Summarise by site
  group_by(country, species) %>% 
  dplyr::summarise(dplyr::across(biomass_mg,
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
             y = biomass_mg_sum)) +
  geom_point() +
  geom_errorbar(aes(ymin = biomass_mg_sum - biomass_mg_sd, 
                    ymax = biomass_mg_sum + biomass_mg_sd), width = 0.2) +
  facet_wrap(~country,
             labeller = labeller(country = 
                                   c("bras" = "BR",
                                     "trini" = "TT"))) +
  xlab("") +
  ylab("Estimated dry mass(mg)") +
  ylim(0,11) +
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

# Plots of final biomasses ------------------------------------------------
# Make dats
dats <- 
  community %>% 
  dplyr::filter(when == "end") %>% 
  dplyr::select(-when) %>%
  ## Add unknown class for all those unknown species
  dplyr::mutate(class = ifelse(is.na(class),
                               "unknown", class)) %>% 
  ## Make species name for plotting
  get_specnames() %>% 
  dplyr::mutate(species = ifelse(stringr::str_detect(string = species, pattern = "Tipu"),
                                 "Tipulidae", species),
                biomass_mg = as.numeric(biomass_mg)) %>% 
  dplyr::select(country:species) %>% 
  ## Summarise by site
  group_by(country, species) %>% 
  dplyr::summarise(dplyr::across(biomass_mg,
                                 list(sum = sum, sd = sd)))


# Plot 1 groups that we seeded at the beginning of the expt
## Make list of picture labels
labels <- c(Chironomidae_Tanypodinae = "<img src='brastri/www/chir.jpg' width='25' /><br>",
            Chironominae_Polypedilum = "<img src='brastri/www/chir.jpg' width='25' /><br>",
            Culicidae_Weomyia = "<img src='brastri/www/culi.jpg' width='25' /><br>",
            Scirtidae_Scirtes = "<img src='brastri/www/scir.jpg' width='25' /><br>",
            Tipulidae = "<img src='brastri/www/tipu.jpg' width='25' /><br>") # replace with whatever

## Make plot
biomass_plot_after <- 
  ggplot(data = 
           dats %>% 
           ## filter the groups I want
           dplyr::filter(species %in% c("Scirtidae_Scirtes", "Tipulidae", 
                                        "Chironomidae_Tanypodinae", "Chironominae_Polypedilum", 
                                        "Culicidae_Weomyia")) %>% 
           ## Reorder species name as factor, whose level follows growth rate
           dplyr::mutate(species = factor(species, levels = c("Scirtidae_Scirtes", "Tipulidae", 
                                                              "Chironomidae_Tanypodinae", "Chironominae_Polypedilum", 
                                                              "Culicidae_Weomyia"))),
         aes(x = species,
             y =  biomass_mg_sum)) +
  geom_point() +
  geom_errorbar(aes(ymin = biomass_mg_sum - biomass_mg_sd, 
                    ymax = biomass_mg_sum + biomass_mg_sd), width = 0.2) +
  facet_wrap(~country,
             labeller = labeller(country = 
                                   c("bras" = "BR",
                                     "trini" = "TT"))) +
  xlab("") +
  ylab("Estimated dry mass(mg)") +
  scale_x_discrete(labels = labels) +
  scale_y_continuous(breaks = c(0, 3, 6, 9, 15, 25, 30)) +
  theme(axis.text.x = ggtext::element_markdown(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Save plot
ggsave(here::here("brastri", "www",
                  "biomass_plot_after.jpg"),
       biomass_plot_after,
       width = 7,
       height = 5,
       bg = "white")

# Plot 2 tourists
biomass_plot_after_tourists <- 
  ggplot(data = 
           dats %>% 
           ## filter the groups I want
           dplyr::filter(species %notin% c("Scirtidae_Scirtes", "Tipulidae", 
                                           "Chironomidae_Tanypodinae", "Chironominae_Polypedilum", 
                                           "Culicidae_Weomyia", "Odonata_Coenagrionidae")) %>% 
           ## edit the names for easier reading
           dplyr::mutate(species = stringr::str_remove(string = species, 
                                                       pattern = ".*_")),
         aes(x = species,
             y =  biomass_mg_sum)) +
  geom_point() +
  geom_errorbar(aes(ymin = biomass_mg_sum - biomass_mg_sd, 
                    ymax = biomass_mg_sum + biomass_mg_sd), width = 0.2) +
  facet_wrap(~country,
             labeller = labeller(country = 
                                   c("bras" = "BR",
                                     "trini" = "TT"))) +
  xlab("") +
  ylab("Estimated dry mass(mg)") +
  theme(axis.text.x = element_text(angle = 90),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Save plot
ggsave(here::here("brastri", "www",
                  "biomass_plot_after_tourists.jpg"),
       biomass_plot_after_tourists,
       width = 7,
       height = 5,
       bg = "white")




# Table of how many larvae seeded, emerged, found, missing --------
# Make first part of table
table3 <- 
  tibble::tibble(species = rep(x = c("Scirtidae_Scirtes", "Tipulidae",
                                     "Chironomidae_Tanypodinae", "Chironominae_Polypedilum", 
                                     "Culicidae_Weomyia", "Odonata_Coenagrionidae"), 
                               times = 2),
                 org = rep(x = c("<i>Scirtes</i>", "Tipulidae",
                                 "Tanypodinae", "<i>Polypedilum</i>", 
                                 "<i>Weomyia</i>", "Coenagrionidae"), 
                           times = 2),
                 country = rep(x = c("bras", "trini"),
                               each = 6),
                 Seeded = c(NA, 3 * 32, 1 * 32, 4 * 32, 7 * 32, 1 * 16, # BR
                            5 * 32, 3 * 32, NA, 1 * 32, NA, NA)) # TT

# Add how many emerged
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
## Do the cleaning 
temp1 <- 
  emergence_selected %>% 
  ## Make species name for plotting
  dplyr::filter(stringr::str_detect(string = species,
                                    pattern = "Culi|Wye|Coe|Chironomidae|Tipu")) %>% 
  ## Remove species number
  dplyr::mutate(species  = stringr::str_remove(string = species,
                                               patter = "_.*$")) %>% 
  ## Sum across sites 
  dplyr::select(country, species) %>% 
  dplyr::group_by(country, species) %>% 
  dplyr::tally() %>% 
  dplyr::rename(Emerged = n) %>% 
  ## Now give the same names than in the other data frames
  dplyr::mutate(species = ifelse(species == "Coenagrionidae",
                                 "Odonata_Coenagrionidae", ifelse(species == "Chironomidae",
                                                                  "Chironominae_Polypedilum" , ifelse(species == "Culicidae",
                                                                                                      "Culicidae_Weomyia" , "Tipulidae"))))

# Add how many were found at the end
temp2 <- 
  community %>% 
  dplyr::filter(when == "end") %>% 
  ## Make species name for plotting
  get_specnames() %>% 
  ## Filter the species we want
  dplyr::mutate(species = ifelse(stringr::str_detect(string = species, pattern = "Tipu"),
                                 "Tipulidae", species)) %>% 
  dplyr::filter((species %in% c("Tipulidae", "Odonata_Coenagrionidae",
                                "Chironomidae_Tanypodinae", "Chironominae_Polypedilum") & country == "bras") |
                  (species %in% c("Scirtidae_Scirtes", "Tipulidae", "Chironominae_Polypedilum") &
                     country == "trini")) %>% 
  ## Sum across sites
  dplyr::select(country, species) %>% 
  dplyr::group_by(country, species) %>% 
  dplyr::tally() %>% 
  dplyr::rename(Remaining = n)

# Compute missing and save
table3 <- 
  table3 %>% 
  dplyr::left_join(temp1,
                   by = c("country", "species")) %>% 
  dplyr::left_join(temp2,
                   by = c("country", "species")) %>% 
  ## Put emerged chironomids for Tanypodinae 
  dplyr::mutate(Emerged = ifelse(species == "Chironomidae_Tanypodinae" & country == "bras",
                                 54, ifelse(species == "Chironomidae_Tanypodinae" & country == "trini",
                                            21, Emerged))) %>% 
  ## Replace 0 by NAs, and vice versa
  dplyr::mutate(Emerged = ifelse(!is.na(Seeded) & is.na(Emerged),
                                 0, Emerged),
                Remaining = ifelse(!is.na(Seeded) & is.na(Remaining),
                                   0, Remaining)) %>% 
  ## Remove species column
  dplyr::select(-species)


# Save table separately
## Brasil
readr::write_csv(table3 %>% 
                   dplyr::filter(country == "bras") %>% 
                   dplyr::select(-country),
                 here::here("brastri", "data",
                            "table_3a.csv"))
# Trini
readr::write_csv(table3 %>% 
                   dplyr::filter(country == "trini") %>% 
                   dplyr::select(-country),
                 here::here("brastri", "data",
                            "table_3b.csv"))

