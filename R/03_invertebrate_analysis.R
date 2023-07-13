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
## Water chemistry
water <-
  readr::read_csv(here::here("brastri", "data",
                             "water_data.csv")) %>% 
  ## Make day date and remove chlorophyll in trini
  dplyr::mutate(day = lubridate::dmy(day),
                chloro_ugL = ifelse(country == "trini",
                                    NA, chloro_ugL)) %>% 
  ## Remove tap
  dplyr::filter(bromeliad_id != "tap") %>% 
  ## Add decomposition values
  dplyr::left_join(readr::read_csv(here::here("brastri", "data",
                                              "bromeliad_data.csv")) %>% 
                     dplyr::select(country, bromeliad_id, contains(c("_dry", "_normal"))),
                   by = c("country", "bromeliad_id"))

## Aquatic communities
community <-
  readr::read_csv(here::here("brastri", "data",
                             "community_data.csv")) %>% 
  ## Remove ci columns
  dplyr::select(-contains("ci_"), -path,  -day,
                -abundance)
## Bromeliads
bromeliads <-
  readr::read_csv(here::here("brastri", "data",
                             "bromeliad_data.csv")) %>% 
  ## Keep experimental bromeliads only
  dplyr::filter(stringr::str_detect(string = bromeliad_id, 
                                    pattern = "E")) %>% 
  ## Remove the columns not needed
  dplyr::select(-contains(c("_g", "actual", "site", "mm")))

## Emergence data
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
                              day - 91, day -  280))

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
  dplyr::select(-when) %>%
  ## Make species name for plotting
  get_specnames() %>% 
  dplyr::mutate(species = ifelse(stringr::str_detect(string = species, pattern = "Tipu"),
                                 "Tipulidae", species),
                biomass_mg = as.numeric(biomass_mg)) %>% 
  dplyr::select(country:species) %>% 
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


# Models on total emerged biomass -----------------------------------------------
# Overall biomass
## Fit model
emergencemodel_all <-
  brms::brm(log(biomass_mg + 0.01) ~
              resource*predator + (1|bromspecies/country),
            iter = 5000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.982,
                           max_treedepth = 10),
            data = summarise_emergence(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "all") %>% 
              dplyr::filter(biomass_mg <10))
## Check assumptions
plot(emergencemodel_all)
## Check effects
bayestestR::describe_posterior(emergencemodel_all)
## Plot
figs3a <- 
  treatment_plot(model = emergencemodel_all, 
                 scale = "log",
                 parameter = "emergence_all", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = summarise_emergence(dats = emergence_selected,
                                                bromeliads = bromeliads,
                                                group = "all") %>% 
                                dplyr::mutate(biomass_mg = biomass_mg + 0.01)%>% 
                   dplyr::filter(biomass_mg <10))

# Seeded biomass
## Fit model
emergencemodel_seed <-
  brms::brm(log(biomass_mg + 0.01) ~
              resource*predator + (1|bromspecies/country),
            iter = 5000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 10),
            data = summarise_emergence(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "seed") %>% 
              dplyr::filter(biomass_mg <10))
## Check assumptions
plot(emergencemodel_seed)
## Check effects
bayestestR::describe_posterior(emergencemodel_seed)
## Plot
fig3a <- 
  treatment_plot(model = emergencemodel_seed, 
                 scale = "log",
                 parameter = "emergence_seed", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = summarise_emergence(dats = emergence_selected,
                                                 bromeliads = bromeliads,
                                                 group = "seed") %>% 
                   dplyr::mutate(biomass_mg = biomass_mg + 0.01)%>% 
                   dplyr::filter(biomass_mg <10))


# Chironomidae
## Fit model
emergencemodel_chiro <-
  brms::brm(log(biomass_mg + 0.01) ~
              resource*predator + (1|bromspecies/country),
            iter = 5000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.999,
                           max_treedepth = 10),
            data = summarise_emergence(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "Chiro"))
## Check assumptions
plot(emergencemodel_chiro)
## Check effects
bayestestR::describe_posterior(emergencemodel_chiro)
## Plot
fig3b <- 
  treatment_plot(model = emergencemodel_chiro, 
                 scale = "log",
                 parameter = "emergence_chir_tot", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = summarise_emergence(dats = emergence_selected,
                                                 bromeliads = bromeliads,
                                                 group = "Chiro") %>% 
                   dplyr::mutate(biomass_mg = biomass_mg + 0.01))

# Culicidae
## Fit model
emergencemodel_culi <-
  brms::brm(log(biomass_mg + 0.01) ~
              resource*predator,
            iter = 5000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.995,
                           max_treedepth = 10),
            data = summarise_emergence(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "Culi") %>% 
              dplyr::filter(country == "bras"))
## Check assumptions
plot(emergencemodel_culi)
## Check effects
bayestestR::describe_posterior(emergencemodel_culi)
## Plot
figs3b <- 
  treatment_plot(model = emergencemodel_culi, 
                 scale = "log",
                 parameter = "emergence_culi_tot", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = summarise_emergence(dats = emergence_selected,
                                                 bromeliads = bromeliads,
                                                 group = "Culi") %>% 
                   dplyr::filter(country == "bras") %>% 
                   dplyr::mutate(biomass_mg = biomass_mg + 0.01))

# Tipulidae
## Fit model
emergencemodel_tipu <-
  brms::brm(log(biomass_mg + 0.01) ~
              resource*predator + (1|bromspecies/country),
            iter = 5000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.995,
                           max_treedepth = 10),
            data = summarise_emergence(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "Tipu"))
## Check assumptions
plot(emergencemodel_tipu)
## Check effects
bayestestR::describe_posterior(emergencemodel_tipu)
## Plot
figs3c <- 
  treatment_plot(model = emergencemodel_tipu, 
                 scale = "log",
                 parameter = "emergence_tipu_tot", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = summarise_emergence(dats = emergence_selected,
                                                 bromeliads = bromeliads,
                                                 group = "Tipu") %>% 
                   dplyr::mutate(biomass_mg = biomass_mg + 0.01))

# Ceratopogonidae
## Fit model
emergencemodel_cera <-
  brms::brm(log(biomass_mg + 0.01) ~
              resource*predator + (1|bromspecies/country),
            iter = 5000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.999,
                           max_treedepth = 15),
            data = summarise_emergence(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "Cera"))
 ## Check assumptions
plot(emergencemodel_cera)
## Check effects
bayestestR::describe_posterior(emergencemodel_cera)
## Plot
figs3d <- 
  treatment_plot(model = emergencemodel_cera, 
                 scale = "log",
                 parameter = "emergence_cera_tot", 
                 bromeliads = bromeliads, 
                 communities = community, 
                 water = water, 
                 emergence = summarise_emergence(dats = emergence_selected,
                                                 bromeliads = bromeliads,
                                                 group = "Cera") %>% 
                   dplyr::mutate(biomass_mg = biomass_mg + 0.01))


# Table summarising results
## Make table
table_4 <- 
  data.frame(
    ` ` = c("<strong>Total emerged <br>biomass <br>ROPE = (-0.01, 0.01)</strong>", # total
            "<strong>Total biomass <br> from seeded groups <br>ROPE = (-0.01, 0.01)</strong>", # seed
            "<strong>Total chironomid <br>biomass <br>ROPE = (-0.01, 0.01)</strong>", # chiro
            "<strong>Total culicid <br>biomass <br>ROPE = (-0.01, 0.01)</strong>", # culi
            "<strong>Total tipulid <br>biomass <br>ROPE = (-0.01, 0.01)</strong>", # tipu
            "<strong>Total ceratopogonid <br>biomass <br>ROPE = (-0.01, 0.01)</strong>" # cerato
    ),
    Intercept = c("<strong>-2.01 (-4.18, 0.19) <br>pd = 96.54% <br>% in ROPE = 0.13</strong>", # total
                  "<strong>-2.15 (-4.40, -0.01) <br>pd = 97.54% <br>% in ROPE = 0.03</strong>", # seed
                  "<strong>-3.26 (-5.08, -1.44) <br>pd = 99.65% <br>% in ROPE = 0</strong>", # chiro
                  "<strong>-1.53 (-2.87, -0.22) <br>pd = 98.86% <br>% in ROPE = 0</strong>", # culi
                  "<strong>-3.60 (-5.69, -1.91) <br>pd = 99.89% <br>% in ROPE = 0</strong>", # tipu
                  "<strong>-4.25 (-5.91, -2.79) <br>pd = 99.99% <br>% in ROPE = 9.20</strong>" # cerato
    ),
    `Resource enriched` = c("-0.31 (-1.25, 0.59) <br>pd = 75.16% <br>% in ROPE = 1.39", # total
                            "-0.39 (-1.38, 0.62) <br>pd = 78.14% <br>% in ROPE = 0.97", # seed
                            "-0.20 (-0.85,  0.44) <br>pd = 72.81% <br>% in ROPE = 2.22", # chiro
                            "-0.60 (-2.45,  1.28) <br>pd = 74.28% <br>% in ROPE = 0.66", # culi
                            "-0.21 (-1.35,  0.94) <br>pd = 64.49% <br>% in ROPE = 1.46", # tipu
                            "-0.08 (-0.59,  0.44) <br>pd = 62.51% <br>% in ROPE = 3.51" # cerato
    ),
    `Predator present` = c("-0.14 (-1.62, 1.34) <br>pd = 58.16% <br>% in ROPE = 1.27", # total
                           "-0.35 (-1.91,  1.18) <br>pd = 67.03% <br>% in ROPE = 1.15", # seed
                           "<strong>-1.07 (-2.08, -0.05) <br>pd = 98.02% <br>% in ROPE = 0</strong>", # chiro
                           "-0.46 (-2.31,  1.45) <br>pd = 69.36% <br>% in ROPE = 0.77", # culi
                           "0.11 (-1.61,  1.83) <br>pd = 54.71% <br>% in ROPE = 0.80", # tipu
                           "-0.44 (-1.24,  0.37) <br>pd = 85.73% <br>% in ROPE = 1.27" # cerato
    ),
    `Resource enrichedxPredator present` = c("-0.02 (-1.90, 1.86) <br>pd = 50.80% <br>% in ROPE = 0.77", # total
                                             "0.17 (-1.79, 2.17) <br>pd = 56.99% <br>% in ROPE = 0.75", # seed
                                             "<strong>1.37 (0.10,  2.67) <br>pd = 98.38% <br>% in ROPE = 0</strong>",# chiro
                                             "0.31 (-2.38,  2.96) <br>pd = 59.52% <br>% in ROPE = 0.63",# culi
                                             "0.06 (-2.14,  2.31) <br>pd = 51.76% <br>% in ROPE = 0.72", # tipu
                                             "0.24 (-0.81,  1.26) <br>pd = 67.84% <br>% in ROPE = 1.62" # cerato
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
              resource*predator + (1|bromeliad_id/bromspecies/country),
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
fig3c <- 
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
fig3d <- 
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
              resource*predator + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.92,
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
fig3e <- 
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
              resource*predator + (1|bromeliad_id/bromspecies/country),
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
figs3e <- 
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
    ` ` = c("<strong>Individual chironomid <br>dry body mass <br>ROPE = (-0.004, 0.004)</strong>", # chiro
            "<strong>Individual culicid <br>dry body mass <br>ROPE = (-0.01, 0.01)</strong>", # culi
            "<strong>Individual dry <br>tipulid body mass <br>ROPE = (-0.04, 0.04)</strong>", # tipu
            "<strong>Individual dry <br>ceratopogonid body mass <br>ROPE = (-0.002, 0.002)</strong>" # cerato
    ),
    Intercept = c("<strong>0.06 (0.05, 0.08) <br>pd = 100% <br>% in ROPE = 0</strong>", # chiro
                  "<strong>0.25 (0.17, 0.33) <br>pd = 100% <br>% in ROPE = 0</strong>", # culi
                  "<strong>0.88 (0.50, 1.24) <br>pd = 100% <br>% in ROPE = 0</strong>", # tipu
                  "<strong>0.04 (0.01, 0.06) <br>pd = 99.83% <br>% in ROPE = 0</strong>" # cerato
    ),
    `Resource enriched` = c("-0.005 (-0.03, 0.02) <br>pd = 66.70% <br>% in ROPE = 25.18", # chiro
                            "-0.08 (-0.21, 0.04) <br>pd = 89.42% <br>% in ROPE = 8.50", # culi
                            "<strong>-0.47 (-1.02, 0.10) <br>pd = 95.43% <br>% in ROPE = 2.11</strong>", # tipu
                            "0.02 (-0.01, 0.06) <br>pd = 91.22% <br>% in ROPE = 4.66" # cerato

    ),
    `Predator present` = c("-0.02 (-0.05, 0.02) <br>pd = 83.17% <br>% in ROPE = 11.53", # chiro
                           "-0.08 (-0.20, 0.04) <br>pd = 90.70% <br>% in ROPE = 8.97", # culi
                           "-0.25 (-0.96, 0.50) <br>pd = 76.92% <br>% in ROPE = 7.05", # tipu
                           "-0.003 (-0.04, 0.03) <br>pd = 58.45% <br>% in ROPE = 11.61" # cerato
    ),
    `Resource enrichedxPredator present` = c("<strong>0.06 (0.01, 0.11) <br>pd = 99.17% <br>% in ROPE = 0</strong>",# chiro
                                             "<strong>0.17 (-0.02, 0.36) <br>pd = 96.28% <br>% in ROPE = 2.97</strong>",# culi
                                             "0.63 (-0.53, 1.74) <br>pd = 88.58% <br>% in ROPE = 3.13", # tipu
                                             "-0.02 (-0.08, 0.03) <br>pd = 81.95% <br>% in ROPE = 5.42" # cerato
    )
  )

## Save table
readr::write_csv(table_5,
                 here::here("brastri", "data",
                            "table_5.csv"))


# Emergence figures -------------------------------------------------------
# Figure of all results
# Generate figure
## Get legend
legend <- 
  cowplot::get_legend(fig3a)
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
                     fig3e +
                       theme(legend.position = "none") +
                       ggtitle("e"),
                     legend,
                     ncol = 2)
## Save figure
ggsave(here::here("brastri", "www",
                  "fig3.jpg"),
       fig3,
       width = 7,
       height = 11,
       bg = "white")


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
                     figs3e +
                       theme(legend.position = "none") +
                       ggtitle("e"),
                     legend,
                     ncol = 2)
## Save figure
ggsave(here::here("brastri", "www",
                  "figs3.jpg"),
       figs3,
       width = 7,
       height = 9,
       bg = "white")


# Models on growth rate ---------------------------------------------------
# Chironomidae
## Fit model
indgrowthmodel_chiro <-
  brms::brm(ndays~
              resource*predator + (1|bromeliad_id/bromspecies/country),
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
figs4a <- 
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
figs4b <- 
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
              resource*predator + (1|bromeliad_id/bromspecies/country),
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
figs4c <- 
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
              resource*predator + (1|bromeliad_id/bromspecies/country),
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
figs4d <- 
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
figs4 <- 
  cowplot::plot_grid(figs4a +
                       theme(legend.position = "none") +
                       ggtitle("a"),
                     figs4b +
                       theme(legend.position = "none") +
                       ggtitle("b"),
                     figs4c +
                       theme(legend.position = "none") +
                       ggtitle("c"),
                     figs4d +
                       theme(legend.position = "none") +
                       ggtitle("d"),
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
       width = 7,
       height = 9,
       bg = "white")


# Table summarising results
## Make table
table_6 <- 
  data.frame(
    ` ` = c("<strong>Number of days until <br> chironomid emergence <br>ROPE = (-0.74, 0.74)</strong>", # chiro
            "<strong>Number of days until <br> culicid emergence <br>ROPE = (-0.77, 0.77)</strong>", # culi
            "<strong>Number of days until <br> tipulid emergence <br>ROPE = (-0.77, 0.77)</strong>", # tipu
            "<strong>Number of days until <br> ceratopogonid emergence <br>ROPE = (-0.64, 0.64)</strong>" # cerato
    ),
    Intercept = c("<strong>16.60 (13.22, 19.91) <br>pd = 100% <br>% in ROPE = 0</strong>", # chiro
                  "<strong>13.21 (7.71, 18.61) <br>pd = 99.98% <br>% in ROPE = 0</strong>", # culi
                  "<strong>13.23 (6.50, 20.04) <br>pd = 99.96% <br>% in ROPE = 0</strong>", # tipu
                  "<strong>18.03 (12.85, 23.31]) <br>pd = 100% <br>% in ROPE = 0</strong>" # cerato
    ),
    `Resource enriched` = c("-0.69 (-5.61, 4.18) <br>pd = 61.20% <br>% in ROPE = 24.50", # chiro
                            "-0.38 (-8.64, 7.86) <br>pd = 53.47% <br>% in ROPE = 15.34", # culi
                            "0.75 (-9.32, 10.73) <br>pd = 55.85% <br>% in ROPE = 12.61", # tipu
                            "-0.75 (-8.63,  7.41) <br>pd = 57.10% <br>% in ROPE = 12.95" # cerato
                            
    ),
    `Predator present` = c("-3.45 (-10.24,  3.18) <br>pd = 85.78% <br>% in ROPE = 24.50", # chiro
                           "4.10 (-3.70, 12.49) <br>pd = 85.15% <br>% in ROPE = 9.11", # culi
                           "5.88 (-8.17, 20.20) <br>pd = 81.11% <br>% in ROPE = 6.26", # tipu
                           "0.09 (-8.10,  8.77) <br>pd = 50.95% <br>% in ROPE = 12.87" # cerato
    ),
    `Resource enrichedxPredator present` = c("6.34 (-2.63, 15.55) <br>pd = 91.72% <br>% in ROPE = 4.92",# chiro
                                             "-3.71 (-16.51,  8.13) <br>pd = 73.80% <br>% in ROPE = 9.53",# culi
                                             "-2.32 (-23.70, 17.84) <br>pd = 59.19% <br>% in ROPE = 6.68", # tipu
                                             "-1.70 (-13.77,  9.97) <br>pd = 62.08% <br>% in ROPE = 9.61" # cerato
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
  brms::brm(log(prop + 0.01) ~
              resource*predator + (1|bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.96,
                           max_treedepth = 10),
            data = summarise_proportion(dats = emergence_selected,
                                       bromeliads = bromeliads,
                                       group = "seed"))
## Check assumptions
plot(propemergencemodel_seed)
## Check effects
bayestestR::describe_posterior(propemergencemodel_seed)
## Plot
figs5a <- 
  treatment_plot(model = propemergencemodel_seed, 
                 scale = "log",
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
  brms::brm(log(prop + 0.01) ~
              resource*predator ,
            iter = 2000,
            family = gaussian(link = "identity"),    
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
figs5b <- 
  treatment_plot(model = propemergencemodel_culi, 
                 scale = "log",
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
  brms::brm(log(prop + 0.01) ~
              resource*predator + (1|bromspecies/country),
            iter = 5000,
            family = gaussian(link = "identity"),    
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
figs5c <- 
  treatment_plot(model = propemergencemodel_tipu, 
                 scale = "log",
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
                     legend,
                     ncol = 2)

## Save figure
ggsave(here::here("brastri", "www",
                  "figs5.jpg"),
       figs5,
       width = 7,
       height = 7,
       bg = "white")

# Table summarising results
## Make table
table_7 <- 
  data.frame(
    ` ` = c("<strong>Proportion of seeded <br>larvae emerging <br>ROPE = (-0.01, 0.01)</strong>", # seed
            "<strong>Proportion of culicids <br>emerging <br>ROPE = (-0.01, 0.01)</strong>", # culi
            "<strong>Proportion of tipulids <br>emerging <br>ROPE = (-0.01, 0.01)</strong>" # tipu

            
    ),
    Intercept = c("<strong>-2.58 (-4.42, -0.76) <br>pd = 99.65% <br>% in ROPE = 0</strong>", # seed
                  "<strong>-1.94 (-3.12, -0.74) <br>pd = 99.88% <br>% in ROPE = 0</strong>", # culi
                  "<strong>-3.82 (-5.65, -2.30) <br>pd = 99.88% <br>% in ROPE = 0</strong>" # tipu
                
                  
    ),
    `Resource enriched` = c("-0.04 (-0.79, 0.75) <br>pd = 54.30% <br>% in ROPE = 2.05", # seed
                            "-0.33 (-2.04, 1.28) <br>pd = 65.00% <br>% in ROPE = 0.84", # culi
                            "0.07 (-0.94, 1.08) <br>pd = 55.99% <br>% in ROPE = 1.82" # tipu
                            
    ),
    `Predator present` = c("-0.38 (-1.57, 0.80) <br>pd = 73.88% <br>% in ROPE = 1.08", # seed
                           "-0.16 (-1.84, 1.54) <br>pd = 57.63% <br>% in ROPE = 0.79", # culi
                           "0.12 (-1.45, 1.68) <br>pd = 56.11% <br>% in ROPE = 1.06" # tipu
                           
    ),
    `Resource enrichedxPredator present` = c("0.01 (-1.50, 1.45) <br>pd = 50.58% <br>% in ROPE = 0.95", # seed
                                             "-0.16 (-2.51, 2.31) <br>pd = 55.55% <br>% in ROPE = 0.61", # culi
                                             "-0.20 (-2.21, 1.82) <br>pd = 58.58% <br>% in ROPE = 0.93"# tipu
                                            
                                             
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
  brms::brm(log(biomass_mg + 0.1) ~
              resource*predator + (1|bromspecies/country),
            iter = 5000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 10),
            data = summarise_leftover(dats = community_end,
                                       bromeliads = bromeliads,
                                       group = "all"))
## Check assumptions
plot(leftovermodel_all)
## Check effects
bayestestR::describe_posterior(leftovermodel_all)
## Plot
figs6a <- 
  treatment_plot(model = leftovermodel_all, 
                 scale = "log",
                 parameter = "leftover_all", 
                 bromeliads = bromeliads, 
                 communities = summarise_leftover(dats = community_end,
                                                   bromeliads = bromeliads,
                                                   group = "all") %>% 
                   dplyr::mutate(biomass_mg = biomass_mg + 0.01), 
                 water = water, 
                 emergence = emergence)

# Seeded biomass
## Fit model
leftovermodel_seed <-
  brms::brm(log(biomass_mg + 0.01)~
              resource*predator + (1|bromspecies/country),
            iter = 5000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 10),
            data = summarise_leftover(dats = community_end,
                                       bromeliads = bromeliads,
                                       group = "seed"))
## Check assumptions
plot(leftovermodel_seed)
## Check effects
bayestestR::describe_posterior(leftovermodel_seed)
## Plot
fig3a <- 
  treatment_plot(model = leftovermodel_seed, 
                 scale = "log",
                 parameter = "leftover_seed", 
                 bromeliads = bromeliads, 
                 communities = summarise_leftover(dats = community_end,
                                                  bromeliads = bromeliads,
                                                  group = "seed") %>% 
                   dplyr::mutate(biomass_mg = biomass_mg + 0.01), 
                 water = water, 
                 emergence = emergence)


# Chironomidae
## Fit model
leftovermodel_chiro <-
  brms::brm(log(biomass_mg + 0.01) ~
              resource*predator + (1|bromspecies/country),
            iter = 5000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.999,
                           max_treedepth = 20),
            data = summarise_leftover(dats = community_end,
                                       bromeliads = bromeliads,
                                       group = "Poly"))
## Check assumptions
plot(leftovermodel_chiro)
## Check effects
bayestestR::describe_posterior(leftovermodel_chiro)
## Plot
fig3b <- 
  treatment_plot(model = leftovermodel_chiro, 
                 scale = "none",
                 parameter = "leftover_chir_tot", 
                 bromeliads = bromeliads, 
                 communities = summarise_leftover(dats = community_end,
                                                  bromeliads = bromeliads,
                                                  group = "Chiro"), 
                 water = water, 
                 emergence = emergence)

# Culicidae
## Fit model
leftovermodel_culi <-
  brms::brm(log(biomass_mg + 0.01) ~
              resource*predator,
            iter = 5000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.99,
                           max_treedepth = 10),
            data = summarise_leftover(dats = community_end,
                                       bromeliads = bromeliads,
                                       group = "Culi") %>% 
              dplyr::filter(country == "bras"))
## Check assumptions
plot(leftovermodel_culi)
## Check effects
bayestestR::describe_posterior(leftovermodel_culi)
## Plot
figs6b <- 
  treatment_plot(model = leftovermodel_culi, 
                 scale = "log",
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
  brms::brm(log(biomass_mg + 0.01) ~
              resource*predator + (1|bromspecies/country),
            iter = 5000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.995,
                           max_treedepth = 10),
            data = summarise_leftover(dats = community_end,
                                       bromeliads = bromeliads,
                                       group = "Tipu"))
## Check assumptions
plot(leftovermodel_tipu)
## Check effects
bayestestR::describe_posterior(leftovermodel_tipu)
## Plot
figs6c <- 
  treatment_plot(model = leftovermodel_tipu, 
                 scale = "log",
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
  brms::brm(log(biomass_mg + 0.01) ~
              resource + (1|bromspecies),
            iter = 5000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.999,
                           max_treedepth = 15),
            data = summarise_leftover(dats = community_end,
                                       bromeliads = bromeliads,
                                       group = "Scir") %>% 
              dplyr::filter(country == "trini"))
## Check assumptions
plot(leftovermodel_scir)
## Check effects
bayestestR::describe_posterior(leftovermodel_scir)
## Prepare data
model_effect <- 
  brms::conditional_effects(leftovermodel_scir,
                            method = "fitted")$resource %>% 
  dplyr::mutate(estimate__ = exp(estimate__),
                lower__ = exp(lower__),
                upper__ = exp(upper__))
## Plot
figs6d <- 
  ggplot(data = summarise_leftover(dats = community_end,
                                   bromeliads = bromeliads,
                                   group = "Scir") %>% 
           dplyr::filter(country == "trini"),
         aes(x = resource,
             y = biomass_mg,
             colour = resource)) + 
  geom_jitter(aes(alpha = 0.3)) +
  geom_point(size = 3,
             data = model_effect,
             aes(x = resource, 
                 y = estimate__,
                 colour = resource), 
             position = position_dodge(0.5)) +
  geom_errorbar(data = model_effect,
                aes(ymin = lower__, 
                    ymax = upper__,
                    colour = resource), 
                width = 0.2,
                position = position_dodge(0.5)) +
  
  ggtitle("") +
  xlab("Resource") +
  scale_x_discrete(labels = c("Enriched", "Control")) +
  ylab(axis_label(parameter = "leftover_scir_tot")) +
  scale_color_manual(name = "Resource",
                     labels = c("Control", "Enriched"), 
                     values = c("tan1", "tan4")) +
  guides(alpha = "none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Table summarising results
## Make table
table_8 <- 
  data.frame(
    ` ` = c("<strong>Total leftover <br>biomass <br>ROPE = (-0.01, 0.01)</strong>", # total
            "<strong>Total leftover biomass <br> from seeded groups <br>ROPE = (-0.01, 0.01)</strong>", # seed
            "<strong>Total leftover <br>chironomid biomass <br>ROPE = (-0.01, 0.01)</strong>", # chiro
            "<strong>Total leftover <br>culicid biomass <br>ROPE = (-0.01, 0.01)</strong>", # culi
            "<strong>Total leftover <br>tipulid biomass <br>ROPE = (-0.01, 0.01)</strong>", # tipu
            "<strong>Total leftover <br>scirtid biomass <br>ROPE = (-0.01, 0.01)</strong>" # scir
            
    ),
    Intercept = c("0.48 (-1.22, 2.16) <br>pd = 77.34% <br>% in ROPE = 1.05", # total
                  "-0.88 (-2.95, 1.38) <br>pd = 81.88% <br>% in ROPE = 0.52", # seed
                  "0.23 (--0.67,  1.12) <br>pd = 82.90% <br>% in ROPE = 2.93", # chiro
                  "<strong>-2.89 (-4.06, -1.78) <br>pd = 100% <br>% in ROPE = 0</strong>", # culi
                  "<strong>-4.35(-5.84, -3.03) <br>pd = 100% <br>% in ROPE = 0</strong>", # tipu
                  "-0.38 (-1.97, 1.39) <br>pd = 71.78% <br>% in ROPE = 1.06" # scir
                  
    ),
    `Resource enriched` = c("-0.14 (-0.93, 0.64) <br>pd = 64.42% <br>% in ROPE = 2.01", # total
                            "-0.58 (-1.78, 0.64) <br>pd = 83.13% <br>% in ROPE = 0.82", # seed
                            "<strong>-0.13 (0.22, -0.04) <br>pd = 99.59% <br>% in ROPE = 0</strong>", # chiro
                            "-0.40 (1.97, 1.17) <br>pd = 70.12% <br>% in ROPE = 0.80", # culi
                            "-0.13 (-0.82, 0.58) <br>pd = 63.62% <br>% in ROPE = 1.95", # tipu
                            "-0.68 (-2.06, 0.69) <br>pd = 84.51% <br>% in ROPE = 0.89" # scir
                            
    ),
    `Predator present` = c("0.10 (-1.13, 1.33) <br>pd = 55.88% <br>% in ROPE = 1.47", # total
                           "1.44 (-0.44, 3.33) <br>pd = 93.44% <br>% in ROPE = 0.22", # seed
                           "-0.07 (-0.22, 0.08) <br>pd = 81.60% <br>% in ROPE = 13.87", # chiro
                           "-1.24 (-2.86, 0.33) <br>pd = 94.03% <br>% in ROPE = 0.32", # culi
                           "0.52 (-0.56, 1.61) <br>pd = 82.61% <br>% in ROPE = 1.02", # tipu
                           "" # scir
                           
                           
    ),
    `Resource enrichedxPredator present` = c("0.04 (-1.52, 1.58) <br>pd = 52.03% <br>% in ROPE = 1.02", # total
                                             "-0.006 (-2.39, 2.32) <br>pd = 50.27% <br>% in ROPE = 0.78", # seed
                                             "0.13 (-0.06, 0.31) <br>pd = 91.45% <br>% in ROPE = 6.29",# chiro
                                             "0.26 (-2.01, 2.52) <br>pd = 59.74% <br>% in ROPE = 0.72",# culi
                                             "1.01 (-0.37,  2.42) <br>pd = 92.37% <br>% in ROPE = 0.45", # tipu
                                             "" # scir
                                             
    )
  )

## Save table
readr::write_csv(table_8,
                 here::here("brastri", "data",
                            "table_4.csv"))


# Models on individual emerged body mass ----------------------------------
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
            control = list(adapt_delta = 0.92,
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
              resource*predator + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),      
            control = list(adapt_delta = 0.92,
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
figs6e <- 
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
## Prepare data
model_effect <- 
  brms::conditional_effects(indleftovermodel_scir,
                            method = "fitted")$resource
## Plot
fig3d <- 
  ggplot(data = community_end %>% 
           dplyr::mutate(biomass_mg = as.numeric(biomass_mg)) %>% 
           dplyr::filter(stringr::str_detect(string = genus,
                                             pattern = "Scir")),
         aes(x = resource,
             y = biomass_mg,
             colour = resource)) + 
  geom_jitter(aes(alpha = 0.3)) +
  geom_point(size = 3,
             data = model_effect,
             aes(x = resource, 
                 y = estimate__,
                 colour = resource), 
             position = position_dodge(0.5)) +
  geom_errorbar(data = model_effect,
                aes(ymin = lower__, 
                    ymax = upper__,
                    colour = resource), 
                width = 0.2,
                position = position_dodge(0.5)) +
  
  ggtitle("") +
  xlab("Resource") +
  scale_x_discrete(labels = c("Enriched", "Control")) +
  ylab(axis_label(parameter = "leftover_scir_tot")) +
  scale_color_manual(name = "Resource",
                     labels = c("Control", "Enriched"), 
                     values = c("tan1", "tan4")) +
  guides(alpha = "none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Table summarising results
## Make table
table_9 <- 
  data.frame(
    ` ` = c("<strong>Individual culicid <br>dry body mass <br>ROPE = (-0.01, 0.01)</strong>", # culi
            "<strong>Individual dry <br>tipulid body mass <br>ROPE = (-0.01, 0.01)</strong>", # tipu
            "<strong>Individual dry <br>scirtid body mass <br>ROPE = (-0.002, 0.002)</strong>" # scir
    ),
    Intercept = c("<strong>0.15 (0.01, 0.27) <br>pd = 97.87% <br>% in ROPE = 0.28</strong>", # culi
                  "-1.09 (-3.77, 1.55) <br>pd = 81.77% <br>% in ROPE = 0.18", # tipu
                  "<strong>0.19 (0.11, 0.27) <br>pd = 100% <br>% in ROPE = 0</strong>" # scir
    ),
    `Resource enriched` = c("<strong>0.19 (-0.02, 0.40) <br>pd = 96.79% <br>% in ROPE = 1.44</strong>", # culi
                            "0.73 (-3.95, 5.40) <br>pd = 63.48% <br>% in ROPE = 0.45", # tipu
                            "<strong>0.12 (0.00, 0.24) <br>pd = 97.15% <br>% in ROPE = 2.18</strong>" # scir
                            
    ),
    `Predator present` = c("<strong>0.28 (-0.04, 0.61) <br>pd = 96.43% <br>% in ROPE = 1.04</strong>", # culi
                           "-1.20 (-4.74, 2.08) <br>pd = 79.65% <br>% in ROPE = 0.37", # tipu
                           "" # scir
    ),
    `Resource enrichedxPredator present` = c("<strong>-0.49 (-0.98, -0.03) <br>pd = 98.06% <br>% in ROPE = 0</strong>",# culi
                                             "0.01 (-5.15, 5.46) <br>pd = 50.35% <br>% in ROPE = 0.45", # tipu
                                             "" # scir
    )
  )

## Save table
readr::write_csv(table_9,
                 here::here("brastri", "data",
                            "table_5.csv"))


# Emergence figures -------------------------------------------------------
# Figure of all results
# Generate figure
## Get legend
legend <- 
  cowplot::get_legend(fig3a)
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
fig3 <- 
  cowplot::plot_grid(legend,
                     fig3,
                     nrow = 2,
                     rel_heights = c(0.1, 0.9))

## Save figure
ggsave(here::here("brastri", "www",
                  "fig3.jpg"),
       fig3,
       width = 7,
       height = 9,
       bg = "white")



# Generate figure
## Make figure
figs6 <- 
  cowplot::plot_grid(figs6a +
                       theme(legend.position = "none") +
                       ggtitle("a"),
                     figs6b +
                       theme(legend.position = "none") +
                       ggtitle("b"),
                     figs6c +
                       theme(legend.position = "none") +
                       ggtitle("c"),
                     figs6d +
                       theme(legend.position = "none") +
                       ggtitle("d"),
                     figs6e +
                       theme(legend.position = "none") +
                       ggtitle("e"),
                     legend,
                     ncol = 2)
## Save figure
ggsave(here::here("brastri", "www",
                  "figs6.jpg"),
       figs6,
       width = 7,
       height = 9,
       bg = "white")


