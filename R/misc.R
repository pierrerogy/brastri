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
## Bromeliads
bromeliads <-
  readr::read_csv(here::here("brastri", "data",
                             "bromeliad_data.csv")) %>% 
  ## Keep experimental bromeliads only
  dplyr::filter(stringr::str_detect(string = bromeliad_id, patter = "E")) %>% 
  ## Remove the columns not needed
  dplyr::select(-contains(c("_g", "actual", "site", "mm")))

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
                     dplyr::select(country, bromeliad_id, contains("_dry")),
                   by = c("country", "bromeliad_id"))


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




# Treatments on water chemistry variables ---------------------------------
# Temperature
## Fit model
tempmodel <-
  brms::brm(log(temp_C)  ~
              resource*predator + (1|day) + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(tempmodel)
## Check effects
bayestestR::describe_posterior(tempmodel)
## Plot
fig2a <- 
  treatment_plot(model = tempmodel, 
                 parameter = "Temperature",  
                 scale = "log",
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence)

# pH
## Fit model
pHmodel <-
  brms::brm(log(pH)  ~
              resource*predator + (1|day) + (1|bromeliad_id/bromspecies/country),
            iter = 4000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.85,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(pHmodel)
## Check effects
bayestestR::describe_posterior(pHmodel)
## Plot
fig2b <- 
  treatment_plot(model = pHmodel, 
                 parameter = "pH", 
                 scale = "log",
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence)

# Conductivity
## Fit model
condmodel <-
  brms::brm(sqrt(conductivity_uScm)  ~
              resource*predator + (1|day) + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.9,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(condmodel)
## Check effects
bayestestR::describe_posterior(condmodel)
## Plot
fig2c <- 
  treatment_plot(model = condmodel, 
                 parameter = "Conductivity", 
                 scale = "sqrt", 
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence)

# Total P
## Fit model
tpmodel <-
  brms::brm(log(total_p_ugL)  ~
              resource*predator + (1|day) + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.88,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(tpmodel)
## Check effects
bayestestR::describe_posterior(tpmodel)
## Plot
fig2d <- 
  treatment_plot(model = tpmodel, 
                 parameter = "TP", 
                 scale = "log", 
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence)

# Phosphate ppm
## Fit model
pppmmodel <-
  brms::brm(log(phosphate_ppm + 0.001)  ~
              resource + (1|day) + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.85,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(pppmmodel)
## Check effects
bayestestR::describe_posterior(pppmmodel)
## Plot
# Prepare data
## Effect
model_effect <- 
  brms::conditional_effects(pppmmodel,
                            method = "fitted")$resource %>% 
  dplyr::mutate(estimate__ = exp(estimate__),
                lower__ = exp(lower__),
                upper__ = exp(upper__))
## Plot
fig2e <- 
  ggplot(data = water,
         aes(x = resource,
             y = phosphate_ppm + 0.0001,
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
  scale_y_continuous(trans = "log",
                     breaks = c(0.01, 1, 5, 10)) +
  ylab(axis_label(parameter = "PPPM")) +
  scale_color_manual(name = "Resource",
                     labels = c("Control", "Enriched"), 
                     values = c("tan1", "tan4")) +
  guides(alpha = "none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "black"))

# Chlorophyll
## Fit model
chloromodel <-
  brms::brm(log(chloro_ugL + 0.001)  ~
              resource*predator + (1|day) + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.95,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(chloromodel)
## Check effects
bayestestR::describe_posterior(chloromodel)
## Plot
fig2f <- 
  treatment_plot(model = chloromodel, 
                 parameter = "Chlorophyll-a", 
                 scale = "log", 
                 bromeliads = bromeliads, 
                 communities = communities, 
                 water = water, 
                 emergence = emergence)


# Generate table with outputs
## Make table
table_1 <- 
  data.frame(
    ` ` = c("<strong>Temperature <br>ROPE =(-0.01, 0.01)</strong>", # temperature
           "<strong>pH <br>ROPE =(-0.01, 0.01)</strong>", # pH
           "<strong>Conductivity <br>ROPE =(-2.52, 2.52)</strong>", # cond
           "<strong>Total P <br>ROPE =(-0.01, 0.01)</strong>", # TP
           "<strong>Phosphate <br>ROPE =(-0.22, 0.22)</strong>", # PPPM
           "<strong>Chlorophyll-a <br>ROPE =(-0.01, 0.01)</strong>" # Chloro
    ),
    Intercept = c("<strong>3.22 (3.17, 3.28) <br>pd = 100% <br>% in ROPE = 0</strong>", # temperature
                  "<strong>1.78 (1.76, 1.81) <br>pd = 100% <br>% in ROPE = 0</strong>", # pH
                  "<strong>4.50 (3.05, 5.84) <br>pd = 100% <br>% in ROPE = 0</strong>", # cond
                  "<strong>4.70  (4.28,  5.14) <br>pd = 100% <br>% in ROPE = 0</strong>", # TP
                  "<strong> -3.52 (-5.52, -1.47) <br>pd = 95.40% <br>% in ROPE = 0.18</strong>", # PPPM
                  "<strong>-6.17 (-7.35, -5.09) <br>pd = 100% <br>% in ROPE = 0</strong>" # Chloro
    ),
    Resource = c("<0.001 (-0.01, 0.01) <br>pd = 56.05% <br>% in ROPE = 94.34", # temperature
                 "-0.03 (-0.06, 0.00) <br>pd = 96.67% <br>% in ROPE = 10.7", # pH
                 "1.81 (0.89, 2.83) <br>pd = 99.9% <br>% in ROPE = 93.68", # cond
                 "-0.31 (-0.80,  0.18) <br>pd = 89.38% <br>% in ROPE =1.61", # TP
                 "<strong>1.82 (0.39,  3.32) <br>pd = 99.35% <br>% in ROPE = 0</strong>", # PPPM
                 "-0.18 (-1.22,  0.84) <br>pd = 64.40% <br>% in ROPE = 1.39" # Chloro
    ),
    Predator = c("-0.002(-0.02, 0.02) <br>pd = 51.58% <br>% in ROPE = 75.92", # temperature
                 "0.006 (-0.02, 0.01) <br>pd = 63.1% <br>% in ROPE = 38.74", # pH
                 "-0.44 (-1.40, 0.56) <br>pd = 82.03% <br>% in ROPE = 100", # cond
                 "<strong>-0.48  (-0.98,  0.00) <br>pd = 97.60% <br>% in ROPE = 0.11</strong>", # TP
                 "", # PPPM
                 "0.21  (-0.79,  1.21) <br>pd = 66.95% <br>% in ROPE = 1.58" # Chloro
    ),
    ResourcexPredator = c("0.003 (-0.02, 0.03) <br>pd = 60.45% <br>% in ROPE = 63.16", # temperature
                              "0.01 (-0.05, 0.07) <br>pd = 65.71% <br>% in ROPE = 26.21", # pH
                              "-0.31 (-1.72, 1.01) <br>pd = 69.2% <br>% in ROPE = 100",# cond
                              "0.05 (-0.65,  0.75) <br>pd = 55.97% <br>% in ROPE = 3",# TP
                              "", # PPPM
                              "1.04 (-0.37,  2.48]) <br>pd = 92.80% <br>% in ROPE = 0.32" # Chloro
    )
  )
## Save table
readr::write_csv(table_1,
                 here::here("brastri", "data",
                            "table_1.csv"))


# Generate figure
## Make figure
fig2 <- 
  cowplot::plot_grid(fig2a +
                       theme(legend.position = "none") +
                       ggtitle("a"),
                     fig2b +
                       theme(legend.position = "none") +
                       ggtitle("b"),
                     fig2c +
                       theme(legend.position = "none") +
                       ggtitle("c"),
                     fig2d +
                       theme(legend.position = "none") +
                       ggtitle("d"),
                     fig2e +
                       theme(legend.position = "none") +
                       ggtitle("e"),
                     fig2f +
                       theme(legend.position = "none") +
                       ggtitle("f"),
                     ncol = 2)
## Combine with legend
legend <- 
  cowplot::get_legend(fig2a)
fig2 <- 
  cowplot::plot_grid(legend,
                     fig2,
                     nrow = 2,
                     rel_heights = c(0.1, 0.9))
## Save figure
ggsave(here::here("brastri", "www",
                  "fig2.jpg"),
       fig2,
       width = 7,
       height = 11,
       bg = "white")


# Treatments on decomposition and biomass ---------------------------------
# Decomposition coarse
## Fit model
coarsemodel <-
  brms::brm(log(prop_loss_coarse_dry)  ~
              resource*predator + (1|day) + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.95,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(coarsemodel)
## Check effects
bayestestR::describe_posterior(coarsemodel)

# Decomposition fine
## Fit model
finemodel <-
  brms::brm(log(prop_loss_fine_dry)  ~
              resource*predator + (1|day) + (1|bromeliad_id/bromspecies/country),
            iter = 2000,
            family = gaussian(link = "identity"),    
            control = list(adapt_delta = 0.95,
                           max_treedepth = 10),
            data = water)
## Check assumptions
plot(finemodel)
## Check effects
bayestestR::describe_posterior(finemodel)

