###############################################################################
#
# Goal: Estimate gmin values following RWC idea
#   1. merge info about part_Plant, moisture, RWC
#   2. Calculate water loss, fit model, estimate gmin



# Script: Sharath Paligi (sharath.paligi@uni-goettingen.de) 
#      

###############################################################################

# 1. Preparations -------------------------------------------------------------

# load libraries 
pacman::p_load(tidyverse, broom, gtools, conflicted)
conflict_prefer("lag", "dplyr")
conflict_prefer("filter", "dplyr")


# 2.1 set constants we will use in this script ---------------------------
# measurements were performed under controlled conditions, this will make our calculations easier
eu_cons <- 2.71828 # euler's constant 
rh = 50            # rh was set to 50 %
atm_p <- 101.4     # constant atm_p

# 2.2 add RWC values at TLP to the gmin data ------------------------------


# 2.3 load data sets ------------------------
# load raw gmin data
gmin_data2 <- read_csv("outputs/gmin_data2.csv") %>% 
# we want to save some info about the part_plant, moisture, and plant_num
  select(id, temp, treatment, dry_weight, leaf_area) %>% 
  distinct_all() 


# load interpolated mass and time values
gmin_mass_interpolated <- read_csv("outputs/gmin_mass_interpolated.csv") %>% 
  select(!c(newtime,  rwc)) %>% 
  distinct_all()

# 3.1 calculate gmin, RWC and stuff ----------------------------------------------
gmin_calculation <- gmin_mass_interpolated %>% 
  # merge interpolated values and the basic info we need for modelling
  left_join(gmin_data2) %>% 
  group_by(id) %>% 
  distinct_all() %>% 
  mutate(
    # Saturated vapor pressure in kpa (kilo pascals)
    VPsat = 610.78*eu_cons^(temp/(temp+238.3)*17.2694)/1000,
    # calculation of VPD
    mfVPD = (1-(rh/100))*VPsat/atm_p,
    # time difference between measurements
    time_dif = time_min - lag(time_min),
    # difference in leaf weights
    leaf_mass_dif = leaf_mass_new - lag(leaf_mass_new),
    # below the leaf area needs to be changed these are just the preliminary values
    gmin = -(leaf_mass_dif/18*1000)/(time_dif*60)/mfVPD/(leaf_area*2/10000),
    rwc = 100*((leaf_mass_new - dry_weight)/((max(leaf_mass_new)) - dry_weight)),
    rwd = 1 - ((leaf_mass_new - dry_weight)/((max(leaf_mass_new)) - dry_weight)),
    si = 1:n(),
    species = str_sub(id, 1, 4)
  ) %>% 
  select(!c(VPsat, mfVPD, leaf_mass_dif, time_dif)) 

unique(gmin_calculation$id)


# create conditions to model the inclusion and exclusion of the values
gmin_mod_prep <- gmin_calculation %>% 
  # classify criterias that we are interested in and exclude the rest from the model
  mutate(mod_include = case_when(c(rwc > 90 | rwc < 70) ~ "n", T ~ "t")
         ) %>%
  relocate(id, si, species, dry_weight, rwc, gmin, mod_include)



# how many points do we have according to our current classification for the linear model
(counts <- gmin_mod_prep %>% 
  group_by(id, treatment, species, mod_include) %>% 
  filter(mod_include == "t") %>% 
  summarise(mod_include = n()) %>% 
  arrange(mod_include)
)

# make a rough estimate of the values
gmin_rough_est <- gmin_mod_prep %>%
  group_by(id, treatment, species) %>%
  filter(mod_include == "t") %>%
  summarise(gmin = mean(gmin, na.rm = T))

# summarise Gmin data
gmin_rough_est %>%
  ggplot(aes(species, gmin, col = treatment, fill = treatment)) +
  geom_boxplot(alpha = 0.2) +
  geom_jitter( alpha = 0.5,
              position = position_dodge(preserve = c("total", "single"), width = 0.75)) +
  scale_fill_viridis_d(aesthetics = c("fill", "col"),
                       option = "D",
                        name = "Treatment") +
  labs(x = "Species",
       y = expression(paste(G[min], ~"(mmol", ~m^-2, ~s^-1, ")" ))) +
  theme_bw(base_size = 10) +
  facet_wrap(~species, scales = "free") +
  theme(legend.text = element_text(size = 10),
        axis.title  = element_text(size = 14),
        strip.text =  element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = c(0.1, 0.91)) 


unique(gmin_sps_temp$treatment)

gmin_sps_temp <- gmin_rough_est %>% 
  ungroup() %>%  
  mutate(species = fct_relevel(species, c("TICO", "ACPL",
                                          "FREX", "ROPS")),
         treatment = fct_relevel(treatment, c("T35", "T25")))


gmin_mod <- glm(gmin ~ species * treatment, data = gmin_sps_temp)

emmeans::emmeans(gmin_mod, "species")


summary(gmin_mod, )


