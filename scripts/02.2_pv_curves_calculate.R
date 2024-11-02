
# 1. Preparations -------------------------------------------------------------
# install.packages("pacman")
pacman::p_load(tidyverse, readxl, readxl, janitor, mgsub)

# load cleaned data -------------------------------------------------------
pv_data_clean <- read_csv("data/pv_data_clean.csv") %>% 
  mutate(window_new = 6,
         window =  5) %>% 
  mutate(factors = gsub("/data/pv-curve-h//",
                        "", files),
         factors = mgsub::mgsub(factors, c("pv_curve_", ".xlsx", "Campaign/"),
                                c("", "", ""))) %>% 
  separate(factors, c("campaign", "species", "site", "date"), sep = "_")


unique(pv_data_clean$files)

# 2. Read merged PV curve datasets, dry weights ---------------------------------------------------

# here the idea is after checking the points we are interested in we re-fit the model with the desired points 
# to have a start and end points and to calculate all the pv-curve parameters
# we merge pv-window new - after double-checking the points
# and pv_window old

pv_data <- pv_data_clean %>% 
  rename(weight_initial = initial_weight, 
         weight_final = end_weight, psi_initial = initial_psi) %>% 
  group_by(ID) %>% 
  select(ID, campaign, species, site, date, weight_initial, weight_final, dry_weight, window_new, window,
         psi_initial, e_cup_initial, e_cup_final, psi, p_inv) %>% 
  # recalculate p_inv, mass_diff and cum_diff as the ones from excel are broken or are not very accurate
  mutate(psi_inv = p_inv,
         psi = -psi/10,
         mass_diff = (e_cup_final - e_cup_initial),
         mass_cum = cumsum(mass_diff),
         leaf_water = weight_initial - mass_cum) %>% 
  filter(!is.na(psi)) %>% 
  unite(ID, c("campaign", "ID"), remove = F, sep = "_")


# have to do this once in the initital stage to quickly get all the sample ID's
# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# pv_data %>%
#   select(ID, campaign, species, site, date) %>%
#   distinct_all() %>%
#   mutate(window_update = "") %>%
#   write_csv("outputs/pv_window.csv")

# check the points on which the linear model has been fit to estimate the PV parameters
pv_data %>%
  filter(species == "SPRUCE" ) %>% 
  # exclude Leinfelde
  #filter(!site == "H") %>% 
 # inside filter to filter after campaign: & campaign == '3rd' 
  ggplot(aes(y = psi_inv, x = mass_cum, group = interaction(ID))) +
  geom_point(col = "#58508d", shape = 21,
             alpha = .6, pch = 20) +
  geom_path(col = "#58508d") +
  facet_wrap(~ID, scales = "free") +
  theme_bw()

# rerun after updating window
pv_window <- read_xlsx("outputs/pv_window_update.xlsx") %>% 
  select(!date)

# we will use the equation from linear model to extrapolate the slope and intercept of the SWC
pv_parameters_calculate1 <- pv_data %>% 
  left_join(pv_window) %>% 
 # filter(ID == "1st_FASY_01") %>% 
  group_by(ID) %>% 
  mutate(SI = 1:n(),
         SI_max = n(),
         # why is this?
         start = max(SI) - (window_update - 1),
         # this is the point used to start fitting the model for tlp values
         tlp_curve = 1:n() >=start,
         tlp_point = start==SI,
         # categorise the TLP_before and TLP_after point in the curve
         curve_classification = case_when(tlp_curve == F ~ "TLP_before", T ~ "TLP_after"),
         swc_curve = 1:n() <=start
  ) %>% 
  ungroup() %>% 
  group_by(ID, swc_curve) %>%  
  mutate(swc_slope = sd(leaf_water)/sd(psi),
         swc_slope = case_when(swc_curve == T ~ swc_slope),
         # to calculate SWC intercept and slope we are only interested in the points before TLP 
         # so we make the other values O first and then make them NA
         swc_intercept = mean(leaf_water) - (swc_slope * mean(psi)),
         swc_slope = case_when(swc_curve == T ~ swc_intercept),
  ) %>% 
  ungroup() %>% 
  fill(swc_slope, swc_intercept) %>% 
  group_by(ID) %>% 
  mutate(rwc_tot = leaf_water/swc_intercept,
         rwc_tot_per = rwc_tot * 100,
         rwc_tot_per_100 = 100 - rwc_tot_per,
         swc = swc_intercept/dry_weight
  ) %>% 
  ungroup() %>% 
  group_by(ID, tlp_curve) %>% 
  mutate( # same thing as we did for swc above. 
    # In this case we will use the points after the stomatal closure point
    tlp_slope = -sd(psi_inv)/sd(rwc_tot_per_100),
    tlp_slope = case_when(tlp_curve == T ~ tlp_slope),
    # to calculate tlp intercept and slope we are only interested in the points before TLP 
    # so we make the other values O first and then make them NA
    tlp_intercept = mean(psi_inv) - (tlp_slope * mean(rwc_tot_per_100)),
    tlp_intercept = case_when(tlp_curve == T ~ tlp_intercept)) %>% 
  ungroup() %>% 
  fill(tlp_slope, tlp_intercept, .direction = "up") %>% 
  ungroup() %>% 
  group_by(ID) %>% 
  mutate( # 
    psi_ft = -(1/tlp_intercept),
    apoplastic_water = 100 + (tlp_intercept/tlp_slope),
    rwc_sym = (rwc_tot - apoplastic_water/100)/(1 - apoplastic_water/100),
    rwc_sym_per = rwc_sym * 100,
    #  solute/osmotic potential
    psi_osmotic = (-1/(tlp_intercept + tlp_slope * rwc_tot_per_100)),
    #  turgor/pressure potential
    psi_turgor = psi - psi_osmotic,
    #      elasticity stuff      #
  ) %>% 
  select(!c(weight_initial, weight_final, e_cup_initial, e_cup_final))


pv_parameters_calculate <- pv_parameters_calculate1 %>% 
  group_by(ID, swc_curve) %>% 
  mutate(#       capacitance  stuff    #
    # relative capcitance at full turgor
    # "ft" - full turgor
    # total
    capacitance_ft_tot = sd(rwc_tot)/sd(psi),
    # swc
    capacitance_ft_sym = sd(rwc_sym)/sd(psi)) %>% 
  ungroup() %>% 
  mutate(capacitance_ft_tot = case_when(swc_curve == T ~ capacitance_ft_tot),
         capacitance_ft_sym = case_when(swc_curve == T ~ capacitance_ft_sym)) %>%
  ungroup() %>% 
  group_by(ID, tlp_curve) %>% 
  mutate(# relative capacitance at TLP
    capacitance_tlp_tot = sd(rwc_tot)/sd(psi),
    # upto swc point
    capacitance_tlp_sym = sd(rwc_sym)/sd(psi),
    elasticity_tot =  sd(psi_turgor)/sd(rwc_tot),
    elasticity_sym =  sd(psi_turgor)/sd(rwc_sym),
    elasticity_tot =  case_when(tlp_curve == T ~ elasticity_tot),
    elasticity_sym = case_when(tlp_curve == T ~ elasticity_sym), 
  ) %>% 
  ungroup() %>% 
  mutate(capacitance_tlp_tot = case_when(tlp_curve == T ~ capacitance_tlp_tot),
         capacitance_tlp_sym = case_when(tlp_curve == T ~ capacitance_tlp_sym)
  ) %>% 
  mutate(psi_tlp = case_when(tlp_point == T ~ psi_osmotic),
         # 
         rwc_tot_tlp = case_when(tlp_point == T ~ rwc_tot_per),
         # there seems to be some issues here, double check this point 
         rwc_sym_tlp = case_when(tlp_point == T ~ rwc_sym_per)) %>% 
  ungroup()  %>% 
  group_by(ID) %>% 
  fill(capacitance_ft_tot:rwc_sym_tlp, 
       .direction = "downup") 

pv_parameters_calculate %>% 
  distinct(ID, elasticity_tot)

write_csv(pv_parameters_calculate, "outputs/pv_parameters_calculate.csv")

unique(pv_parameters_calculate$site)

# check the points on which the linear model has been fit to estimate the PV parameters
# this is the place to fix all the curves correctly
pv_parameters_calculate %>%
  mutate(sample_id = str_sub(ID, 5, 11)) %>% 
  filter(species == "FASY") %>%
  filter(site == "Leinefelde") %>% 
  ggplot(aes(y = psi_inv, x = mass_cum, group = interaction(ID))) +
  geom_point(col = "#58508d", shape = 21,
             alpha = .6, pch = 20) +
  geom_path(col = "#58508d") +
  geom_smooth(col = "#ffa600", method = "lm",  size = 0.5,
              data = ~filter(.x, tlp_curve == T)) +
  # labs(x = "Cumulative mass of water loss", 
  #      y =  "Psi Inverse") +
  facet_wrap(~ID, scales = "free") +
  theme_bw()

pv_parameters_calculate %>%
  mutate(sample_id = str_sub(ID, 5, 11)) %>% 
 # filter(species == "FASY") %>%
 # filter(site == "Leinefelde") %>% 
  ggplot(aes(y = psi, x = rwc_tot_per, col = campaign, group = interaction(ID))) +
  geom_point( shape = 21,
             alpha = .6, pch = 20) +
  geom_vline(xintercept = 90, linetype = 2) +
  geom_path() +
  xlim(70, 100) +
  scale_color_viridis_d(option = "D") +
  # scale_color_gradient(low = "red", high = "green") +
  # geom_smooth(col = "#ffa600", method = "lm",  size = 0.5,
  #             data = ~filter(.x, tlp_curve == T)) +
  # labs(x = "Cumulative mass of water loss", 
  #      y =  "Psi Inverse") +
  facet_wrap(~sample_id) +
  theme_bw() +
  theme(legend.position = c(0.7, 0.05))


ggsave("figures/01-psi-vs-rwc.png")

pv_parameters_calculate %>%
  mutate(sample_id = str_sub(ID, 5, 11)) %>% 
  # filter(species == "FASY") %>%
  # filter(site == "Leinefelde") %>% 
  ggplot(aes(y = psi_inv, x = rwc_tot_per, col = campaign, group = interaction(ID))) +
  geom_point( shape = 21,
              alpha = .6, pch = 20) +
  geom_vline(xintercept = 90, linetype = 2) +
  geom_path() +
  xlim(70, 100) +
  scale_color_viridis_d(option = "D") +
  # scale_color_gradient(low = "red", high = "green") +
  # geom_smooth(col = "#ffa600", method = "lm",  size = 0.5,
  #             data = ~filter(.x, tlp_curve == T)) +
  # labs(x = "Cumulative mass of water loss", 
  #      y =  "Psi Inverse") +
  facet_wrap(~sample_id) +
  theme_bw() +
  theme(legend.position = c(0.7, 0.05))


ggsave("figures/02-psi_inv-vs-rwc.png")


# export the calculated data
# but still to be cleaned
pv_parameters_calculate %>%  
  write_csv("outputs/pv_parameters_calculate.csv")


# select all the measured pv curve parameters
pv_parameters_clean <-  pv_parameters_calculate %>% 
  select(ID, species, site, campaign, swc, psi_tlp, psi_ft, apoplastic_water, 
         rwc_tot_tlp, rwc_sym_tlp, elasticity_tot, elasticity_sym, 
         capacitance_ft_tot, capacitance_ft_sym,
         capacitance_tlp_tot, capacitance_tlp_sym) %>% 
  # check we do not loose any information here
  distinct_all() %>% 
  mutate(psi_tlp1 = (psi_ft*elasticity_tot)/(psi_ft + elasticity_tot))


# this is the desired output file
pv_parameters_clean %>% 
  write_csv("outputs/pv_parameters_clean.csv")

# Get the averages at treatment level for all the parameters --------------
pv_parameters_clean_summary <- pv_parameters_clean %>% 
  ungroup() %>% 
  group_by(ID) %>% 
  summarise(across(where(is.numeric), list(mean = mean, sd = sd),  na.rm = T))

pv_parameters_clean_summary %>% 
  write_csv("outputs/pv_parameters_clean_summary.csv")


pv_parameters_clean %>% 
  group_by(species, site, campaign) %>% 
  summarise(across(c(psi_ft, psi_tlp, rwc_tot_tlp,
                     elasticity_sym, elasticity_tot), mean)) %>% 
  mutate(across(where(is.numeric), ~round(., 2))) %>% 
  flextable::flextable()

pv_parameters_clean %>% 
  ggplot(aes(site, psi_tlp, fill = interaction(campaign), group = interaction(campaign, species, site))) +
  geom_point(position = position_dodge(0.75, preserve = "total")) +
  geom_boxplot(alpha = 0.5, position = position_dodge2(preserve = c("single", "total"))) +
  scale_fill_viridis_d(option = "D") +
  theme_bw() +
  facet_wrap(~species, scales = "free_x")

pv_parameters_clean %>% 
 # filter(site == "Leinefelde") %>% 
  mutate(sample_id = str_sub(ID, 5, 11)) %>% 
  ggplot(aes(campaign, psi_tlp, group = sample_id, color = sample_id)) +
  geom_point(position = position_dodge(preserve = "total")) +
  geom_line() +
  #geom_boxplot(alpha = 0.5, position = position_dodge2(preserve = c("single", "total"))) +
  scale_color_viridis_d(option = "D") +
  theme_bw() +
  facet_wrap(site~species, scales = "free_x")

ggsave("figures/03-psi_tlp-vs-camapign.png")  

pv_parameters_clean %>% 
  # filter(site == "Leinefelde") %>% 
  mutate(sample_id = str_sub(ID, 5, 11)) %>% 
  ggplot(aes(campaign, psi_ft, group = sample_id, color = sample_id)) +
  geom_point(position = position_dodge(preserve = "total")) +
  geom_line() +
  #geom_boxplot(alpha = 0.5, position = position_dodge2(preserve = c("single", "total"))) +
  scale_color_viridis_d(option = "D") +
  theme_bw() +
  facet_wrap(site~species, scales = "free_x")

ggsave("figures/03-psi_ft-vs-camapign.png") 


pv_parameters_clean %>% 
  # filter(site == "Leinefelde") %>% 
  mutate(sample_id = str_sub(ID, 5, 11)) %>% 
  ggplot(aes(campaign, rwc_tot_tlp, group = sample_id, color = sample_id)) +
  geom_point(position = position_dodge(preserve = "total")) +
  geom_line() +
  #geom_boxplot(alpha = 0.5, position = position_dodge2(preserve = c("single", "total"))) +
  scale_color_viridis_d(option = "D") +
  theme_bw() +
  facet_wrap(site~species, scales = "free_x")

ggsave("figures/03-psi_rwc-vs-camapign.png") 

pv_parameters_clean %>% 
  # filter(site == "Leinefelde") %>% 
  mutate(sample_id = str_sub(ID, 5, 11)) %>% 
  ggplot(aes(campaign, elasticity_tot, group = sample_id, color = sample_id)) +
  geom_point(position = position_dodge(preserve = "total")) +
  geom_line() +
  #geom_boxplot(alpha = 0.5, position = position_dodge2(preserve = c("single", "total"))) +
  scale_color_viridis_d(option = "D") +
  theme_bw() +
  facet_wrap(site~species, scales = "free_x")

ggsave("figures/03-elasticity-vs-camapign.png") 

