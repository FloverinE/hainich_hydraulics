###############################################################################
#
# Goal: Estimate gmin values following RWC idea
#   1. merge info about part_Plant, moisture, RWC
#   2. Ccalculate water loss, fit model, estimate gmin



# Script: Sharath Paligi (sharath.paligi@uni-goettingen.de) 
#         Jens Lichter (jens.lichter@uni-goettingen.de)
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
temp <- 25         # durin g the measurements the temp was set to 25 deg
rh = 50            # rh was set to 50 %
atm_p <- 101.4     # constant atm_p

# 2.2 add RWC values at TLP to the gmin data ------------------------------


# 2.3 load data sets ------------------------
# load raw gmin data
gmin_data2 <- read_csv("outputs/gmin_data2.csv") %>% 
# we want to save some info about the part_plant, moisture, and plant_num
  select(id, campaign, dry_weight, leaf_area) %>% 
  distinct_all() 

gmin_data2 %>% 
  names()

# load interpolated mass and time values
gmin_mass_interpolated <- read_csv("outputs/gmin_mass_interpolated.csv") %>% 
  select(!c(newtime,  rwc)) %>% 
  distinct_all()

gmin_mass_interpolated %>% 
  names()

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


gmin_calculation %>% 
  select(id, campaign ) %>% 
  distinct_all()

gmin_calculation %>% names()

# create conditions to model the inclusion and exclusion of the values
gmin_mod_prep <- gmin_calculation %>% 
  # add info about important TLP values and classify them
  # left_join(rwc_TLP) %>% 
  
  filter(!c(time_min > 800)) %>% 
  filter(!rwc < 70) %>% 
  filter(c(time_min > 150)) %>% 
  filter(c(time_min < 450)) %>% 
  
  # classify criterias that we are interested in and exclude the rest from the model
  mutate(mod_include = case_when(species %in% c("FASY") & time_min > 150 ~ TRUE,
                                      species == "FREX" & time_min > 150 ~ TRUE,
                                          TRUE ~ FALSE)) %>%
  relocate(id, si, species, dry_weight, rwc, gmin, mod_include)


gmin_mod_prep %>% 
  filter(campaign == "3rd") %>% 
  ggplot(aes(time_min, rwc)) +
  geom_point() +
  facet_wrap(~id, scales = "free")



# how many points do we have according to our current classification for the linear model
(counts <- gmin_mod_prep %>% 
  group_by(id, species, mod_include) %>% 
  filter(mod_include == T) %>% 
  summarise(mod_include = n()) %>% 
  arrange(mod_include)
)

# make a rough estimate of the values
gmin_rough_est <- gmin_mod_prep %>%
  group_by( campaign,  id, species) %>%
  filter(mod_include == T) %>%
  summarise(gmin = mean(gmin))

gmin_rough_est %>%
  ggplot(aes(species, gmin, fill  = campaign)) +
  geom_boxplot(alpha = 0.5
               #   position = position_dodge(preserve = "total")
  ) +
  geom_jitter( alpha = 0.5,
              position = position_dodge(preserve = c("total", "single"), width = 0.75)) +
  scale_fill_viridis_d( option = "D",
                        name = "Treatment") +
  labs(x = "Species mixture",
       y = expression(paste(g[min], ~"(mmol", ~m^-2, ~s^-1, ")" ))) +
  theme_bw(base_size = 10) +
  theme(legend.text = element_text(size = 10),
        axis.title  = element_text(size = 14),
        strip.text =  element_text(size = 12),
        legend.title = element_text(size = 12),
        axis.text = element_text(size = 10),
        legend.position = c(0.2, 0.81)) +
  guides(fill = guide_legend(nrow = 6), byrow = T)



# 4.1 gmin estimate modeling ----------------------------------------------

# function for the best linear models for a given window breaths
# function for the best linear models for a given window breaths
gmin_best_lm <- function(data, window, tail_sub = NULL){
  n <- nrow(data) - window + 1
  out <- vector(mode = "list", length = n)
  for(i in 1:n) out[[i]] <- lm(leaf_mass_new ~ time_min, data[i:(i + window - 1),])
  # get sigma (r-squared makes no sense because it depends on the slope)
  sigma <- map_dbl(out, ~ summary(.x)$sigma)
  
  # if only a tail subsample is used, make sure other models get pushed out
  if (!is.null(tail_sub) & n > tail_sub) sigma[1:(n - tail_sub)] <- Inf
  
  mod <- out[[which.min(sigma)]]
  
  # return output
  tibble(start = which.min(sigma), 
         sigma = sigma[which.min(sigma)], 
         model = list(mod),
         rsq   = summary(model[[1]])$r.squared)
}

# gmin_best_lm(data, 4, 2)$model[[1]] %>% summary

# function that picks the best model for a specified range
lm_list <- function(data, min = 3, max = 5, tail_sub = NULL){
  tibble(window = min:max, best = map(window, ~gmin_best_lm(data, window = .x, 
                                                            tail_sub = tail_sub))) %>% 
    unnest(best) %>% 
    filter(sigma == min(sigma))
}


# 3. Fit model for all observations -------------------------------------------
# nested version
gmin_best <- gmin_mod_prep %>% 
  ungroup() %>% 
  filter(mod_include == TRUE) %>% 
  group_nest(id, campaign) %>% 
  mutate(res = map(data, ~try(lm_list(.x, min = 3, max = 4, tail_sub = 2)))) %>% 
  unnest(res)

table(gmin_best$window) # most selected windows are as short as possible

# unnested version
gmin_best1 <- gmin_best %>% 
  unnest(data) %>% 
  group_by( id, campaign) %>% 
  mutate(include = 1:n() >= start & 1:n() < start + window,
         rsq = round(rsq, digits = 4))


gmin_mod_points <- gmin_best1 %>% 
  filter(include == T) %>% 
  select(id, campaign, si, include, window, rsq, sigma) 
  
gmin_mod_visualise <- gmin_calculation %>% 
  select(id, si, species, dry_weight, rwc, gmin, time_min,
         leaf_mass_new, campaign) %>% 
  left_join(gmin_mod_points) %>% 
  ungroup() %>% 
  # select(id, part_plant, species, gmin, time_min,
  #        leaf_mass_new, rwc, include, comb, treatment, window) %>% 
  mutate(mod_include = case_when(include == T ~ T, T ~ F)) %>% 
  distinct_all()
  
gmin_mod_visualise_plot <- function(sps, y) {
    ggplot(gmin_mod_visualise %>% 
             filter(sps == species),
           aes(x = time_min, y = .data[[y]], group = interaction(id))) +
    geom_point(aes(col = include), alpha = 0.3, size = 1) +
 #   geom_path(col = "royalblue", size = 0.5) +
    geom_smooth(method = "lm", col = 2, size = 1,
                data = ~filter(.x, include)) +
    facet_wrap(~id, scales = "free") +
    theme_bw() +
    theme(legend.position = "none") 
}

gmin_mod_visualise_plot("FASY", "rwc")
gmin_mod_visualise_plot("FREX", "rwc")


# look at the gmin values and see if they make any sense

(
  gmin_est <- gmin_best1 %>% 
    filter(include == T) %>% 
    group_by(id, campaign, species) %>% 
    arrange(id, campaign) 
)


# arrange the data as required
gmin_data_clean <- gmin_est %>% 
    distinct(id,  .keep_all = T) %>% 
    ungroup() %>% 
 #   relocate(plant_num, id, gmin, rsq, species, leaf_area, everything()) %>% 
    rowwise() %>% 
  group_by(id, campaign, species) %>% 
  summarise(gmin = mean(gmin),
            leaf_area = mean(leaf_area),
            dry_weight = dry_weight,
              time = mean(time_min),
            rwc_mini = min(rwc),
            rwc_max = max(rwc)) %>% 
  # leaf area not true and unreliable measurements for these samples
  filter(!c(species == "Beech" & gmin < 1))


write_csv(gmin_data_clean, "outputs/gmin_data_clean.csv")


# plot gmin data
gmin_data_clean %>% 
  mutate(id = mgsub::mgsub(id, c("FASY_", "FREX_"), c("FASY-", "FREX-"))) %>%
  separate(id, c("sample_id", "campaign"), sep = "_") %>%
  ggplot(aes(species, gmin, fill = campaign)) +
#  stat_summary(geom = "pointrange", position = position_dodge(0.5, preserve = "total")) +
   geom_boxplot(alpha = 0.5,
                position = position_dodge2(preserve = "total")) +
  geom_point(alpha = 0.5, size = 0.7,
              position = position_dodge(preserve = "total", width = 0.75)) +
 # geom_line() +
  scale_fill_viridis_d( option = "D", 
                        name = "Campaign") +
  #  facet_wrap(~species) +
  labs(x = "Species mixture", 
       y = expression(paste(g[min], ~"(mmol", ~m^-2, ~s^-1, ")" ))) +
  theme_bw(base_size = 9) +
  theme(legend.text = element_text(size = 4),
        axis.title  = element_text(size = 7),
        strip.text =  element_text(size = 7),
        legend.title = element_text(size = 5),
        legend.spacing.y = unit(0.05, "cm"),
        legend.spacing.x = unit(0.05, "cm"),
        legend.key.size = unit(0.7, "lines"),
        legend.margin = margin(t = -0.1, r = 0, b = 0, l = -0.2, "cm"),
        legend.position = c(0.91, 0.85)) +
  guides(fill = guide_legend(override.aes = list(size = 0.5))) +
  facet_wrap(~species, scales = "free")

# plot gmin data
gmin_data_clean %>% 
  mutate(id = mgsub::mgsub(id, c("FASY_", "FREX_"), c("FASY-", "FREX-"))) %>%
  separate(id, c("sample_id", "campaign"), sep = "_") %>%
  ggplot(aes(campaign, gmin, col = sample_id, group = sample_id)) +
  #  stat_summary(geom = "pointrange", position = position_dodge(0.5, preserve = "total")) +
  # geom_boxplot(alpha = 0.5,
  #              position = position_dodge2(preserve = "total")) +
  geom_point(alpha = 0.5, size = 0.7,
             position = position_dodge(preserve = "total")) +
  geom_line() +
  scale_fill_viridis_d( option = "D", 
                        name = "Campaign") +
  #  facet_wrap(~species) +
  labs(x = "Species mixture", 
       y = expression(paste(g[min], ~"(mmol", ~m^-2, ~s^-1, ")" ))) +
  theme_bw(base_size = 9) +
  theme(legend.text = element_text(size = 4),
        axis.title  = element_text(size = 7),
        strip.text =  element_text(size = 7),
        legend.title = element_text(size = 5),
        legend.spacing.y = unit(0.05, "cm"),
        legend.spacing.x = unit(0.05, "cm"),
        legend.key.size = unit(0.7, "lines"),
        legend.margin = margin(t = -0.1, r = 0, b = 0, l = -0.2, "cm"),
      #  legend.position = c(0.91, 0.85)
        ) +
  guides(fill = guide_legend(override.aes = list(size = 0.5))) +
  facet_wrap(~species)

ggsave("figures/01-1_gmin.png", height = 50, width = 85, units = "mm", dpi = 600)




