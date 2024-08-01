###############################################################################
#
# Load, clean, calculate and arrange  gmin data
#
# Script:Sharath Paligi (sharathsp93@gmail.com) 
#      
###############################################################################

# 1. Preparations -------------------------------------------------------------

pacman::p_load(tidyverse, readxl, janitor, conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")


# set constants used in this script

eu_cons <- 2.71828 # euler's constant 
temp <- 25         # durin g the measurements the temp was set to 25 deg
rh = 50            # rh was set to 50 %
atm_p <- 101.4     # constant atm_p

# 2. Load datasets --------------------------------------------------------

# 2.1 Load leaf area data -------------------------------------------------

# list files. General path for all files
gmin_files_path <- here::here("data/gmin/")

# list all the pv curve measured files
gmin_leaf_area_files <- list.files(gmin_files_path, recursive = T,
                                   full.names = T, pattern = "area")

# list all the sheets with the pv data
# read the data here 
gmin_leaf_area <-  tibble(files = gmin_leaf_area_files) %>% 
  mutate(data = map(files, ~ read_excel(.x))) %>%
  unnest(data) %>% 
  select(files, sample_id = `WinFOLIA Reg 2014a`, area = TotLeafArea, timestamp = 'Operator Date Time') %>% 
  clean_names() %>% 
  filter(!sample_id == "SampleId") 

# check the file names
unique(gmin_leaf_area$sample_id)


# clean the desiccation curve measurements
gmin_leaf_area_clean <- gmin_leaf_area %>% 
  mutate(timestamp = lubridate::dmy_hms(timestamp),
         date = lubridate::date(timestamp), # get date time, we get campaign info from this
         # extract temp treatments
         treatment = case_when(str_detect(sample_id, "T20") ~ "T20",
                               str_detect(sample_id, "T25") ~ "T25",
                               str_detect(sample_id, "T35") ~ "T35"),
         sample_id = toupper(sample_id),
         # extract species
         species = case_when(str_detect(sample_id, "FREX") ~ "FREX",
                             str_detect(sample_id, "TICO") ~ "TICO",
                             str_detect(sample_id, "ROPS") ~ "ROPS",
                             str_detect(sample_id, "ACPL") ~ "ACPL"),
         # extract tree id
         tree_id = str_sub(sample_id, 7, 8),
         # separate info for which leaf from scanning was used for the measurement
         leaf_id = str_sub(sample_id, 1, 1),
         # create new sample id
         sample_id = paste0(species, "_", tree_id),
         campaign = case_when(date < '2024-06-30' ~ "June")) %>% 
  # remove unnecessary variables from this
  select(!c(files, timestamp, tree_id, date)) %>% 
  arrange(sample_id, leaf_id) %>% 
  group_by(sample_id, species, leaf_id, treatment, campaign) %>% 
  summarise(area = mean(area))

unique(gmin_leaf_area_clean$sample_id)

# 2.2 add dry weights of the samples here ---------------------------------
# extract only files with gmin measurements
gmin_dryweight_files <- list.files(gmin_files_path, 
                                   recursive = T, full.names = T, pattern = "dry")

# list all the sheets with the pv data
# read the data here 
gmin_dry_weight <-  tibble(files = gmin_dryweight_files) %>% 
  mutate(data = map(files, ~ read_excel(.x))) %>%
  unnest(data) %>% 
  clean_names() %>% 
  mutate(treatment = case_when(str_detect(files, "T20") ~ "T20",
                               str_detect(files, "T25") ~ "T25",
                               str_detect(files, "T35") ~ "T35"),
         date = lubridate::mdy(date),
         campaign = case_when(date < '2024-06-30' ~ "June"),
         sample_id = gsub("_", "", sample_id),
         species = str_sub(sample_id, 1, 4),
         tree_id = str_sub(sample_id, 5, 6),
         tree_id = str_pad(tree_id, 2, pad = "0"),
         sample_id = paste0(species, "_", tree_id)) %>% 
  select(sample_id, species, tree_id, campaign, weight_without_container, date, treatment) %>% 
  rename(dry_weight = weight_without_container)


unique(gmin_dry_weight$sample_id)

# 2.3 read data from curves ---------------------------------------------------------------

# list all the measured curves
gmin_curve_files <- list.files(gmin_files_path, recursive = T, pattern = "curve")

# list all the sheets with the pv data
gmin_curve_sheets <- map(here::here(gmin_files_path, gmin_curve_files), excel_sheets)

# read the data here 
gmin_data <-  tibble(files = here::here(gmin_files_path, gmin_curve_files), 
                     sheets = gmin_curve_sheets) %>% 
  unnest_longer(col = sheets) %>%  
  mutate(data = map2(files, sheets, ~ read_excel(.x, .y, 
                                                 progress = readxl_progress()))) %>%
  unnest(data) %>% 
  clean_names() 

# clean the dataset
gmin_clean <- gmin_data %>% 
  # fix the timestamps
  mutate(time_hm = lubridate::ymd_hms(time_hm),
         hours = lubridate::hour(time_hm),
         minutes = lubridate::minute(time_hm),
         time_hm = paste0(hours, ":", minutes)) %>% 
  # assign treatment
  mutate(treatment = case_when(str_detect(files, "T20") ~ "T20",
                               str_detect(files, "T25") ~ "T25",
                               str_detect(files, "T35") ~ "T35"),
         # extract temperature for calulation for that particular treatment
         temp = case_when(treatment == "T25" ~ 25,
                          treatment == "T35" ~ 35),
         date = lubridate::ymd(date),
         # assign the measurement date, will be useful for later if we 
         # conduct multiple measurement campaigns
         campaign = case_when(date < '2024-06-30' ~ "June"),
         # sample_id to CAPITAL LETTERS
         sample_id = toupper(str_sub(sample_id, 1, 7))) %>% 
  # select varuables we only need
  select(c(sample_id, atm_p, leaf_id, temp, treatment, campaign, leaf_mass_no_wax, petri_dish_mass, 
           time_hm, real_time, leaf_mass, hours, minutes)) %>% 
  fill(sample_id:petri_dish_mass)


unique(gmin_clean$species)
unique(gmin_leaf_area_clean$species)
unique(gmin_dry_weight$species)


gmin_cal <- gmin_clean %>% 
  #filter(sample_id %in% c("ACPL_01")) %>% 
  left_join(gmin_dry_weight) %>% 
  left_join(gmin_leaf_area_clean) %>% 
  rename(leaf_area = area) %>% 
  filter(!is.na(dry_weight)) %>% 
  filter(!is.na(leaf_mass)) %>% 
  ungroup() %>% 
  filter(real_time < 2000) %>% 
  mutate(id = paste0(sample_id, "_", treatment)) %>% 
  group_by(treatment, sample_id, campaign, leaf_id) %>% 
  mutate( max_leaf = max(leaf_mass),
          wax_mass =  max(leaf_mass) - leaf_mass_no_wax,
          leaf_mass_r = leaf_mass - petri_dish_mass  - wax_mass,
          # Saturated vapor pressure in kpa (kilo pascals)
          VPsat = 610.78*eu_cons^(temp/(temp+237.3)*17.2694)/1000,
          # calculation of VPD
         mfVPD = (1-(rh/100))*VPsat/atm_p,
         # time difference between measurements
         time_dif = real_time - lag(real_time),
         # difference in leaf weights
         leaf_mass_dif = leaf_mass_r - lag(leaf_mass_r),
         # calulate the standardized loss of water per unit time
         gmin = -(leaf_mass_dif/18*1000)/(time_dif*60)/mfVPD/(leaf_area*2/10000),
         rwc = 100*((leaf_mass_r - dry_weight)/((max(leaf_mass_r)) - dry_weight)),
         rwd = 1 - ((leaf_mass_r - dry_weight)/((max(leaf_mass_r)) - dry_weight))) %>% 
  unite(id, c(id, campaign), remove = F, sep = "_") %>% 
  select(!c("leaf_mass_no_wax", "petri_dish_mass",  "atm_p", 
             "wax_mass", "VPsat", "mfVPD", time_dif))


# check each individual curves and see how they dropeed their mass with time
gmin_cal %>% 
  ggplot(aes(real_time, rwc, group = interaction(id, sample_id, temp))) +
  geom_point() +
  geom_line() +
  facet_wrap(temp~sample_id, scales = "free") +
  theme_bw()

# check relative values
# so we will plot with RWC (relative water content against time)!
gmin_cal %>% 
  ggplot(aes(real_time, rwc, group = interaction(id),
             col = treatment)) +
  geom_point() +
  geom_line(aes(col = treatment)) +
  facet_wrap(~species, scales = "free") +
  theme_bw()

write_csv(gmin_cal, "outputs/gmin_cal.csv")

