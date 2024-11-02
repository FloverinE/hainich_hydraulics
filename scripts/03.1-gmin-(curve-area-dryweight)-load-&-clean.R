###############################################################################
#
# Load, clean, calculate and arrange  gmin data
#
# Script:Sharath Paligi (sharathsp93@gmail.com) 
#      



#   check if there issues with leaf areas
# possible issues - two scans made from one fascicle and only one entered.
# "HN2_SB_071_0283_30needles" check this raw data - how many needles were used
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


# 2.2 add dry weights of the samples here ---------------------------------
gmin_files_path <- here::here("data/gmin/")

# list all the pv curve measured files
gmin_leaf_area_files <- list.files(gmin_files_path, recursive = T, full.names = T, pattern = "LeafArea")


# list all the sheets with the pv data
# read the data here 
gmin_leaf_area <-  tibble(files = gmin_leaf_area_files) %>% 
  mutate(data = map(files, ~ read_excel(.x, skip = 6))) %>%
  unnest(data) %>% 
  select(files, sample_id = `WinFOLIA Reg 2014a`, area = TotLeafArea) %>% 
  clean_names() %>% 
  filter(!sample_id == "SampleId") %>% 
  mutate(campaign = gsub("/Users/sharathsp3/Documents/Projects-R/2023/hainich_hydraulics/data/gmin//",
                        "", files),
         campaign = mgsub::mgsub(campaign, c("_Gmin_LeafArea.xls"),
                                c("")),
         campaign = mgsub::mgsub(campaign, c("C1", "C2", "C3", "C4"), c("1st", "2nd", "3rd", "4th"))) %>% 
  select(!files)


unique(gmin_leaf_area$files)

# 2.2 add dry weights of the samples here ---------------------------------
gmin_files_path <- here::here("data/gmin/")

# list all the pv curve measured files
gmin_dryweight_files <- list.files(gmin_files_path, recursive = T, full.names = T, pattern = "dry")


# list all the sheets with the pv data
# read the data here 
gmin_dry_weight <-  tibble(files = gmin_dryweight_files) %>% 
  mutate(data = map(files, ~ read_excel(.x))) %>%
  unnest(data) %>% 
  clean_names() %>% 
  mutate(factors = gsub("/Users/sharathsp3/Documents/Projects-R/2023/hainich_hydraulics/data/gmin//",
                        "", files),
         factors = mgsub::mgsub(factors, c("gmin_dry_weight_", ".xlsx", "campaign/"),
                                c("", "", ""))) %>% 
  separate(factors, c("campaign", "species", "site", "date"), sep = "_") %>% 
  select(!files)


# 2.3 read data from curves ---------------------------------------------------------------

# path for the file
gmin_files_path <- here::here("data/gmin/")

# list all the pv curve measured files
gmin_files <- list.files(gmin_files_path, recursive = T, pattern = "curve")

# list all the sheets with the pv data
gmin_sheets <- map(here::here(gmin_files_path, gmin_files), excel_sheets)

# read the data here 
gmin_data <-  tibble(files = here::here(gmin_files_path, gmin_files), 
                     sheets = gmin_sheets) %>% 
  unnest_longer(col = sheets) %>% 
  #  slice(17:18) %>% 
  mutate(data = map2(files, sheets, ~ read_excel(.x, .y, 
                                                 progress = readxl_progress()
                                                 ))) %>%
  unnest(data) %>% 
  clean_names() 

gmin_clean <- gmin_data %>% 
  mutate(time_hm = lubridate::ymd_hms(time_hm),
         hours = lubridate::hour(time_hm),
         minutes = lubridate::minute(time_hm),
         #    date = lubridate::dmy(date),
         #hour_min = lubridate::make_datetime(date, hours, minutes )
  ) %>% 
  mutate(factors = gsub("/Users/sharathsp3/Documents/Projects-R/2023/hainich_hydraulics/data/gmin//",
                        "", files),
         factors = mgsub::mgsub(factors, c("gmin_curve_", ".xlsx", "campaign/"),
                                c("", "", ""))) %>% 
  separate(factors, c("campaign", "species", "site", "date"),  sep = "_") %>% 
  fill(sample_id:atm_p)



gmin_clean %>% 
  names()

unique(gmin_dry_weight$files)

gmin_cal <- gmin_clean %>% 
  left_join(gmin_dry_weight) %>% 
  left_join(gmin_leaf_area) %>% 
  rename(leaf_area = area) %>% 
  filter(!is.na(leaf_mass)) %>% 
  
  select(!c(files, sheets, time, day, time_hm, gmin, hours, minutes, start_time, comments)) %>% 
  ungroup() %>% 
  group_by(campaign, sample_id) %>% 
  mutate(id = cur_group_id(),
         max_leaf = max(leaf_mass),
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
         # below the leaf area needs to be changed these are just the preliminary values
         gmin = -(leaf_mass_dif/18*1000)/(time_dif*60)/mfVPD/(leaf_area*2/10000),
         rwc = 100*((leaf_mass_r - dry_weight)/((max(leaf_mass_r)) - dry_weight)),
         rwd = 1 - ((leaf_mass_r - dry_weight)/((max(leaf_mass_r)) - dry_weight)),
  ) %>% 
  unite(id, c(sample_id, campaign), remove = F, sep = "_") %>% 
  select(!c(t, "leaf_mass_no_wax", "petri_dish_mass",  "atm_p", 
            "v_psat","mf_vpd", "date", "wax_mass", "VPsat", "mfVPD", time_dif))

gmin_cal %>% 
  names()

write_csv(gmin_cal, "outputs/gmin_cal.csv")
  
