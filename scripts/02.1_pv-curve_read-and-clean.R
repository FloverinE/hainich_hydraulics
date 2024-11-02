# Read and clean pV curve datasets from Chaco campaign
#
# Script:Sharath Paligi (sharathsp93@gmail.com) 
#
###############################################################################

# 1. Preparations -------------------------------------------------------------

pacman::p_load(tidyverse, readxl, janitor)

conflicts_prefer(dplyr::filter)

# 2. reading PV data --------------------------------------------
# get headers
# path for the file
pv_files_path <- here::here("data/pv-curve-h/")



# list all the pv curve measured files
pv_files <- list.files(pv_files_path, recursive = T, pattern = "curve")

# list all the sheets with the pv data
pv_sheets <- map(here::here(pv_files_path, pv_files), excel_sheets)

# load all the data from all campaigns
# here list the names and then mix them up later with the data
pv_data_header <-  tibble(files = here::here(pv_files_path, pv_files), 
                          sheets = pv_sheets) %>% 
  unnest_longer(col = sheets) %>% 
  # slice(50:51) %>% 
  mutate(data = map2(files, sheets, ~ read_excel(.x, .y, 
                                                 progress = readxl_progress(),
                                                 range = cell_rows(c(1:2))))) %>% 
  unnest(data) %>% 
  clean_names() 

# read the data here 
pv_data <-  tibble(files = here::here(pv_files_path, pv_files), 
                   sheets = pv_sheets) %>% 
  unnest_longer(col = sheets) %>% 
  #  slice(17:18) %>% 
  mutate(data = map2(files, sheets, ~ read_excel(.x, .y, 
                                                 progress = readxl_progress(),
                                                 skip = 2))) %>%
  unnest(data) %>% 
  clean_names() %>% 
  select(!sample)

# merge the names and the other details
pv_data_clean <- pv_data %>% 
  left_join(pv_data_header) %>% 
  rename(psi = water_potential_10_min, p_inv = x1_p_1_m_pa, ID = sample,
         mass_diff = weight_difference, mass_cum = curve, e_cup_initial = e_cup) %>% 
  filter(!is.na(psi))  


# 5. Export final dataset -----------------------------------------------------
write_csv(pv_data_clean, "data/pv_data_clean.csv")


