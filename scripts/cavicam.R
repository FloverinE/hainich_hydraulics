
# Setup -------------------------------------------------------------------
{
  library(tidyverse)
  # library(openxlsx)
  library(scam)
  
  write.excel <- function(x,
                          row.names = FALSE,
                          col.names = FALSE,
                          ...) {
    write.table(
      x,
      "clipboard-2048",
      sep = "\t",
      row.names = row.names,
      col.names = col.names,
    )
  }
}
# load data ---------------------------------------------------------------

df_water_potential <- read.csv("data/cavicam/2024_psi_all.csv")

df_area <- read.csv("data/cavicam/2024_cavicam_all.csv")


# prepare cavitated area --------------------------------------------------

files <- list.files("data//cavicam/camp1/", pattern = "\\.csv", full.names = T)

df_area <- files %>%
  map_df(read_csv, .id = "sample_ID") 

df_area <- files %>%
  set_names(nm = basename(.)) %>%
  map_df(read_csv, .id = "sample_ID") |> 
  rename(id = "...1" ) 


df_area <- df_area |>
  group_by(sample_ID) |>
  mutate(
    area_cav = cumsum(Area),
    perc_area_cav =  area_cav / sum(Area),
    vessel_order = sample_ID |> str_extract("all|major"),
    sample_ID = sample_ID |> str_extract("frex\\_\\d{2}|fasy\\_\\d{2}") |> toupper(),
    minutes = id * 5 - 5
  ) ## 1 picture every 5 minutes

# df_camp3 <- df_area |> 
#   mutate(campaign = "3",
#          year = 2024,
#          date = "2024-08-13")

# df_camp2 <- df_area |> 
#   mutate(campaign = "2",
#          year = 2024,
#          date = "2024-07-10")

df_camp1 <- df_area |> 
  mutate(campaign = "1",
         year = 2024,
         date = "2024-05-19") 

df_area_all <- rbind(df_camp1,
                     df_camp2,
                     df_camp3)[, c(1, 2, 12:19)]

df_area |> 
  ggplot() +
  geom_path(aes(x = minutes, y = perc_area_cav, group = sample_ID, color = sample_ID)) +
  theme_bw() +
  facet_wrap(~ vessel_order)


write.csv(df_area_all, "data/cavicam/2024_cavicam_all.csv", row.names = F)

# prepare water potential ----------------------------------------------

lst <- lapply(2:9, function(i) readxl::read_excel("data/cavicam/camp3/camp3_psi.xlsx", sheet = i))

# coerce list to df
df_cavi <- do.call(rbind.data.frame, lst)
df_cavi <- df_cavi |> 
  mutate(drying_interval = start_measurement - lag(start_measurement,1),
         equilibration_interval =  start_measurement - start_equilibration)

## get elapsed time since start
df_cavi <- df_cavi |> 
  group_by(sample_ID) |> 
  mutate(minutes = difftime(start_measurement, min(start_measurement), units = "mins") |> as.numeric(),
         campaign = "3") |> 
  ungroup()

df_cavi$drying_interval[df_cavi$drying_interval < 1] = NA

df_cavi_camp3 <- df_cavi

df_cavi <- read.csv("data/cavicam/2024_psi_all.csv")

df_cavi <- rbind.data.frame(df_cavi, df_cavi_camp3)

write.csv(df_cavi, "data/cavicam/2024_psi_all.csv", row.names = F)




# predict psi for cavicams ------------------------------------------------

## function
scamfun <- function(water_potential, area){
  mod <- scam(psi ~ s(minutes, bs = "cr", k = 3), data = water_potential)
  predict(mod, newdata = list(minutes = area$minutes))
}


scamfun(
  df_water_potential_nest$water_potential[[24]], 
        df_area_nest$area[[24]]
        )

df_area_nest <- df_area |> mutate(all_id = paste(campaign, sample_ID, sep = "_")) |> 
  nest(data = -c(all_id)) |> 
  rename("area" = "data")

df_water_potential_nest <- df_water_potential |> mutate(all_id = paste(campaign, sample_ID, sep = "_")) |> 
  nest(data = -c(all_id, campaign)) |> 
  rename("water_potential" = "data")

df_all <- left_join(df_area_nest, df_water_potential_nest, by = "all_id")

df_all <- df_all |> 
  filter(all_id != "3_FREX_06") |>
  filter(all_id != "3_FREX_08") |>
  mutate(psi_pred = map2(water_potential, area, scamfun),
         area = map2(area, psi_pred, ~mutate(.x, psi_pred = .y)))

df_area <- df_all |> 
  select(c(all_id, area)) |> 
  unnest() |> 
  mutate(psi_pred_MPa = -psi_pred / 10,
         campaign = str_extract(all_id, "\\d"))


cavi_psi_pred.png <- df_area |>
  ggplot() +
  geom_path(aes(
    x = psi_pred_MPa,
    y = perc_area_cav,
    group = all_id,
    color = campaign),
  linewidth = 0.75) +
  facet_wrap( ~ sample_ID) +
  theme_bw()
cavi_psi_pred.png
ggsave("plots/cavi_psi_pred.png", width = 10, height = 5)


# fit plc -----------------------------------------------------------------

library(fitplc)


# We use the built-in example dataset 'stemvul' in the examples below. See ?stemvul.
# Most examples will fit the Weibull model (the default); try running some of the examples
# with 'model="sigmoidal"' and compare the results.

# 1. Fit one species (or fit all, see next example)
dfr1 <- subset(stemvul, Species =="dpap")

# Fit Weibull model. Store results in object 'pfit'
# 'varnames' specifies the names of the 'PLC' variable in the dataframe,
# and water potential (WP). 
# In this example, we use only 50 bootstrap replicates but recommend you set this
# to 1000 or so.
pfit <- fitplc(dfr1, varnames=c(PLC="PLC", WP="MPa"), nboot=50)

# Look at fit
pfit
#> Class of object 'plcfit' as returned by 'fitplc'.
#> 
#> Parameters and %s%% confidence interval:
#> 
#>  95%    Estimate Norm - 2.5% Norm - 97.5% Boot - 2.5% Boot - 97.5%
#> SX 27.639042   19.016029    38.636204   16.745048    35.622913
#> PX  2.631328    2.310614     2.955739    2.391684     2.975172
#> 

# Make a standard plot. The default plot is 'relative conductivity',
# (which is 1.0 where PLC = 0). For plotting options, see ?plot.plcfit
plot(pfit)

df_test <- df_area |> filter(sample_ID == "FASY_01", 
                  # campaign == "3",
                  psi_pred_MPa > -6) |> 
  mutate(perc_area_cav = perc_area_cav * 100)

pfit_cc <- fitplcs(df_area |> 
                     mutate(species = substr(sample_ID, 1, 4)), 
                  varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"),
                  group = "species",
                  random = "campaign",
                  nboot=100, 
                  model="sigmoid")
pfit_cc |> plot()

pfit_cc |> summary()

# fit all species ---------------------------------------------------------

df_camp1 <- df_area |> 
  filter(campaign == "1")|>
  mutate(perc_area_cav = perc_area_cav * 100,
         species = sample_ID |> str_extract("FASY|FREX"))

## model separated by sample
mod_fix_p50_camp1 <- fitplcs(df_camp1, "sample_ID", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot = 1000, model="sigmoid")
mod_fix_p50_camp1 |> coef() |> filter(Parameter == "PX") |> write.excel()

mod_fix_p12_camp1 <- fitplcs(df_camp1, "sample_ID", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot = 1000, model="sigmoid", x = 12)
mod_fix_p12_camp1 |> coef() |> filter(Parameter == "PX") |> write.excel()

mod_fix_p88_camp1 <- fitplcs(df_camp1, "sample_ID", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot = 1000, model="sigmoid", x = 88)
mod_fix_p88_camp1 |> coef() |> filter(Parameter == "PX") |> write.excel()

df_camp2 <- df_area |> 
  filter(campaign == "2")|>
  mutate(perc_area_cav = perc_area_cav * 100,
         species = sample_ID |> str_extract("FASY|FREX"))

## model separated by sample
mod_fix_p50_camp2 <- fitplcs(df_camp2, "sample_ID", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot = 1000, model="sigmoid")
mod_fix_p50_camp2 |> coef() |> filter(Parameter == "PX") |> select(c(Estimate, `Boot - 2.5%`, `Boot - 97.5%`)) |> write.excel()

mod_fix_p12_camp2 <- fitplcs(df_camp2, "sample_ID", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot = 1000, model="sigmoid", x = 12)
mod_fix_p12_camp2 |> coef() |> filter(Parameter == "PX") |> select(c(Estimate, `Boot - 2.5%`, `Boot - 97.5%`)) |> write.excel()

mod_fix_p88_camp2 <- fitplcs(df_camp2, "sample_ID", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot = 1000, model="sigmoid", x = 88)
mod_fix_p88_camp2 |> coef() |> filter(Parameter == "PX") |> select(c(Estimate, `Boot - 2.5%`, `Boot - 97.5%`)) |> write.excel()




## model separated by sample, species as random effect
all_fit_species <- fitplcs(df_camp1, "sample_ID", varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"), nboot=50, model="sigmoid", random = "species")
plot(all_fit_species, onepanel=TRUE, plotci=FALSE, px_ci="none", pxlinecol="dimgrey")
all_fit_species$FASY_01$fit 




# nlme for plc fit --------------------------------------------------------

library(nlme)
f.plc <- function(x, a, b, c, d) {
  a + (b - a) / (1 + exp(c * (d - x)))
}

fitplc::fweibull
f.weibull_plc <- function (P, SX, PX, X = 50) 
{
  X <- X[1]
  V <- (X - 100) * log(1 - X/100)
  p <- (P/PX)^((PX * SX)/V)
  relk <- (1 - X/100)^p
  return(relk)
}

f.weibull_plc

f.weibull_plc <- formula(perc_area_cav ~ 1 - exp(-(psi_pred_MPa / a) ^ b))

# cumulative weibull function
f.weibull <- function(x, a, b) {
  1 - exp(-(x / a) ^ b)
}

mod1 <- nlme(perc_area_cav ~ f.plc(psi_pred_MPa, a, b, c, d),
             data = df_FASY_01,
             fixed = a + b + c + d ~ 1,
             random = a ~ 1 | campaign,
             start = c(a = 0, b = 100, c = 1, d = -1),
             control = list(msMaxIter = 2000))


# P is (positive-valued) xylem water potential (i.e. P = −Ψ)
# PX, pressure at X% loss of conductivity
# SX is the slope of the Weibull function at P = 0
# ksat, saturated hydraulic conductivity

# Relative conductance/conductivity to PLC.
# relk_to_plc <- function(relk)100 - 100*relk

# PLC to relative conductance/conductivity.

plc_to_relk <- function(plc)(100 - plc)/100

df_test$relk = plc_to_relk(df_test$perc_area_cav)


f.weibull = function (P, SX, PX, X = 50) {
  X = X[1]
  V = (X - 100) * log(1 - X/100)
  p = (P/PX)^((PX * SX)/V)
  relk = (1 - X/100)^p
  return(relk)
}
f.weibull(10, SX, PX, X = 50)

fitplc(df_test,
       varnames=c(PLC="perc_area_cav", WP="psi_pred_MPa"),
       nboot=50,
       model="Weibull")

pfit_cc$data

nls(relK ~ fweibull(P, SX, PX, X = 50),
    data = pfit_cc$data,
    start = list(SX = 124.587305, PX = 4.227376),
    control = nls.control(maxiter = 1000))
    

df_test$psi_pred_MPa

fsigmoidal(df_test$psi_pred_MPa, 124, 4, 50)
    
nls(relk ~ f.weibull(-psi_pred_MPa, SX, PX, X = 50),
    data = df_test,
    start = list(SX = 124, PX = 4),
    control = nls.control(maxiter = 1000))

f.weibull(df_test$psi_pred_MPa, 124, 4, X = 50)


relk ~ (1 - X/100)^p
relk ~ (1 - X/100)^(P/PX)^((PX * SX)/V)
relk ~ (1 - X/100)^(P/PX)^((PX * SX)/(X - 100) * log(1 - X/100))

X = 50




df_test <- df_area |> filter(sample_ID == "FASY_01", 
                  campaign == "3",
                  psi_pred_MPa > -6) |> 
  mutate(perc_area_cav = perc_area_cav * 100)

mod2 <- nls(
  relk ~ (1 - X / 100) ^ ((-psi_pred_MPa / PX) ^ ((PX * SX) / (X - 100) * log(1 - X / 100))),
  data = df_test,
  # fixed = SX + PX ~ 1,
  # random = b ~ 1 | campaign,
  start = list(SX = 124, PX = 4),
  control = nls.control(maxiter = 1000)
)

df_test2 <- df_area |> filter(sample_ID == "FASY_03", psi_pred_MPa > -6) |>
  mutate(perc_area_cav = perc_area_cav * 100,
         relK = plc_to_relk(perc_area_cav))



mod3 <- nlme(
  relK ~ fweibull(-psi_pred_MPa, SX, PX, X = 50),
  data = df_test2,
  fixed = list(SX ~1, PX ~ 1),
  random = SX + PX ~ 1 | campaign,
  start = list(fixed = c(SX = 124, PX = 4)),
  control = nlmeControl(maxiter = 1000, msMaxIter = 2000)
)


ggplot(df_test2) +
  geom_point(aes(x = -psi_pred_MPa, y = relK, col = campaign)) +
  geom_path(aes(x = -psi_pred_MPa, y = predict(mod3), col = campaign, group = campaign)) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


mod3 <- nlme(
  relK ~ fweibull(-psi_pred_MPa, SX, PX, X = 50),
  data = df_area |> filter(psi_pred_MPa > -6) |>
    mutate(perc_area_cav = perc_area_cav * 100,
           relK = plc_to_relk(perc_area_cav), 
           species = substr(sample_ID, 1, 4)),
  fixed = list(SX ~ 1, PX ~ 1),
  random = SX + PX ~ 1 | species/campaign,
  start = list(fixed = c(SX = 124, PX = 4)),
  control = nlmeControl(maxiter = 1000, msMaxIter = 2000)
)

df_area |> filter(psi_pred_MPa > -10) |>
  mutate(perc_area_cav = perc_area_cav * 100,
         relK = plc_to_relk(perc_area_cav), 
         species = substr(sample_ID, 1, 4)) |>
  modelr::add_predictions(mod3, var = "pred_nlme") |>
  ggplot() +
  geom_point(aes(
    x = -psi_pred_MPa,
    y = relK,
    # col = sample_ID,
    # group = sample_ID,
  ), col = "grey", size = 0.5) +
  geom_line(aes(
    x = -psi_pred_MPa, y = pred_nlme,
    col = campaign, group = campaign
  ), linewidth = 1) +
  facet_wrap( ~ species) +
  ylab("Relative conductivity") +
  xlab("Water potential (MPa)") +
  theme_minimal() 

# preprocess images for faster loading into imagej ------------------------
library(magick)
library(tidyverse)

image_path <- "Q:/cavicam/"
samples <- list.dirs(image_path, full.names = T, recursive = F)
frex08 <- samples[1]
new_folder = paste0(frex08, "_resized")
dir.create(new_folder, showWarnings = F)

for(i in 1:length(list.files(frex08, pattern = "\\.png"))) {
  img <- image_read(list.files(frex08, full.names = T)[i])
  img |>
    image_resize("1296x972") |>
    image_write(paste0(new_folder, "/", list.files(frex08)[i]))
  if (i %% 10 == 0) {
    print(i)
  }
}

# test area ---------------------------------------------------------------



df_test <- df_all |> 
  mutate(psi_pred_cavi = map2(water_potential, water_potential, scamfun),
         cavi = map2(water_potential, psi_pred_cavi, ~mutate(.x, psi_pred_cavi = .y)))

df_test2 <- df_test |> 
  select(c(sample_ID, cavi)) |> 
  unnest()


img1 <- image_read("Q:/cavicam/Cav15_FREX_05_FW/20240531-214508.png", depth = 8)
img2 <- image_read("Q:/cavicam/Cav15_FREX_05_FW/20240531-215019.png", depth = 8)


img1 <- img1 |> image_convert(colorspace = "gray") 
img2 <- img2 |> image_convert(colorspace = "gray")

img1data = img1 |> image_data() |> as.data.frame() 
img1data |> col2rgb() 


# dump --------------------------------------------------------------------

fasy011 <- read.csv("cavicam/camp1/frex07_1-800_qnd.csv")
fasy012 <- read.csv("cavicam/camp1/frex07_800-end_qnd.csv")

fasy01 <- rbind.data.frame(fasy011, fasy012) 
fasy01 <- fasy01 |> 
  mutate(sample_ID = "FREX_07",
         campaign = "1",
         year = "2024",
         date = "2024-05-29",
         X = 1:nrow(fasy01))
write.csv(fasy01, "cavicam/camp1/FREX_07.csv", row.names = F)


  

