# setup -------------------------------------------------------------------
library(tidyverse)




# load data ---------------------------------------------------------------



# constants ---------------------------------------------------------------

## density of water [kg / m^3]
rho_water = 998.2 # from Williams 1996 p10

## gravitational acceleration [m / s^2]
g = 9.81

## time step [s]
delta_t_sec = 1800

## canopy layer capacitance [mmol / MPa m^2]
capacitance_n = 8000 # from Williams 1996 p10 "Schulze 1985"

delta # slope of the saturation vapour pressure temperature relationship [kPa / K]

## psychrometric constant [Pa / K]
gamma = 66 

## latent heat of vaporization [MJ / m^-3]
LE = 2453 

# functions ---------------------------------------------------------------

convert_H2O_mm2mmol <- function(H2O_mm) {
  H2O_mmol = H2O_mm / 18.01528
  return(H2O_mmol)
}

## calculate canopy layer transpiration E_n [mmol / m^2 s]
## FAO version ET [mm/day]

f_ET_FAO <- function(delta, Rn,  G, gamma, TA_degC,  u,  es,  ea) {
  ET = (0.408 * delta * (Rn - G) + gamma * (900 / TA_degC + 273) * u * (es - ea)) / 
    (delta + gamma * (1 + 0.34 * u))
  return(E_n)
}

##
f_ET <- function(delta, Rn, G, rho_air, c_p, VPD, g_air, gamma, g_sw, LE) {
  ET = (delta * (Rn - G) + rho_air * c_p * VPD * g_air) / 
    (delta + gamma * (1 + g_sw / g_air) * LE)
  return(ET)
}




## calculate leaf water potential
f_psi_leaf <- function(psi_soil, layer_height_m, E_n, R_sn, R_pn, psi_leaf_old, capacitance_n) {
  psi_leaf_new = delta_t_sec * ((psi_soil - rho_water * g * layer_height_m - E_n * (R_sn - R_pn) - psi_leaf_old) / 
                                  capacitance_n * (R_sn - R_pn))
  return(psi_leaf_new)
}



