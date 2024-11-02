

pv_parameters_calculate %>% 
  names()



pv_parameters_calculate %>%
  filter(species == "FREX") %>%
  ggplot(aes(y = psi, x = rwc_tot_per, group = interaction(ID))) +
  geom_point(col = "#58508d", shape = 21,
             alpha = .6, pch = 20) +
  geom_path(col = "#58508d") +
  geom_smooth(col = "#ffa600", method = "lm",  size = 0.5,
              data = ~filter(.x, tlp_curve == T)) +
  labs(x = "Relative water content (%)",
       y =  "Xylem water potential (Bars)") +
  facet_wrap(~ID, scales = "free") +
  theme_bw()

ggsave("figures/1.1-fraxinus-(PSI-RWC).png", width = 230, height = 200, units = "mm", dpi = 300)

 


pv_parameters_calculate %>%
  filter(species == "FASY") %>%
  ggplot(aes(y = psi, x = rwc_tot_per, group = interaction(ID))) +
  geom_point(col = "#58508d", shape = 21,
             alpha = .6, pch = 20) +
  geom_path(col = "#58508d") +
  geom_smooth(col = "#ffa600", method = "lm",  size = 0.5,
              data = ~filter(.x, tlp_curve == T)) +
  labs(x = "Relative water content (%)",
       y =  "Xylem water potential (Bars)") +
  facet_wrap(~ID, scales = "free") +
  theme_bw()

ggsave("figures/1.2-beech-(PSI-RWC).png", width = 230, height = 200, units = "mm", dpi = 300)



pv_parameters_calculate %>%
  filter(species == "SPRUCE") %>%
  ggplot(aes(y = psi, x = rwc_tot_per, group = interaction(ID))) +
  geom_point(col = "#58508d", shape = 21,
             alpha = .6, pch = 20) +
  geom_path(col = "#58508d") +
  geom_smooth(col = "#ffa600", method = "lm",  size = 0.5,
              data = ~filter(.x, tlp_curve == T)) +
  labs(x = "Relative water content (%)",
       y =  "Xylem water potential (Bars)") +
  facet_wrap(~ID, scales = "free") +
  theme_bw()

ggsave("figures/1.3-spruce-(PSI-RWC).png", width = 230, height = 200, units = "mm", dpi = 300)

