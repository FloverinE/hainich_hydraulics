library(tidyverse)

source("scripts/ggplot_themes.R")

df_choat_graph = data.frame(x_decr = seq(-30, 50, length = 100),
                            x_incr = seq(10, 100, length = 100),
                            x = seq(1, 100, length = 100)) |> 
  mutate(
    y_decr = 1 / (1 + exp(0.3 * x - 10)),
    y_incr = 1 / (1 + exp(-0.2 * x + 10))
  )

sec_axis_val = 1

p1 <- ggplot(df_choat_graph |> filter(x < 20)) +
  geom_line(aes(x = x, y = y_decr), linewidth = 1, color = "blue") +
  geom_line(aes(x = x, y = y_incr), linewidth = 1, color = "red") +
  # geom_line(aes(x = x + 10, y = y_incr), linewidth = 1, color = "red", linetype = "dashed") +
  annotate("text", x = 20, y = 1.05, label = "Stomatal conductance", color = "blue") +
  # annotate("text", x = 80, y = 0.05, label = "Cuticular conductance", color = "blue") +
  annotate("text", x = 20, y = 0.05, label = "Embolism", color = "red") +
  # annotate("point", x = 40, y = 0.12, size = 5, shape = 4, stroke = 2) +
  # annotate("text", x = 50, y = 0.10, label = expression(TLP ~ "&" ~ P[12]), 
           # hjust = 0, vjust = 0, parse = TRUE, size = 5) +
  # annotate("point", x = 50, y = 0.5, size = 5, shape = 4, stroke = 2) +
  # annotate("text", x = 60, y = 0.5, label = expression(P[50]), hjust = 0, vjust = 0, parse = TRUE, size = 5) +
  # annotate("point", x = 60, y = 0.88, size = 5, shape = 4, stroke = 2) +
  # annotate("text", x = 70, y = 0.88, label = expression(P[88]), hjust = 0, vjust = 0, parse = TRUE, size = 5) +
  annotate("segment", x = 35, y = -0.17, xend = 65, yend = -0.17, arrow = arrow(type = "closed", length = unit(0.1, "inches")), linewidth = 1) +
  scale_y_continuous(expand = c(0, 0.0),  
                     name = "Water loss from stomata and cuticles",
                     sec.axis = sec_axis(~.*1, name="Loss of hydraulic conductance\ndue to embolism")) +
  coord_cartesian(clip = "off", ylim = c(-0.1, 1.1)) + 
  xlim(0, 100) +
  xlab(expression(atop("Increasing drought stress", "Decreasing " * Psi ~and~RWC))) +
  thesis_theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
p1

p2 <- ggplot(df_choat_graph |> filter(x < 50)) +
  geom_line(aes(x = x, y = y_decr), linewidth = 1, color = "blue") +
  geom_line(aes(x = x, y = y_incr), linewidth = 1, color = "red") +
  # geom_line(aes(x = x + 10, y = y_incr), linewidth = 1, color = "red", linetype = "dashed") +
  annotate("text", x = 20, y = 1.05, label = "Stomatal conductance", color = "blue") +
  annotate("text", x = 80, y = 0.05, label = "Cuticular conductance", color = "blue") +
  annotate("text", x = 20, y = 0.05, label = "Embolism", color = "red") +
  annotate("point", x = 40, y = 0.12, size = 5, shape = 4, stroke = 2) +
  annotate("text", x = 50, y = 0.10, label = expression(TLP ~ "&" ~ P[12]), 
           hjust = 0, vjust = 0, parse = TRUE, size = 5) +
  # annotate("point", x = 50, y = 0.5, size = 5, shape = 4, stroke = 2) +
  # annotate("text", x = 60, y = 0.5, label = expression(P[50]), hjust = 0, vjust = 0, parse = TRUE, size = 5) +
  # annotate("point", x = 60, y = 0.88, size = 5, shape = 4, stroke = 2) +
  # annotate("text", x = 70, y = 0.88, label = expression(P[88]), hjust = 0, vjust = 0, parse = TRUE, size = 5) +
  annotate("segment", x = 35, y = -0.17, xend = 65, yend = -0.17, arrow = arrow(type = "closed", length = unit(0.1, "inches")), linewidth = 1) +
  scale_y_continuous(expand = c(0, 0.0),  
                     name = "Water loss from stomata and cuticles",
                     sec.axis = sec_axis(~.*1, name="Loss of hydraulic conductance\ndue to embolism")) +
  coord_cartesian(clip = "off", ylim = c(-0.1, 1.1)) + 
  xlim(0, 100) +
  xlab(expression(atop("Increasing drought stress", "Decreasing " * Psi ~and~RWC))) +
  thesis_theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
p2

p3 <- ggplot(df_choat_graph) +
  geom_line(aes(x = x, y = y_decr), linewidth = 1, color = "blue") +
  geom_line(aes(x = x, y = y_incr), linewidth = 1, color = "red") +
  # geom_line(aes(x = x + 10, y = y_incr), linewidth = 1, color = "red", linetype = "dashed") +
  annotate("text", x = 20, y = 1.05, label = "Stomatal conductance", color = "blue") +
  annotate("text", x = 80, y = 0.05, label = "Cuticular conductance", color = "blue") +
  annotate("text", x = 20, y = 0.05, label = "Embolism", color = "red") +
  annotate("point", x = 40, y = 0.12, size = 5, shape = 4, stroke = 2) +
  annotate("text", x = 50, y = 0.10, label = expression(TLP ~ "&" ~ P[12]), 
           hjust = 0, vjust = 0, parse = TRUE, size = 5) +
  annotate("point", x = 50, y = 0.5, size = 5, shape = 4, stroke = 2) +
  annotate("text", x = 60, y = 0.5, label = expression(P[50]), hjust = 0, vjust = 0, parse = TRUE, size = 5) +
  annotate("point", x = 60, y = 0.88, size = 5, shape = 4, stroke = 2) +
  annotate("text", x = 70, y = 0.88, label = expression(P[88]), hjust = 0, vjust = 0, parse = TRUE, size = 5) +
  annotate("segment", x = 35, y = -0.17, xend = 65, yend = -0.17, arrow = arrow(type = "closed", length = unit(0.1, "inches")), linewidth = 1) +
  scale_y_continuous(expand = c(0, 0.0),  
                     name = "Water loss from stomata and cuticles",
                     sec.axis = sec_axis(~.*1, name="Loss of hydraulic conductance\ndue to embolism")) +
  coord_cartesian(clip = "off", ylim = c(-0.1, 1.1)) + 
  xlim(0, 100) +
  xlab(expression(atop("Increasing drought stress", "Decreasing " * Psi ~and~RWC))) +
  thesis_theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
p3

p4 <- ggplot(df_choat_graph) +
  geom_line(aes(x = x, y = y_decr), linewidth = 1, color = "blue") +
  geom_line(aes(x = x, y = y_incr), linewidth = 1, color = "red") +
  geom_line(aes(x = x + 10, y = y_incr), linewidth = 1, color = "red", linetype = "dashed") +
  annotate("text", x = 20, y = 1.05, label = "Stomatal conductance", color = "blue") +
  annotate("text", x = 80, y = 0.05, label = "Cuticular conductance", color = "blue") +
  annotate("text", x = 20, y = 0.05, label = "Embolism", color = "red") +
  annotate("segment", x = 40, y = 0.12, xend = 49, yend = 0.12, arrow = arrow(type = "closed", length = unit(0.1, "inches")), 
           linewidth = 0.8, linetype = "solid") +
  annotate("point", x = 50, y = 0.12, size = 5, shape = 4, stroke = 2) +
  annotate("text", x = 55, y = 0.10, label = expression(TLP ~ "&" ~ P[12]), 
           hjust = 0, vjust = 0, parse = TRUE, size = 5) +
  annotate("segment", x = 50, y = 0.5, xend = 59, yend = 0.5, arrow = arrow(type = "closed", length = unit(0.1, "inches")), 
           linewidth = 0.8, linetype = "solid") +
  annotate("point", x = 60, y = 0.5, size = 5, shape = 4, stroke = 2) +
  annotate("text", x = 65, y = 0.5, label = expression(P[50]), hjust = 0, vjust = 0, parse = TRUE, size = 5) +
  annotate("segment", x = 60, y = 0.88, xend = 69, yend = 0.88, arrow = arrow(type = "closed", length = unit(0.1, "inches")), 
           linewidth = 0.8, linetype = "solid") +
  annotate("point", x = 70, y = 0.88, size = 5, shape = 4, stroke = 2) +
  annotate("text", x = 75, y = 0.88, label = expression(P[88]), hjust = 0, vjust = 0, parse = TRUE, size = 5) +
  annotate("segment", x = 35, y = -0.17, xend = 65, yend = -0.17, arrow = arrow(type = "closed", length = unit(0.1, "inches")), linewidth = 1) +
  scale_y_continuous(expand = c(0, 0.0),  
                     name = "Water loss from stomata and cuticles",
                     sec.axis = sec_axis(~.*1, name="Loss of hydraulic conductance\ndue to embolism")) +
  coord_cartesian(clip = "off", ylim = c(-0.1, 1.1)) + 
  xlim(0, 100) +
  xlab(expression(atop("Increasing drought stress", "Decreasing " * Psi ~and~RWC))) +
  thesis_theme +
  theme(axis.text.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())
p4

ggsave("figures/choat_graph1.png", p1, width = 7, height = 5)
ggsave("figures/choat_graph2.png", p2, width = 7, height = 5)
ggsave("figures/choat_graph3.png", p3, width = 7, height = 5)
ggsave("figures/choat_graph4.png", p4, width = 7, height = 5)
