library(tidyverse)

data <- read.csv("Q:/GWDG_owncloud/Project_CanopyHydraulics/Data/UniGoe_Laura/sap flow corrected_2023/rawdata1.csv") |> 
  mutate(Time = as.POSIXct(Time, format = "%d/%m/%Y %H:%M:%S"))


ggplot(data) +
  geom_line(aes(x = Time, y = AlphaInner))


sapflow_corr <- read.csv("Q:/GWDG_owncloud/Project_CanopyHydraulics/Data/UniGoe_Laura/sap flow corrected_2023/Sapflow_corrected.csv", sep = ";") |> 
  mutate(Time = as.POSIXct(Time, format = "%Y-%m-%d %H:%M:%S"))

ggplot(sapflow_corr) +
  geom_line(aes(x = Time, y = J_outer)) +
  facet_wrap(~Tree_id)

sapflow_corr |> 
  mutate(date = as.Date(Time)) |>
  group_by(Tree_id, date) |>
  summarise(mean_J = mean(J_outer, na.rm = TRUE),
            sum_J = sum(J_outer, na.rm = TRUE),
            sd_J = sd(J_outer, na.rm = TRUE)) |> 
  ggplot() +
  geom_line(aes(x = date, y = sum_J)) +
  facet_wrap(~Tree_id)
