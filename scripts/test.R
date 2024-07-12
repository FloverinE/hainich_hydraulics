
library(tidyverse)

df_circ_beech_h <- read.csv("data/circ_change_beech_hour.csv")
df_circ_beech_h <- df_circ_beech_h |> 
  mutate(time = as.POSIXct(time))


ggplot(df_circ_beech_h) +
  geom_line(aes(x = time, y = Fs_418))

plot(time ~ Fs_418, data = df_circ_beech_h)

df_circ_beech_h$time |> min()
df_circ_beech_h$Fs_418 |> min()

df_circ_beech_h_long <- df_circ_beech_h |> pivot_longer(-time)

df_circ_beech_h_long |> 
  ggplot() +
  geom_line(aes(x = time, y = value)) +
  facet_wrap(~name)
