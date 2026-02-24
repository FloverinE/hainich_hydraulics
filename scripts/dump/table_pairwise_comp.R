df_gmin_pairs <- gmin_emmean |>
  pairs() |> 
  as.data.frame() |> 
  mutate(contr1spec = contrast |> str_extract("\\(.*?\\)") |> str_extract("FASY|FREX"),
         contr1date = contrast |> str_extract("\\(.*?\\)") |> str_extract("\\d{4}-\\d{2}-\\d{2}")  |> as.Date(format = "%Y-%m-%d"),
         contr2spec = contrast |> str_extract_all(" \\(.*?\\)")  |> str_extract("FASY|FREX"),
         contr2date = contrast |> str_extract_all(" \\(.*?\\)") |> str_extract("\\d{4}-\\d{2}-\\d{2}") |> as.Date(format = "%Y-%m-%d"),
         year1 = contr1date |> year(),
         year2 = contr2date |> year(),
         ## "classify" into intra- or interspecific
         comparison = case_when(
           contr1spec == contr2spec & contr1date != contr2date & year1 == year2 ~ "intra",
           contr1spec != contr2spec & contr1date == contr2date & year1 == year2 ~ "inter",
           T ~ "other" ## rest is not interesting, e.g. FREX in June vs FASY in August
         ),
         sign_0.05 = p.value < 0.05
  ) |> 
  filter(comparison != "other" ,
         is.na(p.value) == F)

# write.csv(df_gmin_pairs, "data/gmin/df_gmin_pairs.csv", row.names = FALSE)

df_gmin_pairs <- read_csv("data/gmin/df_gmin_pairs.csv")

library(systemfonts)
library(flextable)

df_gmin_pairs |>
  filter(comparison == "inter") |> 
  select(c(contr1spec, contr1date, contr2spec, estimate, p.value, sign_0.05)) |>
  flextable() |>
  colformat_double(j = c("estimate", "p.value"), digits = 3) |>
  ft_theme()

df_gmin_pairs |>
  filter(comparison == "intra") |> 
  select(c(contr1spec, contr1date, contr2date, estimate, p.value, sign_0.05)) |>
  flextable() |>
  colformat_double(j = c("estimate", "p.value"), digits = 3) |>
  ft_theme()
