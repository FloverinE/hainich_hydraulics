# setup -------------------------------------------------------------------

library(officer)
library(tidyverse)

df.trait_tables = list.files("tables/01_hydr_traits/emmeans_contrasts/", pattern = ".docx", full.names = T) |> 
  as.data.frame() |> 
  setNames("files")

# keep in alphabetical order
df.trait_tables = df.trait_tables |> 
  mutate(tables = map(files, ~ .x |> read_docx()))
  

df.trait_tables$tables[1]
