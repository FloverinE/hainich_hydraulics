emmeans(regrid(emm), pairwise ~ year + species, type = "response")


has_na = case_when(as.data.frame(emm)$emmean |> is.na() |> sum() > 1 ~ T, T ~ F)

if (has_na) {
  # there are non‑estimable rows → keep the original object
  emm <- emm
} else {
  # all rows are estimable → safe to call regrid()
  emm <- regrid(emm)
}
