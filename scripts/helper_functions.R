f.round <- function(x, dec = 3) {
  sprintf(paste0("%.", dec, "f"), round(x, dec))
}

f.p_value_bins <- function(pval) {
  case_when(
    pval > 0.1 ~ paste0(f.round(pval, 3), " n.s."),
    pval > 0.05 & pval <= 0.1 ~ paste0(f.round(pval, 3), " ."),
    pval > 0.01 & pval <= 0.05 ~ paste0(f.round(pval, 3), " *"),
    pval > 0.001 & pval <= 0.01 ~ paste0(f.round(pval, 3), " **"),
    pval <= 0.001 ~ paste0(f.round(pval, 3), " ***"),
    TRUE ~ NA_character_
  )
}
