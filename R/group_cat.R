group_cat <- function(x, cat, newvars, vars, comp) {

  # for now, only works 1 variable at a time
  # also only works for narrow (1 variable) cat
  cat_sym <- as.symbol(cat)
  newvars <- as.symbol(newvars)
  x <- group_by(x, !!cat_sym)
  x2 <- summarise_at(x, vars, comp)
  x2 <- rename(x2, !!newvars := vars)
  x <- ungroup(x)
  x <- dplyr::full_join(x, x2, by = cat)
  # x <- statajoin::stata_join(x, x2, type = "m:1", by = cat)
  x$merge <- NULL
  rm(x2)
  return(x)

}
