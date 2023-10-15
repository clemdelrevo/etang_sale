# IMPORT DATA

# import belt data

import_es_belt <- function() {
  
  belt_wide <- read.csv2("data/es_wide_belt.csv")
  
  belt_wide
  
}

# import fixe data

import_es_fix <- function() {
  
  fix_wide <- read.csv2("data/es_wide_fix.csv")
  
  fix_wide

}

#ex vegan

#data("BCI")
#cumul <- apply(BCI, 2, cumsum)
#richesse <- apply(cumul, 1, function(x) sum(x > 0))
#d <- data.frame(A = 0:50, S = c(0, richesse))

#ggplot2::ggplot(d, ggplot2::aes(A, S)) +
  #ggplot2::geom_line()
