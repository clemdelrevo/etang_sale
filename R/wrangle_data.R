# WRANGLE DATA

# transform data in long data frame

wrangle_belt <- function(belt_wide) {
  
  #targets::tar_load(belt_wide)
  belt_long <- tidyr::pivot_longer(belt_wide, cols = c("diadema_savignyi",
                                                          "echinothrix_diadema",
                                                          "echinometra_mathaei",
                                                          "stomopneustes_variolaris",
                                                          "echinothrix_calamaris",
                                                          "tripneustes_gratilla",
                                                          "toxopneustes_pileolus",
                                                          "heterocentrotus_mamillatus",
                                                          "holothuria_leucospilota",
                                                          "holothuria_atra",
                                                          "stichopus_chloronotus",
                                                          "actinopyga_mauritiana",
                                                          "actinopyga_echinites",
                                                          "holothuria_cinerascens",
                                                          "holothuria_nobilis",
                                                          "holothuria_verrucosa",
                                                          "synapta_maculata",
                                                          "holothuria_pervicax",
                                                          "holothuria_flavomaculata",
                                                          "bohadschia_vitiensis",
                                                          "actinopyga_apillata"
                                                          ))
  
  colnames(belt_long) <- c("habitat", "station", "transect", "method", "espece", "abondance")
  belt_long <- belt_long |>
    dplyr::group_by(habitat, station, method, espece) |>
    dplyr::summarise(abondance = mean(abondance))
  #n <- 1680
  #mean_value <- 10
  #sd_value <- 3
  #belt_long$abondance <- rnorm(n, mean = mean_value, sd = sd_value)
  #belt_wide <- tidyr::pivot_wider(belt_long, names_from = espece, values_from = abondance, values_fn = mean)
  belt_long <- data.frame(belt_long)

  return(belt_long)
  
}

wrangle_fix <- function(fix_wide) {
  
  #targets::tar_load(fix_wide)
  fix_long <- tidyr::pivot_longer(fix_wide, cols = c("diadema_savignyi",
                                                        "echinothrix_diadema",
                                                        "echinometra_mathaei",
                                                        "stomopneustes_variolaris",
                                                        "echinothrix_calamaris",
                                                        "tripneustes_gratilla",
                                                        "toxopneustes_pileolus",
                                                        "heterocentrotus_mamillatus",
                                                        "holothuria_leucospilota",
                                                        "holothuria_atra",
                                                        "stichopus_chloronotus",
                                                        "actinopyga_mauritiana",
                                                        "actinopyga_echinites",
                                                        "holothuria_cinerascens",
                                                        "holothuria_nobilis",
                                                        "holothuria_verrucosa",
                                                        "synapta_maculata",
                                                        "holothuria_pervicax",
                                                        "holothuria_flavomaculata",
                                                        "bohadschia_vitiensis",
                                                        "actinopyga_apillata"
                                                        ))
  
  fix_long <- fix_long[, c("habitat", "station", "method", "name", "value")]
  colnames(fix_long) <- c("habitat", "station", "method", "espece", "abondance")
  #fix_long$method <- "fix"
  #n <- 1680
  #mean_value <- 10
  #sd_value <- 3
  #fix_long$abondance <- rnorm(n, mean = mean_value, sd = sd_value)
  #fix_wide <- tidyr::pivot_wider(fix_long, names_from = espece, values_from = abondance, values_fn = mean)
  fix_long <- data.frame(fix_long)
  
  return(fix_long)
  
}
