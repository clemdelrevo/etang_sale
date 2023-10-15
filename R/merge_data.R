# MERGE DATA

clade <- function() {
  
  echinidae <- c(
                 "diadema_savignyi",
                 "echinothrix_diadema",
                 "echinometra_mathaei",
                 "stomopneustes_variolaris",
                 "echinothrix_calamaris",
                 "tripneustes_gratilla",
                 "toxopneustes_pileolus",
                 "heterocentrotus_mamillatus"
                 )
  
  holothuridae <- c(
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
                    )
  
  echinothrix <- c(
                  "echinothrix_diadema",
                  "echinothrix_calamaris"
                  )
  
  actinopyga <- c(
                  "actinopyga_mauritiana",
                  "actinopyga_echinites",
                  "actinopyga_apillata"
                  )
  
  holothuria <- c(
                  "holothuria_leucospilota",
                  "holothuria_atra",
                  "holothuria_cinerascens",
                  "holothuria_nobilis",
                  "holothuria_verrucosa",
                  "holothuria_pervicax",
                  "holothuria_flavomaculata"
                  )
  
  return(list(holothuridae = holothuridae,
              echinidae = echinidae,
              echinothrix = echinothrix,
              actinopyga = actinopyga,
              holothuria = holothuria))
  
}

merge_long_data <- function(belt_long, fix_long, clade) {
  
  #targets::tar_load(belt_long)
  #targets::tar_load(fix_long)
  
  clade <- clade()
  echinidae    <- clade$echinidae
  holothuridae <- clade$holothuridae
  echinothrix  <- clade$echinothrix
  actinopyga   <- clade$actinopyga
  holothuria   <- clade$holothuria
  
  final_long <- rbind(belt_long, fix_long)
  
  final_long$family <- NA  
  final_long$family[final_long$espece %in% echinidae]    <- "echinidae"
  final_long$family[final_long$espece %in% holothuridae] <- "holothuridae"
  
  final_long$genre <- NA
  final_long$genre[final_long$espece %in% echinothrix] <- "echinothrix"
  final_long$genre[final_long$espece %in% actinopyga] <- "actinopyga"
  final_long$genre[final_long$espece %in% holothuria] <- "holothuria"
  final_long$genre[final_long$espece %in% "echinometra_mathaei"] <- "echinometra"
  final_long$genre[final_long$espece %in% "diadema_savignyi"]    <- "diadema"
  final_long$genre[final_long$espece %in% "stomopneustes_variolaris"] <- "stomopneustes"
  final_long$genre[final_long$espece %in% "tripneustes_gratilla"]  <- "tripneustes"
  final_long$genre[final_long$espece %in% "toxopneustes_pileolus"] <- "toxopneustes"
  final_long$genre[final_long$espece %in% "heterocentrotus_mamillatus"] <- "heterocentrotus"
  final_long$genre[final_long$espece %in% "stichopus_chloronotus"] <- "stichopus"
  final_long$genre[final_long$espece %in% "synapta_maculata"] <- "synapta"
  final_long$genre[final_long$espece %in% "bohadschia_vitiensis"] <- "bohadschia"
  
  sum_sp <- tapply(final_long$abondance, final_long$espece, sum)
  
  keep_sp <- names(sum_sp[sum_sp > 0])
  
  final_long <- final_long[final_long$espece %in% keep_sp, ]
  
  return(final_long)

}

return_wide_data <- function(belt_long, fix_wide, belt_wide, final_long) {
  
  #targets::tar_load(belt_long)
  #targets::tar_load(fix_wide)
  #targets::tar_load(belt_wide)
  #targets::tar_load(final_long)
  
  sp <- unique(final_long$espece)
  fix_wide   <- fix_wide[, c("habitat", "station", "method", sp)]
  belt_wide  <- tidyr::spread(belt_long, key = espece, value = abondance)
  belt_wide  <- belt_wide[, c("habitat", "station", "method", sp)]
  final_wide <- rbind(belt_wide, fix_wide)
  
  return(final_wide)

}
