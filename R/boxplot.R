get_boxplot_habitat <- function(final_long) {
  
  #targets::tar_load(final_long)
  get_sp <- final_long[final_long$espece %in% c("echinometra_mathaei", "stichopus_chloronotus") & !final_long$habitat == "dar_a_epandage_detritique", ]
  
  boxplot <- lapply(unique(get_sp$espece), function(x) {
    
    sp <- get_sp[get_sp$espece == x, ]
    stat.test <- sp |>
       dplyr::group_by(habitat) |>
       rstatix::wilcox_test(abondance ~ method) |>
       rstatix::adjust_pvalue() |>
       rstatix::add_significance("p.adj")
      
    bxp <- ggpubr::ggboxplot(sp, x = "method", y = "abondance",
                            fill = "method", palette = c("#868686FF", "#EFC000FF"),
                            xlab = "Méthode", ylab = "Abondance", facet.by = "habitat") +
     ggplot2::theme(legend.position = "none",
                    strip.background = ggplot2:::element_rect(fill = "grey")) +
     ggplot2::ggtitle(gsub("_", " ", tools::toTitleCase(x)))
   stat.test <- stat.test |> rstatix::add_xy_position(x = "method")
   bxp +
     ggpubr::stat_pvalue_manual(stat.test, label = "Wilcoxon-test, p = {p}",
                                bracket.nudge.y = -1)
  })

  cowplot::plot_grid(boxplot[[1]], boxplot[[2]], nrow = 2)
  
}

get_boxplot_sp <- function(final_long) {
  
  #targets::tar_load(final_long)
  get_sp <- final_long[final_long$espece %in% c("echinometra_mathaei", "stichopus_chloronotus"), ]
  
  boxplot <- lapply(unique(get_sp$espece), function(x) {
    
    #x = "echinometra_mathaei"
    sp <- get_sp[get_sp$espece == x, ]
    stat.test <- sp |>
      rstatix::wilcox_test(abondance ~ method) |>
      rstatix::add_significance()
   
    bxp <- ggpubr::ggboxplot(sp, x = "method", y = "abondance",
                      fill = "method", palette = c("#868686FF", "#EFC000FF"),
                      xlab = "Méthode", ylab = "Abondance") +
      ggplot2::theme(legend.position = "none") +
      ggplot2::ggtitle(gsub("_", " ", tools::toTitleCase(x)))
    stat.test <- stat.test |> rstatix::add_xy_position(x = "method")
    bxp +
      ggpubr::stat_pvalue_manual(stat.test, label = "Wilcoxon-test, p = {p}",
                                 bracket.nudge.y = 1)
    
  })
  
  cowplot::plot_grid(boxplot[[1]], boxplot[[2]], align = "hv")
  
}
