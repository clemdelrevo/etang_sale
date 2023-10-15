plot_abondance <- function(belt_long, fix_long) {
  
  
  targets::tar_load(belt_long)
  targets::tar_load(fix_long)
  
  long_data <- rbind(belt_long, fix_long)
  
  family <- ggplot2::ggplot(long_data, ggplot2::aes(x = espece, y = abondance, fill = espece)) +
    ggplot2::geom_boxplot() + 
    #ggplot2::scale_fill_manual(values = c("#868686FF", "#EFC000FF")) +
    ggplot2::theme_linedraw() +
    #ggplot2::labs(fill = "Méthode") +
    ggplot2::ylab("Abondance") +
    ggplot2::xlab("") +
    ggplot2::theme(legend.position = "none",
                   axis.text.x = ggplot2::element_text(angle = 90))
  
  genre <- ggplot2::ggplot(final_long, ggplot2::aes(x = as.factor(genre), y = abondance)) +
    ggplot2::geom_boxplot(ggplot2::aes(fill = method))+
    ggplot2::facet_wrap(~family, ncol = 1)
  
  f_hab <- ggplot2::ggplot(final_long, ggplot2::aes(x = as.factor(family), y = abondance)) +
    ggplot2::geom_boxplot(ggplot2::aes(fill = method))+
    ggplot2::facet_wrap(~habitat, ncol = 2)
  
  g_hab <- ggplot2::ggplot(final_long, ggplot2::aes(x = as.factor(genre), y = abondance)) +
    ggplot2::geom_boxplot(ggplot2::aes(fill = method))+
    ggplot2::facet_wrap(~habitat, ncol = 2)
  
  
}

