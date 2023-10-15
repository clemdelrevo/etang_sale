get_delta_mean <- function() {

  #targets::tar_load(final_long)
  final_long <- final_long[final_long$espece %in% c("echinometra_mathaei", "stichopus_chloronotus") & !final_long$habitat == "dar_a_epandage_detritique" ,]  
  
  delta_mean <- do.call(rbind, lapply(unique(final_long$espece), function(x) {
    
    #x = "echinometra_mathaei"
    data <- final_long[final_long$espece == x, ]
    data_belt <- data[data$method == "belt", ]
    data_fix  <- data[data$method == "fixe", ]

      delta_curve <- do.call(rbind, lapply(2:15, function(n) {
        
        #n = 13
        select_belt <- do.call(rbind, lapply(1:10000, function(x) data_belt[sample(1:nrow(data_belt), n, replace = FALSE), ]))
        m_belt  <- mean(select_belt$abondance)
        sd_belt <- sd(select_belt$abondance)
        
        select_fix <- do.call(rbind, lapply(1:10000, function(x) data_fix[sample(1:nrow(data_fix), n, replace = FALSE), ]))
        m_fix  <- mean(select_fix$abondance)
        sd_fix <- sd(select_fix$abondance)
        #fix_effectif <- sum(select_fix$abondance)
        
        return(diff)

      }))
    
  }))

  ggplot2::ggplot(delta_mean, ggplot2::aes(x = n, y = diff, color = espece)) +
    ggplot2::geom_line() +
    ggplot2::ylab("Delta") + 
    ggplot2::xlab("Nombre de réplicat") +
    ggplot2::theme_linedraw() + 
    ggplot2::labs(color = "Famille")
  
}

