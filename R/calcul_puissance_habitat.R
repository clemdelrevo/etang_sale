
calcul_power <- function(final_long) {
  
  #targets::tar_load(final_long)
  get_sp <- final_long[final_long$espece %in% c("echinometra_mathaei", "stichopus_chloronotus"), ]
  
  sp <- do.call(rbind, lapply(unique(get_sp$espece), function(specie) {
    
    #specie = "echinometra_mathaei"
    sp <- get_sp[get_sp$espece == specie, ]
    
    stat <- do.call(rbind, lapply(unique(sp$method), function(method) {
      
      #method = "belt"
      df <- sp[sp$method == method, ]
      
      stat_within <- do.call(rbind, lapply(unique(df$habitat), function(hab) {
        
        #hab = "platier_a_colonies_coralliennes"
        sub_df <- df[df$habitat == hab, ]
    
        mean_value <- mean(sub_df$abondance)
        var_value <- var(sub_df$abondance)
        stat <- data.frame(habitat = hab, mean = mean_value, variance = var_value)
        
        return(stat)
        
      }))
        
      stat_within$method <- method
      
      return(stat_within)
      
    }))
    
    stat$specie <- specie
    
    return(stat)
    
  }))
    
  power_sp <- do.call(rbind, lapply(unique(sp$specie), function(specie) {
  
    #specie = "echinometra_mathaei"
    sp <- sp[sp$specie == specie, ]
    
    power_stat <- do.call(rbind, lapply(unique(sp$method), function(method) {
      
      #method = "belt"
      df_method <- sp[sp$method == method, ]
      
      power <- do.call(rbind, lapply(3:10, function(n) {
        
        #n = 4
        p <- power.anova.test(groups = length(df_method$habitat),
                                  between.var = var(df_method$mean),
                                  within.var = mean(df_method$var),
                                  n = n, sig.level = 0.05)

        data.frame(n = n, power = p$power)
   
      }))
      
      power$method <- method
      
      return(power)
      
    }))
    
    power_stat$specie <- specie
    
    return(power_stat)
    
  }))
  
  return(power_sp)
  
}

plot_power <- function(power_sp) {
  
  power_regression <- lapply(unique(power_sp$specie), function(specie) {
  
    #specie = "echinometra_mathaei"
    sub_sp <- power_sp[power_sp$specie == specie, ]
    
    stat <- setNames(lapply(unique(sub_sp$method), function(method) {
      
      #method = "belt"
      m <- sub_sp[sub_sp$method == method, ]
      power_lm <- lm(m$power ~ m$n)
      norm <- shapiro.test(residuals(power_lm))
      homo <- bartlett.test(sub_sp$power ~ sub_sp$n)
      summary <- summary(power_lm)
      
      return(list(m = m,
                  lm = power_lm,
                  norm = norm,
                  homo = homo,
                  summary = summary))
    }), unique(sub_sp$method))
    
    g <- ggplot2::ggplot(sub_sp, ggplot2::aes(x = n, y = power, color = method)) +
      ggplot2::geom_point() +
      ggplot2::geom_line() + 
      ggplot2::xlab("Nombre de réplicat") +
      ggplot2::ggtitle(gsub("_", " ", tools::toTitleCase(specie))) +
      ggplot2::ylab("Puissance") +
      ggplot2::theme_linedraw() + 
      ggplot2::labs(color = "Méthode") + 
      ggplot2::scale_color_manual(values = c("#868686FF", "#EFC000FF")) +
      ggplot2::geom_hline(yintercept = 0.8, color = "red")
    if(specie == "echinometra_mathaei") {
      
      return(g + ggplot2::annotate("text", 
                                 x = 4,
                                 y = 1,
                                 size = 5,
                                 label = paste0("y = ", round(stat$belt$summary$coefficients[2], 4), "x + ", round(stat$belt$summary$coefficients[1], 4)), color = "black") +
               ggplot2::annotate("text", 
                                 x = 7,
                                 y = 1,
                                 size = 5,
                                 label = paste0("y = ", round(stat$fix$summary$coefficients[2], 4), "x + ", round(stat$fix$summary$coefficients[1], 4)), color = "#EFC000FF")
               
      )
    
    } else {return(g)}
    
  })
  
  cowplot::plot_grid(power_regression[[1]], power_regression[[2]])

}


