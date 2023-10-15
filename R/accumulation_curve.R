# Merge the two data.frame method

get_accumulation_curve <- function(fix_long, belt_long) {

  #targets::tar_load(fix_long)
  #targets::tar_load(belt_long)
  
  belt_wide <- tidyr::spread(belt_long, key = espece, value = abondance)
  fix_long <- fix_long[!fix_long$habitat == "dar_a_epandage_detritique", ]
  fix_wide <- tidyr::spread(fix_long, key = espece, value = abondance)
  
  final_wide <- rbind(belt_wide, fix_wide)
  
  data_belt <- final_wide[final_wide$method == "belt", ]
  data_fix  <- final_wide[final_wide$method == "fixe", ]

  belt_sp <- data_belt[, 4:24]
  fix_sp  <- data_fix[, 4:24]
  
  belt_list <- lapply(1:1000, function(x) belt_sp[sample(1:nrow(belt_sp), nrow(belt_sp), replace = FALSE), ])
  fix_list  <- lapply(1:1000, function(x) fix_sp[sample(1:nrow(fix_sp), nrow(fix_sp), replace = FALSE), ])
  
   belt_model <- model <- do.call(rbind, lapply(seq_along(belt_list), function(n) {
    
    cumul_belt <- apply(belt_list[[n]], 2, cumsum)
    richesse_belt <- apply(cumul_belt, 1, function(x) sum(x > 0))
    rich <- data.frame(A = seq(0, 1500, by = 100), S = c(0, richesse_belt), method = "belt")
    
    return(rich)
    
  }))
   
   fix_model <- model <- do.call(rbind, lapply(seq_along(fix_list), function(n) {
     
     cumul_fix <- apply(fix_list[[n]], 2, cumsum)
     richesse_fix <- apply(cumul_fix, 1, function(x) sum(x > 0))
     rich <- data.frame(A = seq(0, 1700, by = 100), S = c(0, richesse_fix), method = "fixe")
     
     return(rich)
     
   }))
   
   model <- rbind(belt_model, fix_model)
   
   final_model <- do.call(rbind, lapply(unique(model$method), function(method) {
   
     method_use <- model[model$method == method, ]
     
     r <- do.call(rbind, lapply(unique(method_use$A), function(n) {
       
       #n = 400
       A = method_use[method_use$A == n, ]
       m <- mean(A$S)
       data.frame(A = n, mean = m, method = method)
       
     }))
     
   }))
    
    #cumul_fix  <- apply(fix_sp, 2, cumsum)
  
  # Calcul richness
  #richesse_belt <- apply(cumul_belt, 1, function(x) sum(x > 0))
  #richesse_fix  <- apply(cumul_fix, 1, function(x) sum(x > 0))
  
  #final_data_richness <- rbind(data.frame(A = seq(0, 1500, by = 100), S = c(0, richesse_belt), method = "belt"),
                               #data.frame(A = seq(0, 2200, by = 100), S = c(0, richesse_fix), method = "fix"))
  
  
  # plot accumulation curve
  ggplot2::ggplot(final_model, ggplot2::aes(x = A, y = mean, color = method)) + 
    ggplot2::geom_line(linewidth = 1.5) +
    ggplot2::xlab("Surface (m carré)") +
    ggplot2::ylab("Richesse spécifique") +
    ggplot2::theme_linedraw() + 
    ggplot2::labs(color = "Méthode") + 
    ggplot2::scale_color_manual(values = c("#868686FF", "#EFC000FF")) + 
    ggplot2::ggtitle("Courbe d'accumulation")
    
}

