power_method <- function(final_long) {
  
  #targets::tar_load(final_long)
  get_sp <- final_long[final_long$espece %in% c("echinometra_mathaei", "stichopus_chloronotus"), ]
  
  sp <- do.call(rbind, lapply(unique(get_sp$espece), function(specie) {
    
    #specie = "stichopus_chloronotus"
    sp <- get_sp[get_sp$espece == specie, ]
    
    stat <- do.call(rbind, lapply(unique(sp$method), function(method) {
      
        #method = "belt"
        df <- sp[sp$method == method, ]
        mean_value <- mean(df$abondance)
        sd_value <- sd(df$abondance)
        stat <- data.frame(specie = specie, mean = mean_value, sd = sd_value)
        stat$method <- method
        
        return(stat)
    
    }))
  
  return(stat)
    
  }))
  
  power_method <- setNames(lapply(unique(sp$specie), function(x) {
    
    #x = "stichopus_chloronotus"
    data <- sp[sp$specie == x, ]
    sdpool <- sqrt((data$sd[data$method == "fixe"]^2 + data$sd[data$method == "belt"]^2) / 2)
    d <- (data$mean[data$method == "fixe"] - data$mean[data$method == "belt"]) / sdpool
    p.t.two <- pwr::pwr.t.test(d = d, power = 0.8, type = "two.sample", alternative = "two.sided")
    plot(p.t.two, main = gsub("_", " ", tools::toTitleCase(x)))
    
  }), unique(sp$specie))
  
  return(power_method)
  
}
