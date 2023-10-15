library(targets)

#targets options
tar_option_set(format = "qs")

# functions and options
tar_source()
options(mc.cores = 5)

# pipeline
list(
  
  ## import data
   tar_target(belt_wide, import_es_belt())
  ,tar_target(fix_wide, import_es_fix())
  
  ## transform to long data
  ,tar_target(belt_long, wrangle_belt(belt_wide))
  ,tar_target(fix_long, wrangle_fix(fix_wide))
  
  ## final data
  ,tar_target(final_long, merge_long_data(belt_long, fix_long, clade))
  ,tar_target(final_wide, return_wide_data(belt_long, fix_wide, belt_wide, final_long))
  
  ## abondance distribution
  ,tar_target(boxplot_sp, get_boxplot_sp(final_long))
  ,tar_target(boxplot_habitat, get_boxplot_habitat(final_long))
  
  ## calcul power of method
  ,tar_target(power_sp, calcul_power(final_long))
  ,tar_target(power_regression, plot_power(power_sp))
  
  ## accumulation curve
  ,tar_target(accumulation_curve, get_accumulation_curve(fix_long, belt_long))
  
)
  
  
