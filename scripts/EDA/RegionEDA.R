
# fit frequency model on region
fit_region_freq_model = function(dt){
  model_region = glm(
    clm.count ~ region + offset(log(exposure)),
    family = poisson,
    data = dt
  )
  return(model_region)
}

# get model coefficients, std errors, confidence interval
get_conf_int_params = function(region_freq_model){

  # get coefficients + std erros
  coefs = summary(region_freq_model)$coefficients
  dt_model_reg = data.table(
    term  = rownames(coefs),
    beta  = coefs[, "Estimate"],
    se    = coefs[, "Std. Error"]
  )
  
  # get upper/lower bounds of CI
  dt_model_reg = dt_model_reg[term != "(Intercept)"]
  dt_model_reg[, lower := beta - se]
  dt_model_reg[, upper := beta + se]
  dt_model_reg[, rel := exp(beta)]
  dt_model_reg[, lower_rel := exp(lower)]
  dt_model_reg[, upper_rel := exp(upper)]
  dt_model_reg = dt_model_reg[order(-rel)]
  dt_model_reg[ , region := gsub("region", "", dt_model_reg$term)]
  
  return(dt_model_reg)
}

# plot the confidence interval for regional betas
plot_regional_betas = function(dt_model_reg){
  plt_region = ggplot(dt_model_reg, aes(x = reorder(term, rel), y = rel)) +
    geom_point() +
    geom_errorbar(aes(ymin = lower_rel, ymax = upper_rel), width = 0.2) +
    geom_hline(yintercept = 1, linetype = "dashed") +
    coord_flip() +
    labs(title = "Region Relativities with 95% CI",
         x = "Region",
         y = "Relativity") +
    theme_minimal()
  show(plt_region)
}

# map regions to Region Groups
map_region_groups = function(dt, region_map){
  setkey(dt, region)
  setkey(region_map, Region)
  dt[region_map, region.grp.f := i.RegionGrp]
  return(dt)
}






















