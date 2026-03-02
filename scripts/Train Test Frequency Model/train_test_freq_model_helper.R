
# Trains frequency model
train_freq_model = function(dt){
  model = glm(formula = clm.count ~ year + 
                        ncd.level.f + 
                        region.grp.f + 
                        prior.claims + 
                        yrs.licensed.f + 
                        nb.rb + 
                        driver.age.f,
              data = dt,
              family = poisson(link="log"),
              offset=log(exposure))
  return(model)
}

# function generates predicted claim counts
generate_freq_preds = function(dt_test, model){
  dt_test[, pred.clm.count := predict(model, newdata=dt_test, type="response")]
  dt_test[, pred.freq := pred.clm.count/exposure]
  return(dt_test)
}

# draw QQ plot based on frequency
draw_freq_qq = function(dt_test, n_bins=6){
  
  # sort the table based on the predicted frequency
  qq_dt = copy(dt_test)
  qq_dt = qq_dt[order(pred.freq)]
  
  # compute the cumulative exposures across the sorted table
  qq_dt[, cum_exp := cumsum(exposure)]
  total_exp = sum(qq_dt$exposure)
  
  # bin the exposures 
  qq_dt[, bin := cut(cum_exp,
                     breaks = seq(0, total_exp, length.out = n_bins + 1),
                     include.lowest = TRUE)]
  
  # compute the aggregate frequency in each bin
  qq_dt = qq_dt[, .(pred.count=sum(pred.clm.count),
                    act.count=sum(clm.count),
                    exp=sum(exposure),
                    pred.freq=sum(pred.clm.count)/sum(exposure),
                    act.freq=sum(clm.count)/sum(exposure)), .(bin)][order(bin)]
  
  # set bin name
  qq_dt[, frequency.bin := as.factor(1:.N)]
  
  # melt qq table before plotting
  qq_melt = melt(qq_dt, id.vars="frequency.bin", measure.vars=c("pred.freq", "act.freq"), 
                 variable.name="freq.type", value.name="freq")
  qq_plt = ggplot(qq_melt, aes(x=frequency.bin, y=freq, color=freq.type)) + geom_point()
  show(qq_plt)
  
  return(qq_dt)
}