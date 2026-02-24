
# function turns categorical features into factors
set_categorical_features = function(dt){
  dt[, (cat_cols) := lapply(.SD, as.factor), .SDcols = cat_cols]
  return(dt)
}

# function sets categorical levels 
# baseline level is category with most exposures
set_categorical_levels = function(dt, cat_cols){
  
  # helper function -> rank categories based on exposure
  set_categorical_helper = function(dt, col){
    exp_cnt = dt[, .(ttl_exposure=sum(exposure)), by=col][order(-ttl_exposure)]
    dt[, (col) := factor(get(col), levels = exp_cnt[, get(col)])]
    return(dt)
  }
  
  # loop through categorical data and reset level
  for(cat in cat_cols){
    dt = set_categorical_helper(dt, col=cat)
  }
  return(dt)
}

# One way AIC/Deviance Plots
compute_frequency_aic_dev = function(dt, vars){
  N = length(vars)
  aic = numeric(N + 1)
  dev = numeric(N + 1)
  
  # create a null model with just the intercept term
  null_m = glm(clm.count ~ 1, 
               data=dt, 
               family=poisson(link="log"),
               offset=log(exposure))
  
  # store aic, deviance of null model 
  aic[1] = null_m$aic
  dev[1] = null_m$deviance
  
  # loop through all single variable models and store AIC/Deviance
  for (i in 1:N){
    f = as.formula(paste("clm.count ~ ", vars[i]))
    m = glm(formula = f,
            data = dt,
            family = poisson(link="log"),
            offset=log(exposure))
    
    aic[i+1] = m$aic
    dev[i+1] = m$deviance
  }
  
  # return AIC and Deviance
  vars = c("NULL", vars)
  one_way_dt = data.table(vars=vars, AIC=aic, Deviance=dev)
  one_way_dt = one_way_dt[order(aic)]

  return(one_way_dt)
}

# plot frequency against binned feature
numeric_freq_relation = function(col, dt, n_bins=10, bin_data=FALSE, breaks=NULL){
  
  # bin numeric feature
  get_breaks = function(col, dt, n_bins){
    x = dt[, get(col)]
    breaks = unique(quantile(x, seq(0, 1, length.out = n_bins + 1)))
    return(breaks)
  }
  
  # if bin_data is TRUE, derive bins and bin data
  if(bin_data){
    if(isnull(breaks)){
      breaks = get_breaks(col, dt, n_bins)
    }
    dt[, bin := cut(get(col), breaks, include.lowest=TRUE)]
  }else{
    dt[, bin := get(col)]
  }
  
  # compute frequency by bin 
  freq = dt[, .(clm.count=sum(clm.count),
                exp.count=sum(exposure),
                freq=sum(clm.count)/sum(exposure)),
            .(bin)][order(bin)]
  
  # Plot frequency by bin 
  scale_factor <- max(freq$freq) / max(freq$exp.count)
  plt = ggplot(freq, aes(x = bin)) +
          geom_col(aes(y = exp.count * scale_factor),
                   fill = "grey80", alpha = 0.6) +
          geom_line(aes(y = freq, group = 1), color = "blue", linewidth = 1) +
          geom_point(aes(y = freq), color = "blue", size = 3) +
          scale_y_continuous(
            name = "Frequency",
            sec.axis = sec_axis(~ . / scale_factor, name = "Exposure Count")
          ) +
          theme_minimal() +
          labs(x = col)
  show(plt)
  
  return(freq)
}

# construct new feature by binning numeric feature
feature_engineer_numeric = function(dt, col, new_col, breaks, labels){
  dt[, (new_col) := cut(get(col), breaks=breaks, labels=labels, right=TRUE)]
  return(dt)
}


















