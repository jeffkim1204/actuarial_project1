
# get train indices
train_test_split = function(dt){
  n = nrow(dt)
  train_index = sample(seq_len(n), size = 0.8 * n)
  return(train_index)
}

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
    
    if(col == "year"){
      # overwrite the year factor levels
      dt[, year := factor(year, levels = c("2013", "2012", "2011", "2010"))]
    }else{
      exp_cnt = dt[, .(ttl_exposure=sum(exposure)), by=col][order(-ttl_exposure)]
      dt[, (col) := factor(get(col), levels = exp_cnt[, get(col)])]
    }
    return(dt)
  }
  
  # loop through categorical data and reset level
  for(cat in cat_cols){
    dt = set_categorical_helper(dt, col=cat)
  }
  return(dt)
}

# One way AIC/Deviance Plots
compute_frequency_aic_dev = function(dt, vars, null_model_vars=NULL){
  N = length(vars)
  aic = numeric(N + 1)
  dev = numeric(N + 1)
  
  # create a null model
  if(is.null(null_model_vars)){
    null_m = glm(clm.count ~ 1, 
                 data=dt, 
                 family=poisson(link="log"),
                 offset=log(exposure))
  }else{
    null_fmla = as.formula(paste("clm.count ~ ", paste(null_model_vars, collapse="+")))
    null_m = glm(formula=null_fmla,
                     data=dt_train,
                     family = poisson(link="log"),
                     offset=log(exposure))
  }
  
  # store aic, deviance of null model 
  aic[1] = null_m$aic
  dev[1] = null_m$deviance
  
  # loop through all single variable models and store AIC/Deviance
  for (i in 1:N){
    if(is.null(null_model_vars)){
      f = as.formula(paste("clm.count ~ ", vars[i]))
    }else{
      model_vars = c(null_model_vars, vars[i])
      f = as.formula(paste("clm.count ~ ", paste(model_vars, collapse="+")))
    }
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

# Bin numeric feature s.t. exposures are approximately uniformly distributed
get_unif_exp_breaks = function(dt, col, n_bins){

  # Bin feature s.t. exposures are approx. uniform
  dt_exp = dt[, .(exp=sum(exposure)), by = .(x = get(col))][order(x)]
  total_exp = sum(dt_exp$exp)
  target_exp = total_exp/n_bins
  dt_exp[, cumexp := cumsum(exp)]
  dt_exp[, bin_id := pmin(n_bins, ceiling(cumexp/target_exp))]
  
  # Create bin edges
  bin_min = dt_exp[, .(left_bin=min(x)), .(bin_id)]
  bin_max = dt_exp[, .(right_bin=max(x)), .(bin_id)]
  bin_edges = merge(bin_min, bin_max, by="bin_id")
  breaks = c(min(bin_edges$left_bin) - 1, sort(unique(bin_edges$right_bin)))
  
  return(breaks)
}

# Turn breaks into bin labels
get_bin_labels = function(breaks, round=FALSE){
  if(round){
    breaks = round(breaks, 2)
  }
  bin_labels = paste0("(", head(breaks, -1), ", ", tail(breaks, -1), "]")
  return(bin_labels)
}

# plot frequency against binned feature
freq_relation = function(dt, bin_col, numeric_factor=FALSE){
  
  # compute frequency by bin 
  freq = dt[, .(clm.count=sum(clm.count),
                exp.count=sum(exposure),
                freq=sum(clm.count)/sum(exposure)),
            by=bin_col][order(get(bin_col))]
  
  if(numeric_factor){
    freq[, (bin_col) := as.numeric(as.character(get(bin_col)))]
    freq = freq[order(get(bin_col))]
  }
  
  # Plot frequency by bin 
  scale_factor <- max(freq$freq) / max(freq$exp.count)
  plt = ggplot(freq, aes(x = get(bin_col))) +
    geom_col(aes(y = exp.count * scale_factor),
             fill = "grey80", alpha = 0.6) +
    geom_line(aes(y = freq, group = 1), color = "blue", linewidth = 1) +
    geom_point(aes(y = freq), color = "blue", size = 3) +
    scale_y_continuous(
      name = "Frequency",
      sec.axis = sec_axis(~ . / scale_factor, name = "Exposure Count")
    ) +
    theme_minimal() +
    labs(x = bin_col)
  show(plt)
  
  return(freq)
}

# construct new feature by binning numeric feature
feature_engineer_numeric = function(dt, col, new_col, breaks, labels){
  dt[, (new_col) := cut(get(col), breaks=breaks, labels=labels, right=TRUE)]
  return(dt)
}

generate_freq_preds = function(dt_test, model){
  dt_test[, pred.clm.count := predict(model, newdata=dt_test, type="response")]
  dt_test[, pred.freq := pred.clm.count/exposure]
  return(dt_test)
}

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

