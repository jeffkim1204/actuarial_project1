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

#-------------------------------------------------------------------------------
# Functions for feature engineering

# Turn breaks into bin labels
get_bin_labels = function(breaks, round=FALSE){
  if(round){
    breaks = round(breaks, 2)
  }
  bin_labels = paste0("(", head(breaks, -1), ", ", tail(breaks, -1), "]")
  return(bin_labels)
}

# construct new feature by binning numeric feature
feature_engineer_numeric = function(dt, col, new_col, breaks, labels){
  dt[, (new_col) := cut(get(col), breaks=breaks, labels=labels, right=TRUE)]
  return(dt)
}

# Feature engineering for yrs.licensed
engineer_yrs_lic = function(dt){
  
  col = "yrs.licensed"
  new_col = "yrs.licensed.f"
  final_breaks = c(0, 1, 2, 4, 10)
  final_bin_labels = get_bin_labels(breaks=final_breaks)
  dt = feature_engineer_numeric(dt=dt, 
                                col=col, 
                                new_col=new_col, 
                                breaks=final_breaks, 
                                labels=final_bin_labels)
  return(dt)
}

# Feature engineering for driver age
engineer_driver_age = function(dt){
  
  col = "driver.age"
  new_col = "driver.age.f"
  final_breaks = c(17, 25, 35, 50, 65, Inf)
  final_bin_labels = get_bin_labels(breaks=final_breaks)
  dt = feature_engineer_numeric(dt=dt, 
                                col=col, 
                                new_col=new_col, 
                                breaks=final_breaks, 
                                labels=final_bin_labels)
  return(dt)
}

# Feature engineering for ncd.level
engineer_ncd_level = function(dt){
  dt[, ncd.level.f := as.numeric(as.character(ncd.level))]
  return(dt)
}

# Feature engineering for regions
engineer_regions = function(dt, region_map){
  dt = map_region_groups(dt=dt, region_map)
  return(dt)
}

full_feature_engineering = function(dt, region_map){
  dt = engineer_yrs_lic(dt)
  dt = engineer_driver_age(dt)
  dt = engineer_ncd_level(dt)
  dt = engineer_regions(dt, region_map)
  return(dt)
}














