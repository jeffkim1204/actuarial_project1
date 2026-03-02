
# Forward Variable Selection + Cross Validation
forward_select_best_var = function(dt, curr_model_vars, vars_not_used){
  
  aic_reduction_ls = c()
  aic_tbl = data.table(vars_not_used)
  
  # train the current model
  curr_fmla = as.formula(paste("clm.count ~ ", paste(curr_model_vars, collapse="+")))
  print(curr_fmla)
  curr_model = glm(formula = curr_fmla,
                   data = dt_train,
                   family = poisson(link="log"),
                   offset=log(exposure))
  
  # loop through variables not used and find the var that reduces AIC the most
  for(var in vars_not_used){
    vars = c(curr_model_vars, var)
    new_fmla = as.formula(paste("clm.count ~ ", paste(vars, collapse="+")))
    new_model = glm(formula = new_fmla,
                    data = dt_train,
                    family = poisson(link="log"),
                    offset=log(exposure))
    aic = AIC(curr_model, new_model)
    aic_reduction = aic$AIC[1] - aic$AIC[2]
    aic_reduction_ls = c(aic_reduction_ls, aic_reduction)
  }
  
  # best var is the one with the greatest reduction in AIC
  aic_tbl[, aic_reduction := aic_reduction_ls]
  aic_tbl = aic_tbl[order(-aic_reduction)]
  best_var = aic_tbl[1, vars_not_used]
  
  return(best_var)
}

cross_val = function(dt, model_fmla, K=10){
  
  # cross validation with new model
  poisson_deviance <- function(y, mu) {
    if (any(mu <= 0, na.rm = TRUE)) stop("mu must be > 0")
    2 * sum(ifelse(y == 0, mu, y * log(y / mu) - (y - mu)))
  }
  poiss_dev_vec = c()
  dt[, fold_id := rep(1:K, length.out = .N)]
  
  # loop through folds & compute Poisson deviance
  for(k in 1:K){
    train = dt[fold_id != k]
    test = dt[fold_id == k]
    model = glm(formula = model_fmla,
                data = train,
                family = poisson(link="log"),
                offset=log(exposure))
    mu = predict(model, newdata=test, type="response")
    y = test$clm.count
    poiss_dev = poisson_deviance(y=y, mu=mu)
    poiss_dev_vec = c(poiss_dev_vec, poiss_dev)
  }
  avg_poiss_dev = mean(poiss_dev_vec)
  print("Poisson Deviance of Model")
  print(avg_poiss_dev)
  return(avg_poiss_dev)
}

forward_step_cross_val = function(dt, curr_model_vars, all_vars){
  
  # declare the current model formula & current deviance
  curr_fmla = as.formula(paste("clm.count ~ ", paste(curr_model_vars, collapse="+")))
  curr_poiss_dev = cross_val(dt_train, curr_fmla)
  
  # instantiate the data vectors
  best_var_vec = c(paste(curr_model_vars, collapse="+"))
  poiss_dev_vec = c(curr_poiss_dev)
  
  # candidate variables
  vars_not_used = setdiff(all_vars, curr_model_vars)
  
  while(length(vars_not_used) > 0){
    
    # identify the best feature to add
    best_var = forward_select_best_var(dt=dt_train, 
                                       curr_model_vars = curr_model_vars,
                                       vars_not_used = vars_not_used)
    
    # add the best feature to the model_var list & remove from vars_not_used
    new_model_vars = c(curr_model_vars, best_var)
    vars_not_used = setdiff(vars_not_used, best_var)
    
    # construct the new model formula and compute the deviance
    new_model_fmla = as.formula(paste("clm.count ~ ", paste(new_model_vars, collapse="+")))
    new_poiss_dev = cross_val(dt_train, new_model_fmla)
    
    # set new_model_vars as curr_model_vars
    curr_model_vars = new_model_vars
    
    # append best variable and deviance
    best_var_vec = c(best_var_vec, best_var)
    poiss_dev_vec = c(poiss_dev_vec, new_poiss_dev)
  }
  
  cross_val_result = data.table(feature=best_var_vec, poiss_dev=poiss_dev_vec)
  return(cross_val_result)
}










