# The purpose of this script is to perform EDA, select features, and build 
# the frequency model. This script will inform us of how to perform 
# feature engineering as well. The model formula below was selected as the final model

# Final Model Selection: 
# clm.count ~ year + ncd.level.f + region.grp.f + prior.claims + yrs.licensed.f + nb.rb + driver.age.f

# Load libraries
library(data.table)
library(here)
library(ggplot2)
library(ggrepel)

# If write, then training/test data will be written out
write_test_data = TRUE
project.dir = here()

# Load data
dt = fread(here("data", "sim-modeling-dataset.csv"))
region_map = fread(here("data", "RegionMap.csv"))
region_map[, Region := as.factor(Region)]

# Load helper functions
source(here("scripts", "EDA", "RegionEDA.R"))
source(here("scripts", "EDA", "Frequency EDA Helper.R"))
source(here("scripts", "EDA", "Forward Selection and CrossVal.R"))

# Global variables
cat_cols = c("year", "nb.rb", "driver.gender", "marital.status",
             "ncd.level", "region", "body.code", "seats", "fuel.type")
num_cols = c("driver.age", "yrs.licensed", "vehicle.age", "vehicle.value",
             "ccm", "hp", "weight", "length", "width", "height", "prior.claims")
cols = c(cat_cols, num_cols)

# Set categorical data as factors
dt = set_categorical_features(dt)

# Set levels for categorical data
dt = set_categorical_levels(dt, cat_cols)

# train/test split
train_indices = train_test_split(dt)
dt_train = dt[train_indices]
dt_test = dt[-train_indices]
if(write_test_data){
  write.csv(dt_train, here("data", "train_data.csv"), row.names = FALSE)
  write.csv(dt_test, here("data", "test_data.csv"), row.names = FALSE)
}

#--------------------------------------------------------------------------
# one way frequency models
one_way_dt = compute_frequency_aic_dev(dt_train, vars=cols)

# Plot the AIC-deviance plot
plt = ggplot(one_way_dt, aes(x = AIC, y = Deviance, label = vars)) +
  geom_point() +
  geom_text_repel(max.overlaps = Inf)
show(plt)

#--------------------------------------------------------------------------
# EDA for yrs.licensed

# Check feature
yrs.lic.freq = freq_relation(dt=dt_train, bin_col="yrs.licensed")

# Bin data s.t. exposures are approximately even across bins & plot
breaks = get_unif_exp_breaks(dt=dt_train, col="yrs.licensed", n_bins=6)
bin_labels = get_bin_labels(breaks=breaks)
dt_train = feature_engineer_numeric(dt=dt_train, 
                              col="yrs.licensed", 
                              new_col="yrs.licensed.f", 
                              breaks=breaks, 
                              labels=bin_labels)
yrs.lic.freq = freq_relation(dt=dt_train, bin_col="yrs.licensed.f")

# Final breaks for feature engineering [2, 4] have no difference
final_breaks = c(0, 1, 2, 4, 10)
final_bin_labels = get_bin_labels(breaks=final_breaks)
dt_train = feature_engineer_numeric(dt=dt_train, 
                              col="yrs.licensed", 
                              new_col="yrs.licensed.f", 
                              breaks=final_breaks, 
                              labels=final_bin_labels)
#--------------------------------------------------------------------------
# EDA for drivers.age
col = "driver.age"
new_col = "driver.age.f"

# Check feature
freq = freq_relation(dt=dt_train, bin_col=col)

# Bin data s.t. exposures are approximately even across bins & plot
breaks = get_unif_exp_breaks(dt=dt_train, col=col, n_bins=30)
breaks = c(breaks[1:length(breaks)-1], 75, Inf)
bin_labels = get_bin_labels(breaks=breaks)
dt_train = feature_engineer_numeric(dt=dt_train, 
                              col=col, 
                              new_col=new_col, 
                              breaks=breaks, 
                              labels=bin_labels)
freq = freq_relation(dt=dt_train, bin_col=new_col)

# select final breaks for frequency modeling
final_breaks = c(17, 25, 35, 50, 65, Inf)
final_bin_labels = get_bin_labels(breaks=final_breaks)
dt_train = feature_engineer_numeric(dt=dt_train, 
                              col=col, 
                              new_col=new_col, 
                              breaks=final_breaks, 
                              labels=final_bin_labels)
#--------------------------------------------------------------------------
# ncd.level
col = "ncd.level"
freq = freq_relation(dt=dt_train, bin_col=col, numeric_factor = TRUE)

# Given that the ncd.level is almost monotonically decreasing, it'll be treated as a numeric feature
dt_train[, ncd.level.f := as.numeric(as.character(ncd.level))]

#--------------------------------------------------------------------------
# region 
  # the region seems useful as it lowers deviance, but the AIC doesn't 
  # improve much because there are too many regions. Thus we have to 
  # reduce dimensionality. I'll group regions based on their beta coefficients. 
region_freq_model = fit_region_freq_model(dt_train)
region_freq_model_params = get_conf_int_params(region_freq_model)
plot_regional_betas(region_freq_model_params)
dt_train = map_region_groups(dt=dt_train, region_map)

#--------------------------------------------------------------------------
# fit preliminary frequency model
model_1 = glm(
  clm.count ~ year + region.grp.f + yrs.licensed.f + ncd.level.f + driver.age.f + nb.rb + offset(log(exposure)),
  family = poisson,
  data = dt_train
)
print(summary(model_1))

#---------------------------------------------------------------------------------------------------------------
# Try adding additional variables from vars_not_used to the 5-variable null model
all_vars = c("year", "region.grp.f", "yrs.licensed.f", "ncd.level.f", "driver.age.f", "nb.rb", "prior.claims",
                 "driver.gender", "marital.status", "body.code", "seats", "fuel.type", "vehicle.age", "vehicle.value",
                 "ccm", "hp", "weight", "length", "width", "height")
curr_model_vars = c("year", "region.grp.f", "yrs.licensed.f", "ncd.level.f", "driver.age.f", "nb.rb")
vars_not_used = setdiff(all_vars, curr_model_vars)

# Plot the AIC-deviance plot
one_way_dt = compute_frequency_aic_dev(dt=dt_train, vars=vars_not_used, null_model_vars=curr_model_vars)
plt = ggplot(one_way_dt, aes(x = AIC, y = Deviance, label = vars)) +
  geom_point() +
  geom_text_repel(max.overlaps = Inf)
show(plt)

#---------------------------------------------------------------------------------------------------------------
# Forward Step Algorithm with 10-Fold Cross Validation
  # We can test whether there are any new variables that can contribute to the model
  # Through this we can confirm that prior.claims is the variable to add
all_vars = c("year", "region.grp.f", "yrs.licensed.f", "ncd.level.f", "driver.age.f", "nb.rb", "prior.claims",
             "driver.gender", "marital.status", "body.code", "seats", "fuel.type", "vehicle.age", "vehicle.value",
             "ccm", "hp", "weight", "length", "width", "height")
curr_model_vars = c("year", "region.grp.f", "yrs.licensed.f", "ncd.level.f", "driver.age.f", "nb.rb")
cross_val_result = forward_step_cross_val(dt=dt_train, 
                                          curr_model_vars = curr_model_vars,
                                          all_vars = all_vars)

#---------------------------------------------------------------------------------------------------------------
# Forward Step Algorithm with 10-Fold Cross Validation
  # Let's try the cross validation with just year in the null model
all_vars = c("year", "region.grp.f", "yrs.licensed.f", "ncd.level.f", "driver.age.f", "nb.rb", "prior.claims",
             "driver.gender", "marital.status", "body.code", "seats", "fuel.type", "vehicle.age", "vehicle.value",
             "ccm", "hp", "weight", "length", "width", "height")
curr_model_vars = c("year")

cross_val_result = forward_step_cross_val(dt=dt_train, 
                                          curr_model_vars = curr_model_vars,
                                          all_vars = all_vars)

#---------------------------------------------------------------------------------------------------------------









