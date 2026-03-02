# Frequency EDA contains all the analysis for feature engineering and selection
# of the final model form. The final frequency model was selected as follows:
  # clm.count ~ year + ncd.level.f + region.grp.f + prior.claims + yrs.licensed.f + nb.rb + driver.age.f

# Load libraries
library(data.table)
library(here)
library(ggplot2)
library(ggrepel)

project.dir = here()
dt_train = fread(here("data", "train_data.csv"))
dt_test = fread(here("data", "test_data.csv"))
region_map = fread(here("data", "RegionMap.csv"))
region_map[, Region := as.factor(Region)]

# Load scripts
source(here("scripts", "Train Test Frequency Model", "feature_engineering.R"))
source(here("scripts", "Train Test Frequency Model", "train_test_freq_model_helper.R"))

# Global variables
cat_cols = c("year", "nb.rb", "driver.gender", "marital.status",
             "ncd.level", "region", "body.code", "seats", "fuel.type")
num_cols = c("driver.age", "yrs.licensed", "vehicle.age", "vehicle.value",
             "ccm", "hp", "weight", "length", "width", "height", "prior.claims")
cols = c(cat_cols, num_cols)

# Set categorical data as factors 
dt_train = set_categorical_features(dt_train)
dt_test = set_categorical_features(dt_test)

# Set base level to level with most exposures
dt_train = set_categorical_levels(dt_train, cat_cols)

# Feature engineering for train and test data
dt_train = full_feature_engineering(dt=dt_train, region_map=region_map)
dt_test = full_feature_engineering(dt=dt_test, region_map=region_map)

# Train/Test frequency model
freq_model = train_freq_model(dt_train)
preds = generate_freq_preds(dt_test=dt_test, model=freq_model)
draw_freq_qq(dt_test=dt_test, n_bins=10)






