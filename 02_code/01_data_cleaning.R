setwd('/Users/adesh/Documents/phd/courses/01_causal_inference/code/school_policy_mob/01_data/')

library(data.table)

schools <- fread('00_nces_original.csv')
covs <- fread('02_covariates.csv')