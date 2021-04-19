# Set up environment
setwd('/Users/adesh/Documents/phd/courses/01_causal_inference/code/school_policy_mob/01_data/')
library(data.table)
library(dplyr)

# Read in data
schools_orig <- fread('00_nces_original.csv')
schl_open <- fread('01_school_open.csv')
covs_orig <- fread('02_covariates.csv')

# Select merging variables and rename appropriately in NCES location data
schools <- schools_orig %>%
  dplyr::select(DistrictNCES = LEAID, FIPS = CNTY, NAME, NMCBSA, CBSA, CSA)

# Validity checks to ensure no duplicate variable names to prevent merging issues
length(names(schools)) == (length(setdiff(names(schools), names(schl_open))) + 1)
length(names(schools)) == (length(setdiff(names(schools), names(covs_orig))) + 1)

# Merge and subset to create final datasets
final_data <- left_join(left_join(schools, covs_orig), schl_open)
final_data_se <- filter(final_data, ST_ABBR %in% c('AL', 'GA', 'LA', 'MS', 'SC'))

# Output datasets
write.csv(final_data, '04_final_data.csv')
write.csv(final_data_se, '04_final_data_se.csv')
