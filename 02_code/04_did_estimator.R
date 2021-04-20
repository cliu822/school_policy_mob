## Difference in Difference by G-computation
library(geepack)
library(dplyr)
library(data.table)
library(ggplot2)

did <- function(data, outcome, exposure, time, cluster) {
  
  # tidy up the names and identify Y, A, W, cluster  
  mod_dat <- data
  mod_dat$outcome <- mod_dat[[outcome]]
  mod_dat$exposure <- mod_dat[[exposure]]
  mod_dat$time <- mod_dat[[time]]
  mod_dat$cluster <- mod_dat[[cluster]]
  
  # create model formula
  cov_names <- setdiff(names(mod_dat), c('outcome', 'exposure', 'time', 'cluster', outcome, exposure, time, cluster))
  mod_formula <- paste0('outcome ~ exposure + time + exposure*time + ', paste(cov_names, collapse = ' + '))

  # fit empirical model
  # qmod <- geeglm(as.formula(mod_formula),
  #  data = mod_dat, id = cluster, family = poisson(link = "log"), corstr = "independence")
  
  qmod <- glm(as.formula(mod_formula), family = poisson(link = "log"), data = mod_dat)
  
  # create counterfactual datasets
  md_11 <- mod_dat %>%
    mutate(exposure = 1, time = 1)
  md_10 <- mod_dat %>%
    mutate(exposure = 1, time = 0)
  md_01 <- mod_dat %>%
    mutate(exposure = 0, time = 1)
  md_00 <- mod_dat %>%
    mutate(exposure = 0, time = 0)
  
  # predict out counterfactuals
  qbar11 <- mean(predict(qmod, newdata = md_11, type = 'response'))
  qbar10 <- mean(predict(qmod, newdata = md_10, type = 'response'))
  qbar01 <- mean(predict(qmod, newdata = md_01, type = 'response'))
  qbar00 <- mean(predict(qmod, newdata = md_00, type = 'response'))
  
  # estimate did
  return(qbar11 - qbar10 - qbar01 + qbar00)
}

###############################################################
###############################################################

# load & clean data
## outcome
mod_dat_0 <- readRDS('/Users/adesh/Documents/phd/courses/01_causal_inference/code/school_policy_mob/01_data/mobs_ne_county.RDS')
### subset to relevant columns
mod_dat <- dplyr::select(mod_dat, FIPS, pre, post)
## exposure and covariates
exp_dat_0 <- fread('/Users/adesh/Documents/phd/courses/01_causal_inference/code/school_policy_mob/05_final2.csv')
### 0: online only, 1: any in person
exp_dat <- distinct(dplyr::select(exp_dat_0, FIPS, ST_ABBR, status_county, ses = V3_THEME1, 
                minority = V3_THEME2, housing = V3_THEME3, 
                epi = V3_THEME4, healthcare = V3_THEME5,
                high_risk = V3_THEME6, 
                pop_dens = V3_THEME7))
exp_dat$FIPS <- ifelse(exp_dat$ST_ABBR == 'AL', as.character(paste0('0', exp_dat$FIPS)), as.character(exp_dat$FIPS))
exp_dat <- dplyr::select(exp_dat, -ST_ABBR)

###############################################################
###############################################################

## Create a dataset long on time
### Exposure
exp_dat_inperson <- filter(exp_dat, status_county == 1)
exp_dat_inperson_0 <- exp_dat_inperson %>%
  mutate(exposure = 0, time = 0)
exp_dat_inperson_1 <- exp_dat_inperson %>%
  mutate(exposure = 1, time = 1)

exp_dat_remote <- filter(exp_dat, status_county == 0)
exp_dat_remote_0 <- exp_dat_remote %>%
  mutate(exposure = 0, time = 0)
exp_dat_remote_1 <- exp_dat_remote %>%
  mutate(exposure = 0, time = 1)
exp_dat_final <- bind_rows(exp_dat_inperson_0, exp_dat_inperson_1, 
                           exp_dat_remote_1, exp_dat_remote_0)

### Outcome
mod_dat_final <- pivot_longer(mod_dat, cols = c('pre', 'post'), 
                              names_to = 'time', values_to = 'outcome')
mod_dat_final$time <- ifelse(mod_dat_final$time == 'pre', 0, 1)

## analytical dataset
analysis_dat <- left_join(mod_dat_final, exp_dat_final) %>%
  dplyr::select(-status_county)
glimpse(analysis_dat)

###############################################################
###############################################################
debug(did)
did(data = analysis_dat, exposure = 'exposure', outcome = 'outcome', time = 'time', cluster = 'FIPS')

set.seed(89247)
did_boot <- c()
for (i in 1:1000) {
  boot_dat <- analysis_dat[sample(1:nrow(analysis_dat), size = nrow(analysis_dat), replace = T),]
  did_boot[i] <- did(data = boot_dat, 
                     exposure = 'exposure', outcome = 'outcome', time = 'time', 
                     cluster = 'FIPS')
}

ggplot(as.data.frame(did_boot)) +
  geom_density(aes(did_boot)) +
  geom_vline(xintercept = mean(did_boot), col = 'blue')

quantile(did_boot, probs = c(0.025, 0.975))
