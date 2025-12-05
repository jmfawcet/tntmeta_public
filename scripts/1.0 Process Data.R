
# Load libraries and functions --------------------------------------------

source('scripts/0.0 Libraries and Functions.R')

# Load data ---------------------------------------------------------------

# Read the data
tnt_meta_dat = read.xlsx('data/tnt_meta_final_clean.xlsx')

included_dv = c('recall', 'recognition') # Excludes Reverse Cued Recall

# Prepare NT SP Data ------------------------------------------------------

nt_sp_dat = tnt_meta_dat %>%
  filter(test_type=='SP', condition=='NT') %>%
  filter(dv %in% included_dv) %>%
  filter(!is.na(exp_m) | !is.na(base_m), n >= 10) %>% # Exclude studies missing means or sample < 10
  mutate(exp_sd = case_when(
    is.na(exp_sd) & !is.na(exp_se) ~ exp_se * (n **.5), # Est. SD from SE if needed
    TRUE ~ exp_sd
  ), base_sd = case_when(
    is.na(base_sd) & !is.na(base_se) ~ base_se * (n **.5), # Est. SD from SE if needed
    TRUE ~ base_sd
  )) %>%
  mutate(t = as.numeric(t), r = case_when(  # Est. r if t and SDs are available but r is not
    is.na(r) & !is.na(t) &
      !is.na(exp_sd) & !is.na(base_sd) &
      !is.na(exp_m) & !is.na(base_m) ~ estimate_r_paired(t, n, exp_m, base_m, exp_sd, base_sd),
    TRUE ~ r
  )) %>%
  mutate(new_id = paste(id, exp, sample_id)) %>%
  mutate(repetitions = as.numeric(repetitions), 
         nt_instruct = ifelse(nt_instruct=='n_g', 'undifferentiated', nt_instruct), # n_g assumed to be undifferentiated
         inc_cond = is_conditionalized!='no') # Included in the conditionalized model

# Figure out the maximal number of repetitions per study
max_reps = group_by(nt_sp_dat, new_id) %>% 
  summarize(max_rep = max(repetitions))
nt_sp_dat = nt_sp_dat %>%
  left_join(max_reps)

# Small meta-analysis to handle missing SDs; conducted using rma for simplicity
# leaving out mean as a predictor as inspection of SD x Mean plots demonstrate
# an association only near boundaries, and predicting without this may mitigate
# the influence of ceiling effects. Also, interesting to note that conditionalization
# is associated with lower variability.
base_sd_temp = escalc(measure = 'SDLN', sdi = base_sd, ni = n, data=nt_sp_dat, append=TRUE)
base_sd_rma = rma.mv(yi, vi, 
                     data=base_sd_temp, 
                     mods=~inc_cond:dv-1,
                     random = list(~ 1 | id, ~ 1 | es_id))
mean_base_sd = predict(base_sd_rma, 
                       newmods = rbind(c(1,0,0,0), c(0,1,0,0), c(0,0,1,0), c(0,0,0,1)), 
                       transf=exp) # Technically estimating median SD here and below due to Jensen's inequality
mean_base_sd = mean_base_sd$pred
names(mean_base_sd) = c('FALSErecall', 'TRUErecall', 'FALSErecognition', 'TRUErecognition')

# Same for experimental conditions
exp_sd_temp = escalc(measure = 'SDLN', sdi = exp_sd, ni = n, data=nt_sp_dat, append=TRUE)
exp_sd_rma = rma.mv(yi, vi, 
                     data=exp_sd_temp, 
                     mods=~inc_cond:dv-1,
                     random = list(~ 1 | id, ~ 1 | es_id))
mean_exp_sd = predict(exp_sd_rma, 
                       newmods = rbind(c(1,0,0,0), c(0,1,0,0), c(0,0,1,0), c(0,0,0,1)), 
                       transf=exp)
mean_exp_sd = mean_exp_sd$pred
names(mean_exp_sd) = c('FALSErecall', 'TRUErecall', 'FALSErecognition', 'TRUErecognition')

# Impute missing r for each condition based on a regression model
r_temp = escalc(measure = 'ZCOR', ri = r, ni = n, data=nt_sp_dat, append=TRUE)
r_rma = rma.mv(yi, vi, 
                    data=r_temp, 
                    mods=~inc_cond:dv-1,
                    random = list(~ 1 | id, ~ 1 | es_id))
mean_r = predict(r_rma, 
                      newmods = rbind(c(1,0,0,0), c(0,1,0,0), c(0,0,1,0), c(0,0,0,1)), 
                      transf=transf.ztor) # Also believe these are median r's

# Store the mean aggregate r value as well as the upper and lower r values based on CI's
# (incase someone wants to run sensitivity analyses)
lower_r = mean_r$ci.lb
upper_r = mean_r$ci.ub
mean_r = mean_r$pred

names(mean_r) = c('FALSErecall', 'TRUErecall', 'FALSErecognition', 'TRUErecognition')
names(lower_r) = c('FALSErecall', 'TRUErecall', 'FALSErecognition', 'TRUErecognition')
names(upper_r) = c('FALSErecall', 'TRUErecall', 'FALSErecognition', 'TRUErecognition')

# Add the imputed SD and correlations into the data
nt_sp_dat = nt_sp_dat %>%
  mutate(exp_sd = case_when(
    is.na(exp_sd) ~ mean_exp_sd[paste0(inc_cond, dv)],
    TRUE ~ exp_sd
  ), base_sd = case_when(
    is.na(base_sd) ~ mean_base_sd[paste0(inc_cond, dv)],
    TRUE ~ base_sd
  ), r = case_when(
    is.na(r) ~ mean_r[paste0(inc_cond, dv)],
    TRUE ~ r
  ))

# Calculate effects (we include code to calculate standardized effects as well, in case someone wants
# to check how our findings hold up using SMCR or SMCC)
nt_sp_dat[c('yi', 'vi')] = escalc(measure = 'MC', m1i = base_m*100, sd1i = base_sd*100, m2i = exp_m*100, sd2i = exp_sd*100, ni = n, ri = r, data=nt_sp_dat, append=FALSE)
nt_sp_dat[c('yi_s', 'vi_s')] = escalc(measure = 'SMCR', m1i = base_m, sd1i = exp_sd, m2i = exp_m, sd2i = base_sd, ni = n, ri = r, data=nt_sp_dat, append=FALSE) # Exp_SD used as sd1i to minimize floor
nt_sp_dat[c('yi_s2', 'vi_s2')] = escalc(measure = 'SMCC', m1i = base_m, sd1i = base_sd, m2i = exp_m, sd2i = exp_sd, ni = n, ri = r, data=nt_sp_dat, append=FALSE)

# Sort the data based on unstandardized effect size, which makes later plotting look good
nt_sp_dat = arrange(nt_sp_dat, yi)

# Creating standard error for brms models
nt_sp_dat = nt_sp_dat %>%
  mutate(sei=vi**.5, sei_s = vi_s**.5, sei_s2 = vi_s2**.5)

# Prepare NT IP Data ------------------------------------------------------

nt_ip_dat = tnt_meta_dat %>%
  filter(test_type=='IP', condition=='NT') %>%
  filter(dv %in% included_dv) %>%
  filter(!is.na(exp_m) | !is.na(base_m), n >= 10) %>%
  mutate(exp_sd = case_when(
    is.na(exp_sd) & !is.na(exp_se) ~ exp_se * (n **.5),
    TRUE ~ exp_sd
  ), base_sd = case_when(
    is.na(base_sd) & !is.na(base_se) ~ base_se * (n **.5),
    TRUE ~ base_sd
  )) %>% 
  mutate(t = as.numeric(t), r = case_when(  # Est. r if t and SDs are available but r is not
    is.na(r) & !is.na(t) &
      !is.na(exp_sd) & !is.na(base_sd) &
      !is.na(exp_m) & !is.na(base_m) ~ estimate_r_paired(t, n, exp_m, base_m, exp_sd, base_sd),
    TRUE ~ r
  )) %>%
  mutate(new_id = paste(id, exp, sample_id)) %>%
  mutate(repetitions = as.numeric(repetitions), 
         nt_instruct = ifelse(nt_instruct=='n_g', 'undifferentiated', nt_instruct), # n_g assumed to be undifferentiated
         inc_cond = is_conditionalized!='no') # Included in the conditionalized model

# Figure out the maximal number of repetitions
max_reps = group_by(nt_ip_dat, new_id) %>% 
  summarize(max_rep = max(repetitions))
nt_ip_dat = nt_ip_dat %>%
  left_join(max_reps)

# Small meta-analysis to handle missing SDs; conducted using rma for simplicity
# leaving out mean as a predictor as inspection of SD x Mean plots demonstrate
# an association only near boundaries, and predicting without this may mitigate
# the influence of ceiling effects. Also, interesting to note that conditionalization
# is associated with lower variability.
base_sd_temp = escalc(measure = 'SDLN', sdi = base_sd, ni = n, data=nt_ip_dat, append=TRUE)
base_sd_rma = rma.mv(yi, vi, 
                     data=base_sd_temp, 
                     mods=~inc_cond-1,
                     random = list(~ 1 | id, ~ 1 | es_id))
mean_base_sd = predict(base_sd_rma, 
                       newmods = rbind(c(1,0), c(0,1)), 
                       transf=exp)
mean_base_sd = mean_base_sd$pred
names(mean_base_sd) = c('FALSE', 'TRUE')


exp_sd_temp = escalc(measure = 'SDLN', sdi = exp_sd, ni = n, data=nt_ip_dat, append=TRUE)
exp_sd_rma = rma.mv(yi, vi, 
                    data=exp_sd_temp, 
                    mods=~inc_cond-1,
                    random = list(~ 1 | id, ~ 1 | es_id))
mean_exp_sd = predict(exp_sd_rma, 
                      newmods = rbind(c(1,0), c(0,1)), 
                      transf=exp)
mean_exp_sd = mean_exp_sd$pred
names(mean_exp_sd) = c('FALSE', 'TRUE')

# Impute missing r for each condition based on a regression model
r_temp = escalc(measure = 'ZCOR', ri = r, ni = n, data=nt_ip_dat, append=TRUE)
r_rma = rma.mv(yi, vi, 
               data=r_temp, 
               mods=~inc_cond-1,
               random = list(~ 1 | id, ~ 1 | es_id))
mean_r = predict(r_rma, 
                 newmods = rbind(c(1,0), c(0,1)), 
                 transf=transf.ztor)
lower_r = mean_r$ci.lb
upper_r = mean_r$ci.ub
mean_r = mean_r$pred

names(mean_r) = c('FALSE', 'TRUE')
names(lower_r) = c('FALSE', 'TRUE')
names(upper_r) = c('FALSE', 'TRUE')


# Add the imputed SD and correlations into the data
nt_ip_dat = nt_ip_dat %>%
  mutate(exp_sd = case_when(
    is.na(exp_sd) ~ mean_exp_sd[as.character(inc_cond)],
    TRUE ~ exp_sd
  ), base_sd = case_when(
    is.na(base_sd) ~ mean_base_sd[as.character(inc_cond)],
    TRUE ~ base_sd
  ), r = case_when(
    is.na(r) ~ mean_r[as.character(inc_cond)],
    TRUE ~ r
  ))


# Calculate effects (once again, including standardized effects in case someone would like to take a look)
nt_ip_dat[c('yi', 'vi')] = escalc(measure = 'MC', m1i = base_m*100, sd1i = base_sd*100, m2i = exp_m*100, sd2i = exp_sd*100, ni = n, ri = r, data=nt_ip_dat, append=FALSE)
nt_ip_dat[c('yi_s', 'vi_s')] = escalc(measure = 'SMCR', m1i = base_m, sd1i = exp_sd, m2i = exp_m, sd2i = base_sd, ni = n, ri = r, data=nt_ip_dat, append=FALSE)  # Exp_SD used as sd1i to minimize floor
nt_ip_dat[c('yi_s2', 'vi_s2')] = escalc(measure = 'SMCC', m1i = base_m, sd1i = base_sd, m2i = exp_m, sd2i = exp_sd, ni = n, ri = r, data=nt_ip_dat, append=FALSE)

nt_ip_dat = arrange(nt_ip_dat, yi)

# Creating standard error for brms models
nt_ip_dat = nt_ip_dat %>%
  mutate(sei=vi**.5, sei_s = vi_s**.5, sei_s2 = vi_s2**.5 )


# Prepare T SP Data ------------------------------------------------------

# Below are the data for the Think analyses, processed similarly to the No-Think analyses
t_sp_dat = tnt_meta_dat %>%
  filter(test_type=='SP', condition=='T') %>%
  filter(dv %in% included_dv) %>%
  filter(!is.na(exp_m) | !is.na(base_m), n >= 10) %>%
  mutate(exp_sd = case_when(
    is.na(exp_sd) & !is.na(exp_se) ~ exp_se * (n **.5),
    TRUE ~ exp_sd
  ), base_sd = case_when(
    is.na(base_sd) & !is.na(base_se) ~ base_se * (n **.5),
    TRUE ~ base_sd
  )) %>%
  mutate(t = as.numeric(t), r = case_when(  # Est. r if t and SDs are available but r is not
    is.na(r) & !is.na(t) &
      !is.na(exp_sd) & !is.na(base_sd) &
      !is.na(exp_m) & !is.na(base_m) ~ estimate_r_paired(t, n, exp_m, base_m, exp_sd, base_sd),
    TRUE ~ r
  )) %>%
  mutate(new_id = paste(id, exp, sample_id)) %>%
  mutate(repetitions = as.numeric(repetitions), 
         nt_instruct = ifelse(nt_instruct=='n_g', 'undifferentiated', nt_instruct), # n_g assumed to be undifferentiated
         inc_cond = is_conditionalized!='no') # Included in the conditionalized model

# Figure out the maximal number of repetitions
max_reps = group_by(t_sp_dat, new_id) %>% 
  summarize(max_rep = max(repetitions))
t_sp_dat = t_sp_dat %>%
  left_join(max_reps)

# Small meta-analysis to handle missing SDs; conducted using rma for simplicity
# leaving out mean as a predictor as inspection of SD x Mean plots demonstrate
# an association only near boundaries, and predicting without this may mitigate
# the influence of ceiling effects. Also, interesting to note that conditionalization
# is associated with lower variability.
base_sd_temp = escalc(measure = 'SDLN', sdi = base_sd, ni = n, data=t_sp_dat, append=TRUE)
base_sd_rma = rma.mv(yi, vi, 
                     data=base_sd_temp, 
                     mods=~inc_cond:dv-1,
                     random = list(~ 1 | id, ~ 1 | es_id))
mean_base_sd = predict(base_sd_rma, 
                       newmods = rbind(c(1,0,0,0), c(0,1,0,0), c(0,0,1,0), c(0,0,0,1)), 
                       transf=exp)
mean_base_sd = mean_base_sd$pred
names(mean_base_sd) = c('FALSErecall', 'TRUErecall', 'FALSErecognition', 'TRUErecognition')

# NOTE: Some yi are infinity due to ceiling effects, they are excluded by default
exp_sd_temp = escalc(measure = 'SDLN', sdi = exp_sd, ni = n, data=t_sp_dat, append=TRUE)
exp_sd_rma = rma.mv(yi, vi, 
                    data=exp_sd_temp, 
                    mods=~inc_cond:dv-1,
                    random = list(~ 1 | id, ~ 1 | es_id))
mean_exp_sd = predict(exp_sd_rma, 
                      newmods = rbind(c(1,0,0,0), c(0,1,0,0), c(0,0,1,0), c(0,0,0,1)), 
                      transf=exp)
mean_exp_sd = mean_exp_sd$pred
names(mean_exp_sd) = c('FALSErecall', 'TRUErecall', 'FALSErecognition', 'TRUErecognition')

# Impute missing r for each condition based on a regression model
r_temp = escalc(measure = 'ZCOR', ri = r, ni = n, data=t_sp_dat, append=TRUE)
r_rma = rma.mv(yi, vi, 
               data=r_temp, 
               mods=~inc_cond:dv-1,
               random = list(~ 1 | id, ~ 1 | es_id))
mean_r = predict(r_rma, 
                 newmods = rbind(c(1,0,0,0), c(0,1,0,0), c(0,0,1,0), c(0,0,0,1)), 
                 transf=transf.ztor)
lower_r = mean_r$ci.lb
upper_r = mean_r$ci.ub
mean_r = mean_r$pred

names(mean_r) = c('FALSErecall', 'TRUErecall', 'FALSErecognition', 'TRUErecognition')
names(lower_r) = c('FALSErecall', 'TRUErecall', 'FALSErecognition', 'TRUErecognition')
names(upper_r) = c('FALSErecall', 'TRUErecall', 'FALSErecognition', 'TRUErecognition')

# Arbitrarily setting TRUErecognition to .2 (reflecting recognition that correlations
# are lower for TRUE than FALSE but keeping it close to FALSE recog) owing to
# the fact that there is only 1 recog study with a correlation
mean_r['TRUErecognition'] = .2

# Add the imputed SD and correlations into the data
t_sp_dat = t_sp_dat %>%
  mutate(exp_sd = case_when(
    is.na(exp_sd) ~ mean_exp_sd[paste0(inc_cond, dv)],
    TRUE ~ exp_sd
  ), base_sd = case_when(
    is.na(base_sd) ~ mean_base_sd[paste0(inc_cond, dv)],
    TRUE ~ base_sd
  ), r = case_when(
    is.na(r) ~ mean_r[paste0(inc_cond, dv)],
    TRUE ~ r
  ))

# Calculate effects (again, including standardized effects - here be mindful that ceiling effects are a bigger risk,
# which would contaminate the standardized effects more than unstandardized effects)
t_sp_dat[c('yi', 'vi')] = escalc(measure = 'MC', m1i = exp_m*100, sd1i = exp_sd*100, m2i = base_m*100, sd2i = base_sd*100, ni = n, ri = r, data=t_sp_dat, append=FALSE)
t_sp_dat[c('yi_s', 'vi_s')] = escalc(measure = 'SMCR', m1i = exp_m, sd1i = base_sd, m2i = base_m, sd2i = exp_sd, ni = n, ri = r, data=t_sp_dat, append=FALSE) # Using base_sd for sd1i to avoid Inf
t_sp_dat[c('yi_s2', 'vi_s2')] = escalc(measure = 'SMCC', m1i = exp_m, sd1i = exp_sd, m2i = base_m, sd2i = base_sd, ni = n, ri = r, data=t_sp_dat, append=FALSE)

t_sp_dat = arrange(t_sp_dat, yi)

# Creating sampling error for brms models
t_sp_dat = t_sp_dat %>%
  mutate(sei=vi**.5, sei_s = vi_s**.5, sei_s2 = vi_s2**.5)

# Prepare T IP Data ------------------------------------------------------

t_ip_dat = tnt_meta_dat %>%
  filter(test_type=='IP', condition=='T') %>%
  filter(dv %in% included_dv) %>%
  filter(!is.na(exp_m) | !is.na(base_m), n >= 10) %>%
  mutate(exp_sd = case_when(
    is.na(exp_sd) & !is.na(exp_se) ~ exp_se * (n **.5),
    TRUE ~ exp_sd
  ), base_sd = case_when(
    is.na(base_sd) & !is.na(base_se) ~ base_se * (n **.5),
    TRUE ~ base_sd
  )) %>%
  mutate(t = as.numeric(t), r = case_when(  # Est. r if t and SDs are available but r is not
    is.na(r) & !is.na(t) &
      !is.na(exp_sd) & !is.na(base_sd) &
      !is.na(exp_m) & !is.na(base_m) ~ estimate_r_paired(t, n, exp_m, base_m, exp_sd, base_sd),
    TRUE ~ r
  )) %>%
  mutate(new_id = paste(id, exp, sample_id)) %>%
  mutate(repetitions = as.numeric(repetitions), 
         nt_instruct = ifelse(nt_instruct=='n_g', 'undifferentiated', nt_instruct), # n_g assumed to be undifferentiated
         inc_cond = is_conditionalized!='no') # Included in the conditionalized model

# Figure out the maximal number of repetitions
max_reps = group_by(t_ip_dat, new_id) %>% 
  summarize(max_rep = max(repetitions))
t_ip_dat = t_ip_dat %>%
  left_join(max_reps)

# Small meta-analysis to handle missing SDs; conducted using rma for simplicity
# leaving out mean as a predictor as inspection of SD x Mean plots demonstrate
# an association only near boundaries, and predicting without this may mitigate
# the influence of ceiling effects. Also, interesting to note that conditionalization
# is associated with lower variability.
base_sd_temp = escalc(measure = 'SDLN', sdi = base_sd, ni = n, data=t_ip_dat, append=TRUE)
base_sd_rma = rma.mv(yi, vi, 
                     data=base_sd_temp, 
                     mods=~inc_cond-1,
                     random = list(~ 1 | id, ~ 1 | es_id))
mean_base_sd = predict(base_sd_rma, 
                       newmods = rbind(c(1,0), c(0,1)), 
                       transf=exp)
mean_base_sd = mean_base_sd$pred
names(mean_base_sd) = c('FALSE', 'TRUE')

exp_sd_temp = escalc(measure = 'SDLN', sdi = exp_sd, ni = n, data=t_ip_dat, append=TRUE)
exp_sd_rma = rma.mv(yi, vi, 
                    data=exp_sd_temp, 
                    mods=~inc_cond-1,
                    random = list(~ 1 | id, ~ 1 | es_id))
mean_exp_sd = predict(exp_sd_rma, 
                      newmods = rbind(c(1,0), c(0,1)), 
                      transf=exp)
mean_exp_sd = mean_exp_sd$pred
names(mean_exp_sd) = c('FALSE', 'TRUE')

# Impute missing r for each condition based on a regression model
r_temp = escalc(measure = 'ZCOR', ri = r, ni = n, data=t_ip_dat, append=TRUE)
r_rma = rma.mv(yi, vi, 
               data=r_temp, 
               mods=~inc_cond-1,
               random = list(~ 1 | id, ~ 1 | es_id))
mean_r = predict(r_rma, 
                 newmods = rbind(c(1,0), c(0,1)), 
                 transf=transf.ztor)
lower_r = mean_r$ci.lb
upper_r = mean_r$ci.ub
mean_r = mean_r$pred

names(mean_r) = c('FALSE', 'TRUE')
names(lower_r) = c('FALSE', 'TRUE')
names(upper_r) = c('FALSE', 'TRUE')

# Add the imputed SD and correlations into the data

t_ip_dat = t_ip_dat %>%
  mutate(exp_sd = case_when(
    is.na(exp_sd) ~ mean_exp_sd[as.character(inc_cond)],
    TRUE ~ exp_sd
  ), base_sd = case_when(
    is.na(base_sd) ~ mean_base_sd[as.character(inc_cond)],
    TRUE ~ base_sd
  ), r = case_when(
    is.na(r) ~ mean_r[as.character(inc_cond)],
    TRUE ~ r
  ))

# Calculate effects

t_ip_dat[c('yi', 'vi')] = escalc(measure = 'MC', m1i = exp_m*100, sd1i = exp_sd*100, m2i = base_m*100, sd2i = base_sd*100, ni = n, ri = r, data=t_ip_dat, append=FALSE)
t_ip_dat[c('yi_s', 'vi_s')] = escalc(measure = 'SMCR', m1i = exp_m, sd1i = base_sd, m2i = base_m, sd2i = exp_sd, ni = n, ri = r, data=t_ip_dat, append=FALSE) # Using baseline SD to standardize
t_ip_dat[c('yi_s2', 'vi_s2')] = escalc(measure = 'SMCC', m1i = exp_m, sd1i = exp_sd, m2i = base_m, sd2i = base_sd, ni = n, ri = r, data=t_ip_dat, append=FALSE)

t_ip_dat = arrange(t_ip_dat, yi)

# Creating sampling error for brms models
t_ip_dat = t_ip_dat %>%
  mutate(sei=vi**.5, sei_s = vi_s**.5, sei_s2 = vi_s2**.5 )
