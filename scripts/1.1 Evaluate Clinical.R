
#
# The purpose of this script is to evaluate whether the clinical and non-clinical samples 
# are similar enough in SIF magnitude to meet our pre-registered requirement. They did not,
# so we excluded control samples from clinical studies.
# 
# Also note this script must be run in full! If the models are fit after
# later scripts are run, the clinical control samples may be removed.
# 

# Load the data -----------------------------------------------------------

source('scripts/1.0 Process Data.R')

# Check whether Clinical Controls = Other Studies for SP ------------------

# Unconditionalized
temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall') %>%
  mutate(is_clinical = case_when(
    include == 1 ~ 'no',
    include == 2 ~ 'yes',
    TRUE ~ 'ERROR'
  ))

nt_sp_unc_isclin <- brm(yi | se(sei) ~ is_clinical-1 + (1|id) + (1|es_id),
                         backend='cmdstanr',
                         data = temp, iter = 20000, cores = 8,
                         prior=c(rm_cat_priors), control=list(adapt_delta=.99, max_treedepth=15))
hypothesis(nt_sp_unc_isclin, 'is_clinicalno-is_clinicalyes = 0')

# Conditionalized
temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall') %>%
  mutate(is_clinical = case_when(
    include == 1 ~ 'no',
    include == 2 ~ 'yes',
    TRUE ~ 'ERROR'
  ))

nt_sp_cond_isclin <- brm(yi | se(sei) ~ is_clinical-1 + (1|id) + (1|es_id),
                 backend='cmdstanr',
                 data = temp, iter = 20000, cores = 8,
                 prior=c(rm_cat_priors), control=list(adapt_delta=.99, max_treedepth=15))
hypothesis(nt_sp_cond_isclin, 'is_clinicalno-is_clinicalyes = 0')

# Check whether Clinical Controls = Other Studies for IP ------------------

# Unconditionalized
temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes') %>%
  mutate(is_clinical = case_when(
    include == 1 ~ 'no',
    include == 2 ~ 'yes',
    TRUE ~ 'ERROR'
  ))

nt_ip_unc_isclin <- brm(yi | se(sei) ~ is_clinical-1 + (1|id) + (1|es_id),
                         backend='cmdstanr',
                         data = temp, iter = 20000, cores = 8,
                         prior=c(rm_cat_priors), control=list(adapt_delta=.99, max_treedepth=15))
hypothesis(nt_ip_unc_isclin, 'is_clinicalno-is_clinicalyes = 0')


# Conditionalized
temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no') %>%
  mutate(is_clinical = case_when(
    include == 1 ~ 'no',
    include == 2 ~ 'yes',
    TRUE ~ 'ERROR'
  ))

nt_ip_cond_isclin <- brm(yi | se(sei) ~ is_clinical-1 + (1|id) + (1|es_id),
                         backend='cmdstanr',
                         data = temp, iter = 20000, cores = 8,
                         prior=c(rm_cat_priors), control=list(adapt_delta=.99, max_treedepth=15))
hypothesis(nt_ip_cond_isclin, 'is_clinicalno-is_clinicalyes = 0')


# SP Models estimating the effect ignoring is_clinical -----------------------

# Unconditionalized
temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall') %>%
  mutate(is_clinical = case_when(
    include == 1 ~ 'no',
    include == 2 ~ 'yes',
    TRUE ~ 'ERROR'
  ))

nt_sp_unc_inc_clin <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
                        backend='cmdstanr',
                        data = temp, iter = 20000, cores = 8,
                        prior=c(rm_priors), control=list(adapt_delta=.99, max_treedepth=15))

summarize_agg_and_PI(nt_sp_unc_inc_clin, temp_sd, label = "NT-SP Unconditionalized Recall (Clinical)")

# Conditionalized
temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall') %>%
  mutate(is_clinical = case_when(
    include == 1 ~ 'no',
    include == 2 ~ 'yes',
    TRUE ~ 'ERROR'
  ))

nt_sp_cond_inc_clin <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
                         backend='cmdstanr',
                         data = temp, iter = 20000, cores = 8,
                         prior=c(rm_priors), control=list(adapt_delta=.99, max_treedepth=15))

summarize_agg_and_PI(nt_sp_cond_inc_clin, temp_sd, label = "NT-SP Conditionalized Recall (Clinical)")

# IP Models estimating the effect ignoring is_clinical -----------------------

# Unconditionalized
temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes') %>%
  mutate(is_clinical = case_when(
    include == 1 ~ 'no',
    include == 2 ~ 'yes',
    TRUE ~ 'ERROR'
  ))

nt_ip_unc_inc_clin <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
                        backend='cmdstanr',
                        data = temp, iter = 20000, cores = 8,
                        prior=c(rm_priors), control=list(adapt_delta=.99, max_treedepth=15))

summarize_agg_and_PI(nt_ip_unc_inc_clin, temp_sd, label = "NT-IP Unconditionalized Recall (Clinical)")

# Conditionalized
temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no') %>%
  mutate(is_clinical = case_when(
    include == 1 ~ 'no',
    include == 2 ~ 'yes',
    TRUE ~ 'ERROR'
  ))

nt_ip_cond_inc_clin <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
                         backend='cmdstanr',
                         data = temp, iter = 20000, cores = 8,
                         prior=c(rm_priors), control=list(adapt_delta=.99, max_treedepth=15))

summarize_agg_and_PI(nt_ip_cond_inc_clin, temp_sd, label = "NT-IP Conditionalized Recall (Clinical)")

