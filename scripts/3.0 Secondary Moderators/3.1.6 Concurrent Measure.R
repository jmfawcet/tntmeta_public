
# SAME PROBE --------------------------------------------------------------
# SP NT UNC Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall') %>%
  mutate(concurrent_measures = ifelse(concurrent_measures != 'None', 'Yes', 'No'))

nt_sp_unc_concur = brm(yi | se(sei) ~ concurrent_measures-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                        data = temp, iter = 20000, warmup = 5000, cores = 8,
                        prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                        file = 'models/nt_sp_unc_concur', file_refit = "on_change")

hypothesis(nt_sp_unc_concur, 'concurrent_measuresYes                - concurrent_measuresNo                   = 0')
hypothesis(nt_sp_unc_concur, 'concurrent_measuresYes                - concurrent_measuresNo                   < 0')

# SP NT CON Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall') %>%
  mutate(concurrent_measures = ifelse(concurrent_measures != 'None', 'Yes', 'No'))

nt_sp_con_concur <- brm(yi | se(sei) ~ concurrent_measures-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                        data = temp, iter = 20000, warmup = 5000, cores = 8,
                        prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                        file = 'models/nt_sp_con_concur', file_refit = "on_change")

hypothesis(nt_sp_con_concur, 'concurrent_measuresYes                - concurrent_measuresNo                   = 0')
hypothesis(nt_sp_con_concur, 'concurrent_measuresYes                - concurrent_measuresNo                   < 0')

# INDEPENDENT PROBE -------------------------------------------------------
# IP NT UNC Base Model --------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall') %>%
  mutate(concurrent_measures = ifelse(concurrent_measures != 'None', 'Yes', 'No'))

nt_ip_unc_concur = brm(yi | se(sei) ~ concurrent_measures-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                        data = temp, iter = 20000, warmup = 5000, cores = 8,
                        prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                        file = 'models/nt_ip_unc_concur', file_refit = "on_change")

hypothesis(nt_ip_unc_concur, 'concurrent_measuresYes                - concurrent_measuresNo                   = 0')
hypothesis(nt_ip_unc_concur, 'concurrent_measuresYes                - concurrent_measuresNo                   < 0')

# IP NT CON Base Model --------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall') %>%
  mutate(concurrent_measures = ifelse(concurrent_measures != 'None', 'Yes', 'No'))

nt_ip_con_concur <- brm(yi | se(sei) ~ concurrent_measures-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                        data = temp, iter = 20000, warmup = 5000, cores = 8,
                        prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                        file = 'models/nt_ip_con_concur', file_refit = "on_change")

hypothesis(nt_ip_con_concur, 'concurrent_measuresYes                - concurrent_measuresNo                   = 0')
hypothesis(nt_ip_con_concur, 'concurrent_measuresYes                - concurrent_measuresNo                   < 0')
