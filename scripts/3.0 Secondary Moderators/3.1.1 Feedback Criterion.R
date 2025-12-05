
# SAME PROBE --------------------------------------------------------------
# SP NT UNC Base Model Recall --------------------------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall', feedback_criterion != 'n_g') %>%
  mutate_at(vars(feedback_criterion), as.numeric) %>%
  mutate(cent_criterion = feedback_criterion - 0.50)

nt_sp_unc_crit = brm(yi | se(sei) ~ 1 + cent_criterion + (1|id) + (1|es_id), backend='cmdstanr',
                  data = temp, iter = 20000, warmup = 5000, cores = 8, 
                  prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                  file = 'models/nt_sp_unc_crit', file_refit = "on_change")

hypothesis(nt_sp_unc_crit, 'cent_criterion = 0')
hypothesis(nt_sp_unc_crit, 'cent_criterion > 0')

# SP NT CON Base Model Recall --------------------------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall', feedback_criterion != 'n_g') %>%
  mutate_at(vars(feedback_criterion), as.numeric) %>%
  mutate(cent_criterion = feedback_criterion - 0.50)

nt_sp_con_crit = brm(yi | se(sei) ~ 1 + cent_criterion + (1|id) + (1|es_id), backend='cmdstanr',
                      data = temp, iter = 20000, warmup = 5000, cores = 8, 
                      prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                      file = 'models/nt_sp_con_crit', file_refit = "on_change")

hypothesis(nt_sp_con_crit, 'cent_criterion = 0')
hypothesis(nt_sp_con_crit, 'cent_criterion > 0')

# INDEPENDENT PROBE -------------------------------------------------------
# IP NT UNC Base Model --------------------------------------------------------

temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall', feedback_criterion != 'n_g') %>%
  mutate_at(vars(feedback_criterion), as.numeric) %>%
  mutate(cent_criterion = feedback_criterion - 0.50)

nt_ip_unc_crit <- brm(yi | se(sei) ~ 1 + cent_criterion + (1|id) + (1|es_id), backend='cmdstanr',
                      data = temp, iter = 20000, warmup = 5000, cores = 8, 
                      prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                      file = 'models/nt_ip_unc_crit', file_refit = "on_change")

hypothesis(nt_ip_unc_crit, 'cent_criterion = 0')
hypothesis(nt_ip_unc_crit, 'cent_criterion > 0')

# IP NT CON Base Model --------------------------------------------------------

temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall', feedback_criterion != 'n_g') %>%
  mutate_at(vars(feedback_criterion), as.numeric) %>%
  mutate(cent_criterion = feedback_criterion - 0.50)

nt_ip_con_crit = brm(yi | se(sei) ~ 1 + cent_criterion + (1|id) + (1|es_id), backend='cmdstanr',
                      data = temp, iter = 20000, warmup = 5000, cores = 8, 
                      prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                      file = 'models/nt_ip_con_crit', file_refit = "on_change")

hypothesis(nt_ip_con_crit, 'cent_criterion = 0')
hypothesis(nt_ip_con_crit, 'cent_criterion > 0')
