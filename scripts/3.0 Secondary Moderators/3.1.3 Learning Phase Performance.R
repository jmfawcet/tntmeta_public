
# SAME PROBE --------------------------------------------------------------
# SP NT UNC Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall', learning_phase_performance != 'n_g') %>%
  mutate_at(vars(learning_phase_performance), as.numeric) %>%
  mutate(cent_lpp = learning_phase_performance - 0.50)

nt_sp_unc_lpp = brm(yi | se(sei) ~ 1 + cent_lpp + (1|id) + (1|es_id), backend='cmdstanr',
                     data = temp, iter = 20000, warmup = 5000, cores = 8, 
                     prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                     file = 'models/nt_sp_unc_lpp', file_refit = "on_change")

hypothesis(nt_sp_unc_lpp, 'cent_lpp = 0')
hypothesis(nt_sp_unc_lpp, 'cent_lpp < 0')

# SP NT CON Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall', learning_phase_performance != 'n_g') %>%
  mutate_at(vars(learning_phase_performance), as.numeric) %>%
  mutate(cent_lpp = learning_phase_performance - 0.50)

nt_sp_con_lpp <- brm(yi | se(sei) ~ 1 + cent_lpp + (1|id) + (1|es_id), backend='cmdstanr',
                     data = temp, iter = 20000, warmup = 5000, cores = 8, 
                     prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                     file = 'models/nt_sp_con_lpp', file_refit = "on_change")

hypothesis(nt_sp_con_lpp, 'cent_lpp = 0')
hypothesis(nt_sp_con_lpp, 'cent_lpp < 0')

# INDEPENDENT PROBE -------------------------------------------------------
# IP NT UNC Base Model --------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall', learning_phase_performance != 'n_g') %>%
  mutate_at(vars(learning_phase_performance), as.numeric) %>%
  mutate(cent_lpp = learning_phase_performance - 0.50)

nt_ip_unc_lpp <- brm(yi | se(sei) ~ 1 + cent_lpp + (1|id) + (1|es_id), backend='cmdstanr',
                     data = temp, iter = 20000, warmup = 5000, cores = 8, 
                     prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                     file = 'models/nt_ip_unc_lpp', file_refit = "on_change")

hypothesis(nt_ip_unc_lpp, 'cent_lpp = 0')
hypothesis(nt_ip_unc_lpp, 'cent_lpp < 0')

# IP NT CON Base Model --------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall', learning_phase_performance != 'n_g') %>%
  mutate_at(vars(learning_phase_performance), as.numeric) %>%
  mutate(cent_lpp = learning_phase_performance - 0.50)

nt_ip_con_lpp = brm(yi | se(sei) ~ 1 + cent_lpp + (1|id) + (1|es_id), backend='cmdstanr',
                     data = temp, iter = 20000, warmup = 5000, cores = 8, 
                     prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                     file = 'models/nt_ip_con_lpp', file_refit = "on_change")

hypothesis(nt_ip_con_lpp, 'cent_lpp = 0')
hypothesis(nt_ip_con_lpp, 'cent_lpp < 0')
