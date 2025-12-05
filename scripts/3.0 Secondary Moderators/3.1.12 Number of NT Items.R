
# SAME PROBE --------------------------------------------------------------
# SP NT UNC Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall', nt_items != 'n_g') %>%
  mutate_at(vars(nt_items), as.numeric) %>%
  mutate(cent_ntitems = scale(nt_items)[,1])

nt_sp_unc_ntitems <- brm(yi | se(sei) ~ 1 + cent_ntitems + (1|id) + (1|es_id), backend='cmdstanr',
                         data = temp, iter = 20000, warmup = 5000, cores = 8, 
                         prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                         file = 'models/nt_sp_unc_ntitems', file_refit = "on_change")

hypothesis(nt_sp_unc_ntitems, 'cent_ntitems < 0')

# SP NT CON Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall', nt_items != 'n_g') %>%
  mutate_at(vars(nt_items), as.numeric) %>%
  mutate(cent_ntitems = scale(nt_items)[,1])

nt_sp_con_ntitems <- brm(yi | se(sei) ~ 1 + cent_ntitems + (1|id) + (1|es_id), backend='cmdstanr',
                         data = temp, iter = 20000, warmup = 5000, cores = 8, 
                         prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                         file = 'models/nt_sp_con_ntitems', file_refit = "on_change")

hypothesis(nt_sp_con_ntitems, 'cent_ntitems < 0')

# INDEPENDENT PROBE -------------------------------------------------------
# IP NT UNC Base Model --------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall', nt_items != 'n_g') %>%
  mutate_at(vars(nt_items), as.numeric) %>%
  mutate(cent_ntitems = scale(nt_items)[,1])

nt_ip_unc_ntitems <- brm(yi | se(sei) ~ 1 + cent_ntitems + (1|id) + (1|es_id), backend='cmdstanr',
                         data = temp, iter = 20000, warmup = 5000, cores = 8, 
                         prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                         file = 'models/nt_ip_unc_ntitems', file_refit = "on_change")

hypothesis(nt_ip_unc_ntitems, 'cent_ntitems < 0')

# IP NT CON Base Model --------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall', nt_items != 'n_g') %>%
  mutate_at(vars(nt_items), as.numeric) %>%
  mutate(cent_ntitems = scale(nt_items)[,1])

nt_ip_con_ntitems <- brm(yi | se(sei) ~ 1 + cent_ntitems + (1|id) + (1|es_id), backend='cmdstanr',
                         data = temp, iter = 20000, warmup = 5000, cores = 8, 
                         prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                         file = 'models/nt_ip_con_ntitems', file_refit = "on_change")

hypothesis(nt_ip_con_ntitems, 'cent_ntitems < 0')
