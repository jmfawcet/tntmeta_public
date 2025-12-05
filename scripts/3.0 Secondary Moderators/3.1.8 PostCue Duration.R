
# Note: Jittered durations are excluded

# SAME PROBE --------------------------------------------------------------
# SP NT UNC Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall', postcue_duration != 'n_g') %>%
  mutate_at(vars(postcue_duration), as.numeric) %>%
  mutate(cent_postcueduration = scale(postcue_duration)[,1])

nt_sp_unc_postcuedur = brm(yi | se(sei) ~ 1 + cent_postcueduration + (1|id) + (1|es_id), backend='cmdstanr',
                            data = temp, iter = 20000, warmup = 5000, cores = 8, 
                            prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                            file = 'models/nt_sp_unc_postcuedur', file_refit = "on_change")

hypothesis(nt_sp_unc_postcuedur, 'cent_postcueduration < 0')

# SP NT CON Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall', postcue_duration != 'n_g') %>%
  mutate_at(vars(postcue_duration), as.numeric) %>%
  mutate(cent_postcueduration = scale(postcue_duration)[,1])

nt_sp_con_postcuedur <- brm(yi | se(sei) ~ 1 + cent_postcueduration + (1|id) + (1|es_id), backend='cmdstanr',
                            data = temp, iter = 20000, warmup = 5000, cores = 8, 
                            prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                            file = 'models/nt_sp_con_postcuedur', file_refit = "on_change")

hypothesis(nt_sp_con_postcuedur, 'cent_postcueduration < 0')

# INDEPENDENT PROBE -------------------------------------------------------
# IP NT UNC Base Model --------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall', postcue_duration != 'n_g') %>%
  mutate_at(vars(postcue_duration), as.numeric) %>%
  mutate(cent_postcueduration = scale(postcue_duration)[,1])

nt_ip_unc_postcuedur <- brm(yi | se(sei) ~ 1 + cent_postcueduration + (1|id) + (1|es_id), backend='cmdstanr',
                            data = temp, iter = 20000, warmup = 5000, cores = 8, 
                            prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                            file = 'models/nt_ip_unc_postcuedur', file_refit = "on_change")

hypothesis(nt_ip_unc_postcuedur, 'cent_postcueduration < 0')

# IP NT CON Base Model --------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall', postcue_duration != 'n_g') %>%
  mutate_at(vars(postcue_duration), as.numeric) %>%
  mutate(cent_postcueduration = scale(postcue_duration)[,1])

nt_ip_con_postcuedur <- brm(yi | se(sei) ~ 1 + cent_postcueduration + (1|id) + (1|es_id), backend='cmdstanr',
                            data = temp, iter = 20000, warmup = 5000, cores = 8, 
                            prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                            file = 'models/nt_ip_con_postcuedur', file_refit = "on_change")

hypothesis(nt_ip_con_postcuedur, 'cent_postcueduration < 0')
