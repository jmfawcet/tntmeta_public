
# SAME PROBE --------------------------------------------------------------
# SP NT UNC Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall', cue_duration != 'n_g') %>%
  mutate_at(vars(cue_duration), as.numeric) %>%
  mutate(cent_cueduration = scale(cue_duration))

nt_sp_unc_cue_dur <- brm(yi | se(sei) ~ 1 + cent_cueduration + (1|id) + (1|es_id), backend='cmdstanr',
                         data = temp, iter = 20000, warmup = 5000, cores = 8, 
                         prior=c(rm_priors, rm_b_priors), 
                         file = 'models/nt_sp_unc_cue_dur', file_refit = "on_change")

hypothesis(nt_sp_unc_cue_dur, 'cent_cueduration = 0')
hypothesis(nt_sp_unc_cue_dur, 'cent_cueduration > 0')

# SP NT CON Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall', cue_duration != 'n_g') %>%
  mutate_at(vars(cue_duration), as.numeric) %>%
  mutate(cent_cueduration =  scale(cue_duration))

nt_sp_con_cue_dur <- brm(yi | se(sei) ~ 1 + cent_cueduration + (1|id) + (1|es_id), backend='cmdstanr',
                         data = temp, iter = 20000, warmup = 5000, cores = 8, control = list(adapt_delta=.999),
                         prior=c(rm_priors, rm_b_priors), 
                         file = 'models/nt_sp_con_cue_dur', file_refit = "on_change")

hypothesis(nt_sp_con_cue_dur, 'cent_cueduration = 0')
hypothesis(nt_sp_con_cue_dur, 'cent_cueduration > 0')

# INDEPENDENT PROBE -------------------------------------------------------
# IP NT UNC Base Model --------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall', cue_duration != 'n_g') %>%
  mutate_at(vars(cue_duration), as.numeric) %>%
  mutate(cent_cueduration = scale(cue_duration))

nt_ip_unc_cue_dur <- brm(yi | se(sei) ~ 1 + cent_cueduration + (1|id) + (1|es_id), backend='cmdstanr',
                         data = temp, iter = 20000, warmup = 5000, cores = 8, 
                         prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                         file = 'models/nt_ip_unc_cue_dur', file_refit = "on_change")

hypothesis(nt_ip_unc_cue_dur, 'cent_cueduration = 0')
hypothesis(nt_ip_unc_cue_dur, 'cent_cueduration > 0')

# IP NT CON Base Model --------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall', cue_duration != 'n_g') %>%
  mutate_at(vars(cue_duration), as.numeric) %>%
  mutate(cent_cueduration = scale(cue_duration))

nt_ip_con_cue_dur <- brm(yi | se(sei) ~ 1 + cent_cueduration + (1|id) + (1|es_id), backend='cmdstanr',
                         data = temp, iter = 20000, warmup = 5000, cores = 8, 
                         prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                         file = 'models/nt_ip_con_cue_dur', file_refit = "on_change")

hypothesis(nt_ip_con_cue_dur, 'cent_cueduration = 0')
hypothesis(nt_ip_con_cue_dur, 'cent_cueduration > 0')
