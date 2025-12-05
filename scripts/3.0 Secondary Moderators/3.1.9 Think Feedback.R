
# SAME PROBE --------------------------------------------------------------
# SP NT UNC Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall') %>%
  mutate(t_feedback = ifelse(t_feedback == 'n_g', 'no', t_feedback))

nt_sp_unc_tfeed <- brm(yi | se(sei) ~ t_feedback - 1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                       data = temp, iter = 10000, warmup = 5000, cores = 8,
                       prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                       file = 'models/nt_sp_unc_tfeed', file_refit = "on_change")

hypothesis(nt_sp_unc_tfeed, 't_feedbackyes                              - t_feedbackno                              > 0')

# SP NT CON Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall') %>%
  mutate(t_feedback = ifelse(t_feedback == 'n_g', 'no', t_feedback))

nt_sp_con_tfeed <- brm(yi | se(sei) ~ t_feedback - 1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                       data = temp, iter = 10000, warmup = 5000, cores = 8,
                       prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                       file = 'models/nt_sp_con_tfeed', file_refit = "on_change")

hypothesis(nt_sp_con_tfeed, 't_feedbackyes                              - t_feedbackno                              > 0')

# INDEPENDENT PROBE -------------------------------------------------------
# IP NT UNC Base Model --------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall') %>%
  mutate(t_feedback = ifelse(t_feedback == 'n_g', 'no', t_feedback))

nt_ip_unc_tfeed <- brm(yi | se(sei) ~ t_feedback - 1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                       data = temp, iter = 10000, warmup = 5000, cores = 8,
                       prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                       file = 'models/nt_ip_unc_tfeed', file_refit = "on_change")

hypothesis(nt_ip_unc_tfeed, 't_feedbackyes                              - t_feedbackno                              > 0')

# IP NT CON Base Model --------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall') %>%
  mutate(t_feedback = ifelse(t_feedback == 'n_g', 'no', t_feedback))

nt_ip_con_tfeed <- brm(yi | se(sei) ~ t_feedback - 1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                       data = temp, iter = 10000, warmup = 5000, cores = 8,
                       prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                       file = 'models/nt_ip_con_tfeed', file_refit = "on_change")

hypothesis(nt_ip_con_tfeed, 't_feedbackyes                              - t_feedbackno                              > 0')

