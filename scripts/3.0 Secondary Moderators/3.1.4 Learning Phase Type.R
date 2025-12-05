
# SAME PROBE --------------------------------------------------------------
# SP NT UNC Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall', learning_phase_type != "n_g") %>%
  mutate(learning_phase_type = relevel(factor(learning_phase_type), 'test-cycle'))

nt_sp_unc_lpt = brm(yi | se(sei) ~ learning_phase_type-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                     data = temp, iter = 20000, warmup = 5000, cores = 8,
                     prior = rm_cat_priors,
                     file = 'models/nt_sp_unc_lpt', file_refit = "on_change")

temp_em = emmeans(nt_sp_unc_lpt, ~learning_phase_type)
pairs(temp_em)

hypothesis(nt_sp_unc_lpt, 'learning_phase_typetestMcycle     - learning_phase_typedropMoff        < 0')

# SP NT CON Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall', learning_phase_type != "n_g") %>%
  mutate(learning_phase_type = relevel(factor(learning_phase_type), 'test-cycle'))

nt_sp_con_lpt <- brm(yi | se(sei) ~ learning_phase_type-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                     data = temp, iter = 20000, warmup = 5000, cores = 8,
                     prior = rm_cat_priors,
                     file = 'models/nt_sp_con_lpt', file_refit = "on_change")

temp_em = emmeans(nt_sp_con_lpt, ~learning_phase_type)
pairs(temp_em)

# INDEPENDENT PROBE -------------------------------------------------------
# IP NT UNC Base Model --------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall', learning_phase_type != "n_g") %>%
  mutate(learning_phase_type = relevel(factor(learning_phase_type), 'test-cycle'))

nt_ip_unc_lpt <- brm(yi | se(sei) ~ learning_phase_type-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                     data = temp, iter = 20000, warmup = 5000, cores = 8,
                     prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                     file = 'models/nt_ip_unc_lpt', file_refit = "on_change")

temp_em = emmeans(nt_ip_unc_lpt, ~learning_phase_type)
pairs(temp_em)

hypothesis(nt_ip_unc_lpt, 'learning_phase_typetestMcycle     - learning_phase_typedropMoff        < 0')

# IP NT CON Base Model --------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall', learning_phase_type != "n_g") %>%
  mutate(learning_phase_type = relevel(factor(learning_phase_type), 'test-cycle'))

nt_ip_con_lpt <- brm(yi | se(sei) ~ learning_phase_type-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                     data = temp, iter = 20000, warmup = 5000, cores = 8,
                     prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                     file = 'models/nt_ip_con_lpt', file_refit = "on_change")

temp_em = emmeans(nt_ip_con_lpt, ~learning_phase_type)
pairs(temp_em)
