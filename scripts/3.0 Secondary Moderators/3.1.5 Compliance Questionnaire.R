
# SAME PROBE --------------------------------------------------------------
# SP NT UNC Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall') %>%
  mutate(compliance_q = ifelse(compliance_q == 'n_g', 'no', compliance_q))

nt_sp_unc_comp <- brm(yi | se(sei) ~ compliance_q-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                      data = temp, iter = 20000, warmup = 5000, cores = 8,
                      prior = rm_cat_priors,
                      file = 'models/nt_sp_unc_comp', file_refit = "on_change")

hypothesis(nt_sp_unc_comp, 'compliance_qno           - compliance_qyes             = 0')
hypothesis(nt_sp_unc_comp, 'compliance_qno           - compliance_qyes             < 0')

# SP NT CON Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall') %>%
  mutate(compliance_q = ifelse(compliance_q == 'n_g', 'no', compliance_q))

nt_sp_con_comp <- brm(yi | se(sei) ~ compliance_q-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                      data = temp, iter = 20000, warmup = 5000, cores = 8,
                      prior = rm_cat_priors,
                      file = 'models/nt_sp_con_comp', file_refit = "on_change")

hypothesis(nt_sp_con_comp, 'compliance_qno           - compliance_qyes             = 0')
hypothesis(nt_sp_con_comp, 'compliance_qno           - compliance_qyes             < 0')


# INDEPENDENT PROBE -------------------------------------------------------
# IP NT UNC Base Model --------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall') %>%
  mutate(compliance_q = ifelse(compliance_q == 'n_g', 'no', compliance_q))

nt_ip_unc_comp <- brm(yi | se(sei) ~ compliance_q-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                      data = temp, iter = 20000, warmup = 5000, cores = 8,
                      prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                      file = 'models/nt_ip_unc_comp', file_refit = "on_change")

hypothesis(nt_ip_unc_comp, 'compliance_qno           - compliance_qyes             = 0')
hypothesis(nt_ip_unc_comp, 'compliance_qno           - compliance_qyes             < 0')

# IP NT CON Base Model --------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall') %>%
  mutate(compliance_q = ifelse(compliance_q == 'n_g', 'no', compliance_q))

nt_ip_con_comp <- brm(yi | se(sei) ~ compliance_q-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                      data = temp, iter = 20000, warmup = 5000, cores = 8,
                      prior = rm_cat_priors, control=list(adapt_delta=.999, max_treedepth=15),
                      file = 'models/nt_ip_con_comp', file_refit = "on_change")

hypothesis(nt_ip_con_comp, 'compliance_qno           - compliance_qyes             = 0')
hypothesis(nt_ip_con_comp, 'compliance_qno           - compliance_qyes             < 0')
