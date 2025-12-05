
# SAME PROBE --------------------------------------------------------------
# SP NT UNC Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall', cuing_method != 'n_g')

nt_sp_unc_cuemeth <- brm(yi | se(sei) ~ cuing_method-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                         data = temp, iter = 20000, warmup = 5000, cores = 8,
                         prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                         file = 'models/nt_sp_unc_cuemeth', file_refit = "on_change")

hypothesis(nt_sp_unc_cuemeth, 'cuing_methodprecue                         - cuing_methodconcurrent                        < 0')
hypothesis(nt_sp_unc_cuemeth, 'cuing_methodtrained                                 - cuing_methodconcurrent                        < 0')

# SP NT CON Base Model Recall --------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall', cuing_method != 'n_g')

nt_sp_con_cuemeth <- brm(yi | se(sei) ~ cuing_method-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                         data = temp, iter = 20000, warmup = 5000, cores = 8,
                         prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                         file = 'models/nt_sp_con_cuemeth', file_refit = "on_change")

hypothesis(nt_sp_con_cuemeth, 'cuing_methodprecue                         - cuing_methodconcurrent                        < 0')
hypothesis(nt_sp_con_cuemeth, 'cuing_methodtrained                                 - cuing_methodconcurrent                        < 0')

# INDEPENDENT PROBE -------------------------------------------------------
# IP NT UNC Base Model --------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall', cuing_method != 'n_g')

nt_ip_unc_cuemeth <- brm(yi | se(sei) ~ cuing_method-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                         data = temp, iter = 20000, warmup = 5000, cores = 8,
                         prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                         file = 'models/nt_ip_unc_cuemeth', file_refit = "on_change")

hypothesis(nt_ip_unc_cuemeth, 'cuing_methodprecue                         - cuing_methodconcurrent                        < 0')
hypothesis(nt_ip_unc_cuemeth, 'cuing_methodtrained                                 - cuing_methodconcurrent                        < 0')

# IP NT CON Base Model --------------------------------------------------------
# 
# Not enough data
# 
# temp=nt_ip_dat %>% 
#   filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall', cuing_method != 'n_g')
# 
# nt_ip_con_cuemeth <- brm(yi | se(sei) ~ cuing_method-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
#                          data = temp, iter = 20000, warmup = 5000, cores = 8,
#                          prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
#                          file = 'models/nt_ip_con_cuemeth', file_refit = "on_change")
# 
# temp_em = emmeans(nt_ip_con_cuemeth, ~cuing_method)
# pairs(temp_em)
