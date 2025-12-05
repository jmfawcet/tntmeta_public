
source('scripts/1.0 Process Data.R')

# Exclude Clinical Controls -----------------------------------------------

nt_sp_dat = nt_sp_dat %>% filter(include==1)
nt_ip_dat = nt_ip_dat %>% filter(include==1)

# SAME PROBE --------------------------------------------------------------
# SP NT UNC Instruction Model Recall --------------------------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall') %>%
  mutate(nt_instruct = relevel(factor(nt_instruct), 'undifferentiated'))

nt_sp_unc_inst_cat <- brm(yi | se(sei) ~ nt_instruct - 1 + (1|id) + (1|es_id), 
                          backend='cmdstanr',
                          data = temp, iter = 20000, warmup = 10000, cores = 8,
                          prior=c(rm_cat_priors), control=list(adapt_delta=.99, max_treedepth=15),
                          file='models/nt_sp_unc_inst_cat', file_refit = "on_change")

nt_sp_unc_inst_cat_em = emmeans(nt_sp_unc_inst_cat, ~ nt_instruct)
pairs(nt_sp_unc_inst_cat_em)

hypothesis(nt_sp_unc_inst_cat, 'nt_instructdirectsuppression - nt_instructundifferentiated> 0')
hypothesis(nt_sp_unc_inst_cat, 'nt_instructthoughtsubstitution - nt_instructundifferentiated > 0')

hypothesis(nt_sp_unc_inst_cat, 'nt_instructdirectsuppression - nt_instructundifferentiated = 0')
hypothesis(nt_sp_unc_inst_cat, 'nt_instructthoughtsubstitution - nt_instructundifferentiated = 0')


# SP NT UNC Instruction Model (Substitutes) Recall --------------------------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall', !is.na(t_s_instruction)) %>%
  mutate(t_s_instruction = relevel(factor(t_s_instruction), 'undifferentiated'))

nt_sp_unc_inst_ts <- brm(yi | se(sei) ~ t_s_instruction-1 + (1|id) + (1|es_id), 
                         backend='cmdstanr',
                         data = temp, iter = 20000, warmup = 10000, cores = 8,
                         prior=c(rm_cat_priors), control=list(adapt_delta=.99, max_treedepth=15),
                         file = 'models/nt_sp_unc_tsinst', file_refit = "on_change")

nt_sp_unc_inst_ts_em = emmeans(nt_sp_unc_inst_ts, ~ t_s_instruction)
pairs(nt_sp_unc_inst_ts_em)

posterior_summary(posterior_epred(nt_sp_unc_inst_ts, newdata = data.frame(sei=0, t_s_instruction=unique(temp$t_s_instruction), id=9999, es_id=9999), allow_new_levels=TRUE, re.form = NA))
posterior_summary(posterior_epred(nt_sp_unc_inst_ts, newdata = data.frame(sei=0, t_s_instruction=unique(temp$t_s_instruction), id=9999, es_id=9999), allow_new_levels=TRUE))

hypothesis(nt_sp_unc_inst_ts, 't_s_instructiongivensub - t_s_instructionundifferentiated> 0')
hypothesis(nt_sp_unc_inst_ts, 't_s_instructiongivensub - t_s_instructionundifferentiated= 0')

# SP NT CON Instruction Model Recall --------------------------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall') %>%
  mutate(nt_instruct=trimws(nt_instruct)) %>%
  mutate(nt_instruct = relevel(factor(nt_instruct), 'undifferentiated'))

nt_sp_con_inst_cat <- brm(yi | se(sei) ~ nt_instruct - 1 + (1|id) + (1|es_id), backend='cmdstanr',
                      data = temp, iter = 20000, warmup = 10000, cores = 8,
                      prior=c(rm_cat_priors), control=list(adapt_delta=.99, max_treedepth=15),
                      file='models/nt_sp_con_inst', file_refit = "on_change")

nt_sp_con_inst_cat_em = emmeans(nt_sp_con_inst_cat, ~ nt_instruct)
pairs(nt_sp_con_inst_cat_em)

hypothesis(nt_sp_con_inst_cat, 'nt_instructdirectsuppression - nt_instructundifferentiated> 0')
hypothesis(nt_sp_con_inst_cat, 'nt_instructthoughtsubstitution - nt_instructundifferentiated > 0')

hypothesis(nt_sp_con_inst_cat, 'nt_instructdirectsuppression - nt_instructundifferentiated= 0')
hypothesis(nt_sp_con_inst_cat, 'nt_instructthoughtsubstitution - nt_instructundifferentiated = 0')

# SP NT CON Instruction Model (Substitutes) Recall --------------------------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall', !is.na(t_s_instruction)) %>%
  mutate(t_s_instruction = relevel(factor(t_s_instruction), 'undifferentiated'))

nt_sp_con_inst_ts <- brm(yi | se(sei) ~ t_s_instruction-1 + (1|id) + (1|es_id), 
                         backend='cmdstanr',
                         data = temp, iter = 20000, warmup = 10000, cores = 8,
                         prior=c(rm_cat_priors), control=list(adapt_delta=.99, max_treedepth=15),
                         file = 'models/nt_sp_con_tsinst', file_refit = "on_change")

nt_sp_con_inst_ts_em = emmeans(nt_sp_con_inst_ts, ~ t_s_instruction)
pairs(nt_sp_con_inst_ts_em)

posterior_summary(posterior_epred(nt_sp_con_inst_ts, newdata = data.frame(sei=0, t_s_instruction=unique(temp$t_s_instruction), id=9999, es_id=9999), allow_new_levels=TRUE, re.form = NA))
posterior_summary(posterior_epred(nt_sp_con_inst_ts, newdata = data.frame(sei=0, t_s_instruction=unique(temp$t_s_instruction), id=9999, es_id=9999), allow_new_levels=TRUE))

hypothesis(nt_sp_con_inst_ts, 't_s_instructiongivensub - t_s_instructionundifferentiated> 0')
hypothesis(nt_sp_con_inst_ts, 't_s_instructiongivensub - t_s_instructionundifferentiated= 0')

# INDEPENDENT PROBE -------------------------------------------------------
# IP NT UNC Instruction Model --------------------------------------------------------

temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall') %>%
  mutate(nt_instruct = relevel(factor(nt_instruct), 'undifferentiated'))

nt_ip_unc_inst_cat <- brm(yi | se(sei) ~ nt_instruct-1 + (1|id) + (1|es_id), backend='cmdstanr',
                      data = temp, iter = 20000, warmup = 10000, cores = 8,
                      prior=c(rm_cat_priors), control=list(adapt_delta=.99, max_treedepth=15),
                      file='models/nt_ip_unc_inst', file_refit = "on_change")

nt_ip_unc_inst_cat_em = emmeans(nt_ip_unc_inst_cat, ~ nt_instruct)
pairs(nt_ip_unc_inst_cat_em)

hypothesis(nt_ip_unc_inst_cat, 'nt_instructdirectsuppression - nt_instructundifferentiated> 0')
hypothesis(nt_ip_unc_inst_cat, 'nt_instructthoughtsubstitution - nt_instructundifferentiated > 0')

hypothesis(nt_ip_unc_inst_cat, 'nt_instructdirectsuppression - nt_instructundifferentiated= 0')
hypothesis(nt_ip_unc_inst_cat, 'nt_instructthoughtsubstitution - nt_instructundifferentiated = 0')

# IP NT UNC Instruction Model (Substitutes) Recall --------------------------------------------------------

temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall', !is.na(t_s_instruction)) %>%
  mutate(t_s_instruction = relevel(factor(t_s_instruction), 'undifferentiated'))

# Too few studies

# nt_ip_unc_inst_ts <- brm(yi | se(sei) ~ t_s_instruction + (1|id) + (1|es_id), 
#                          backend='cmdstanr',
#                          data = temp, iter = 20000, warmup = 10000, cores = 8,
#                          prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15),
#                          file = 'models/nt_ip_unc_tsinst', file_refit = "on_change")
# 
# hypothesis(nt_ip_unc_inst_ts, 't_s_instructiongivensub > 0')
# 
# posterior_summary(posterior_epred(nt_ip_unc_inst_ts, newdata = data.frame(sei=0, t_s_instruction=unique(temp$t_s_instruction), id=9999, es_id=9999), allow_new_levels=TRUE, re.form = NA))
# posterior_summary(posterior_epred(nt_ip_unc_inst_ts, newdata = data.frame(sei=0, t_s_instruction=unique(temp$t_s_instruction), id=9999, es_id=9999), allow_new_levels=TRUE))


# IP NT CON Instruction Model --------------------------------------------------------

temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall') %>%
  mutate(nt_instruct=trimws(nt_instruct)) %>%
  mutate(nt_instruct = relevel(factor(nt_instruct), 'undifferentiated'))

nt_ip_con_inst_cat <- brm(yi | se(sei) ~ nt_instruct-1 + (1|id) + (1|es_id), backend='cmdstanr',
                      data = temp, iter = 20000, warmup = 10000, cores = 8,
                      prior=c(rm_cat_priors), control=list(adapt_delta=.99, max_treedepth=15),
                      file='models/nt_ip_con_inst', file_refit = "on_change")

nt_ip_con_inst_cat_em = emmeans(nt_ip_con_inst_cat, ~ nt_instruct)
pairs(nt_ip_con_inst_cat_em)

hypothesis(nt_ip_con_inst_cat, 'nt_instructdirectsuppression - nt_instructundifferentiated> 0')
hypothesis(nt_ip_con_inst_cat, 'nt_instructthoughtsubstitution - nt_instructundifferentiated > 0')

hypothesis(nt_ip_con_inst_cat, 'nt_instructdirectsuppression - nt_instructundifferentiated= 0')
hypothesis(nt_ip_con_inst_cat, 'nt_instructthoughtsubstitution - nt_instructundifferentiated = 0')

# IP NT CON Instruction Model (Substitutes) Recall --------------------------------------------------------

temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall', !is.na(t_s_instruction)) %>%
  mutate(t_s_instruction = relevel(factor(t_s_instruction), 'undifferentiated'))

# Too few studies

# nt_ip_con_inst_ts <- brm(yi | se(sei) ~ t_s_instruction + (1|id) + (1|es_id),
#                          backend='cmdstanr',
#                          data = temp, iter = 20000, warmup = 10000, cores = 8,
#                          prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15),
#                          file = 'models/nt_ip_con_tsinst', file_refit = "on_change")
# 
# hypothesis(nt_ip_unc_inst_ts, 't_s_instructiongivensub > 0')
# 
# posterior_summary(posterior_epred(nt_ip_con_inst_ts, newdata = data.frame(sei=0, t_s_instruction=unique(temp$t_s_instruction), id=9999, es_id=9999), allow_new_levels=TRUE, re.form = NA))
# posterior_summary(posterior_epred(nt_ip_con_inst_ts, newdata = data.frame(sei=0, t_s_instruction=unique(temp$t_s_instruction), id=9999, es_id=9999), allow_new_levels=TRUE))
