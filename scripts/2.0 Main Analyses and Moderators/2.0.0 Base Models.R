
source('scripts/1.0 Process Data.R')

# Exclude Clinical Controls -----------------------------------------------

nt_sp_dat = nt_sp_dat %>% filter(include==1)
nt_ip_dat = nt_ip_dat %>% filter(include==1)

# SAME PROBE --------------------------------------------------------------
# SP NT UNC Base Model Recall --------------------------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

nt_sp_unc = brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
               backend='cmdstanr',
               data = temp, iter = 20000, warmup = 10000, cores = 8,
               prior=rm_priors, control=list(adapt_delta=.99, max_treedepth=15),
               file = 'models/nt_sp_unc', file_refit = "on_change")

summarize_agg_and_PI(nt_sp_unc, temp_sd, label = "NT-SP Unconditionalized Recall")

# SP NT UNC Base Model Recall (Student t) --------------------------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

nt_sp_unc_t <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
                 backend='cmdstanr',
                 family=student,
                 data = temp, iter = 20000, warmup = 10000, cores = 8,
                 prior=c(rm_priors, student_t_df_prior), control=list(adapt_delta=.99, max_treedepth=15),
                 file = 'models/nt_sp_unc_t', file_refit = "on_change")

summarize_agg_and_PI(nt_sp_unc_t, temp_sd, label = "NT-SP Unconditionalized Recall (t)")

# SP NT CON Base Model Recall --------------------------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv == 'recall')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

nt_sp_con <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id), backend='cmdstanr',
               data = temp, iter = 20000, warmup = 10000, cores = 8,
               prior=rm_priors, control=list(adapt_delta=.99, max_treedepth=15),
               file = 'models/nt_sp_con', file_refit = "on_change")

summarize_agg_and_PI(nt_sp_con, temp_sd, label = "NT-SP Conditionalized Recall")

# SP NT CON Base Model Recall (Student t) --------------------------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv == 'recall')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

nt_sp_con_t <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id), backend='cmdstanr',
                 family=student,
                 data = temp, iter = 20000, warmup = 10000, cores = 8,
                 prior=c(rm_priors, student_t_df_prior), control=list(adapt_delta=.99, max_treedepth=15),
                 file = 'models/nt_sp_con_t', file_refit = "on_change")

summarize_agg_and_PI(nt_sp_con_t, temp_sd, label = "NT-SP Conditionalized Recall (t)")

# INDEPENDENT PROBE -------------------------------------------------------
# IP NT UNC Base Model --------------------------------------------------------

temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv == 'recall')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

nt_ip_unc <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id), backend='cmdstanr',
                  data = temp, iter = 20000, warmup = 10000, cores = 8,
                  prior=rm_priors, control=list(adapt_delta=.99, max_treedepth=15),
                  file = 'models/nt_ip_unc', file_refit = "on_change")

summarize_agg_and_PI(nt_ip_unc, temp_sd, label = "NT-IP Unconditionalized Recall")

# IP NT UNC Base Model (Student t) --------------------------------------------------------

temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv == 'recall')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

nt_ip_unc_t <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id), backend='cmdstanr',
                 family=student,
                 data = temp, iter = 20000, warmup = 10000, cores = 8,
                 prior=c(rm_priors, student_t_df_prior), control=list(adapt_delta=.99, max_treedepth=15),
                 file = 'models/nt_ip_unc_t', file_refit = "on_change")

summarize_agg_and_PI(nt_ip_unc_t, temp_sd, label = "NT-IP Unconditionalized Recall (t)")

# IP NT CON Base Model --------------------------------------------------------

temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv == 'recall')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

nt_ip_con <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id), backend='cmdstanr',
                     data = temp, iter = 20000, warmup = 10000, cores = 8,
                     prior=rm_priors, control=list(adapt_delta=.99, max_treedepth=15),
                     file = 'models/nt_ip_con', file_refit = "on_change")

summarize_agg_and_PI(nt_ip_con, temp_sd, label = "NT-IP Conditionalized Recall")

# IP NT CON Base Model (Student t) --------------------------------------------------------

temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv == 'recall')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

nt_ip_con_t <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id), backend='cmdstanr',
                     family = student,
                     data = temp, iter = 20000, warmup = 10000, cores = 8,
                     prior=c(rm_priors, student_t_df_prior), control=list(adapt_delta=.99, max_treedepth=15),
                     file = 'models/nt_ip_con_t', file_refit = "on_change")

summarize_agg_and_PI(nt_ip_con_t, temp_sd, label = "NT-IP Conditionalized Recall (t)")

# RECOGNITION -------------------------------------------------------------

# SP NT UNC Base Model Recog --------------------------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv =='recognition')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

nt_sp_unc_recog <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
                       backend='cmdstanr',
                       data = temp, iter = 20000, warmup = 10000, cores = 8,
                       prior=rm_priors, control=list(adapt_delta=.999, max_treedepth=15),
                       file = 'models/nt_sp_unc_recog', file_refit = "on_change")

summarize_agg_and_PI(nt_sp_unc_recog, temp_sd, label = "NT-R Unconditionalized")

temp_post = posterior_epred(nt_sp_unc_recog, newdata = data.frame(sei=0, id=9999, es_id=9999), allow_new_levels=TRUE, re.form=~(1|id))
mean(temp_post > 0)

# SP NT UNC Base Model Recog (Exc. Hart) --------------------------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv =='recognition') %>%
  filter(citation != "Hart, 2006")

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

nt_sp_unc_recog_exc_hart <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
                       backend='cmdstanr',
                       data = temp, iter = 20000, warmup = 10000, cores = 8,
                       prior=rm_priors, control=list(adapt_delta=.999, max_treedepth=15),
                       file = 'models/nt_sp_unc_recog_exc_hart', file_refit = "on_change")

summarize_agg_and_PI(nt_sp_unc_recog_exc_hart, temp_sd, label = "NT-R Unconditionalized (exc Hart)")

temp_post = posterior_epred(nt_sp_unc_recog_exc_hart, newdata = data.frame(sei=0, id=9999, es_id=9999), allow_new_levels=TRUE)
mean(temp_post > 0)

# SP NT UNC Base Model Recog (Student t) --------------------------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv =='recognition')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

nt_sp_unc_recog_t <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
                                backend='cmdstanr',
                                family = student,
                                data = temp, iter = 20000, warmup = 10000, cores = 8,
                                prior=c(rm_priors, student_t_df_prior), control=list(adapt_delta=.999, max_treedepth=15),
                                file = 'models/nt_sp_unc_recog_t', file_refit = "on_change")

summarize_agg_and_PI(nt_sp_unc_recog_t, temp_sd, label = "NT-R Unconditionalized (t)")

temp_post = posterior_epred(nt_sp_unc_recog_t, newdata = data.frame(sei=0, id=9999, es_id=9999), allow_new_levels=TRUE)
mean(temp_post > 0)

# SP NT CON Base Model Recog --------------------------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recognition')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

nt_sp_con_recog <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
                       backend='cmdstanr',
                       data = temp, iter = 20000, warmup = 10000, cores = 8,
                       prior=rm_priors, control=list(adapt_delta=.99, max_treedepth=15),
                       file = 'models/nt_sp_con_recog', file_refit = "on_change")

summarize_agg_and_PI(nt_sp_con_recog, temp_sd, label = "NT-R Conditionalized")

temp_post = posterior_epred(nt_sp_con_recog, newdata = data.frame(sei=0, id=9999, es_id=9999), allow_new_levels=TRUE, re.form=~(1|id))
mean(temp_post > 0)

# SP NT CON Base Model Recog (Student t) --------------------------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recognition')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

nt_sp_con_recog_t <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
                       family = student,
                       backend='cmdstanr',
                       data = temp, iter = 20000, warmup = 10000, cores = 8,
                       prior= c(rm_priors, student_t_df_prior), control=list(adapt_delta=.99, max_treedepth=15),
                       file = 'models/nt_sp_con_recog_t', file_refit = "on_change")

summarize_agg_and_PI(nt_sp_con_recog_t, temp_sd, label = "NT-R Conditionalized (t)")

temp_post = posterior_epred(nt_sp_con_recog_t, newdata = data.frame(sei=0, id=9999, es_id=9999), allow_new_levels=TRUE)
mean(temp_post > 0)

# COMPARISONS -------------------------------------------------------------
# Comparing DVs (Unconditionalized) ---------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes') %>%
  mutate(test_type = ifelse(dv=='recall', test_type, 'R')) %>%
  bind_rows(nt_ip_dat %>% filter(repetitions==max_rep, is_conditionalized!='yes'))

nt_sp_unc_compdv <- brm(yi | se(sei) ~ test_type-1 + (test_type-1|id) + (1|es_id),
                 backend='cmdstanr',
                 data = temp, iter = 20000, warmup = 10000, cores = 8,
                 prior=rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                 file = 'models/nt_sp_unc_compdv', file_refit = "on_change")

hypothesis(nt_sp_unc_compdv, 'test_typeIP - test_typeSP > 0')
hypothesis(nt_sp_unc_compdv, 'test_typeSP - test_typeR > 0')

hypothesis(nt_sp_unc_compdv, 'test_typeIP - test_typeSP = 0')
hypothesis(nt_sp_unc_compdv, 'test_typeSP - test_typeR = 0')

# Comparing DVs (Conditionalized) ---------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no') %>%
  mutate(test_type = ifelse(dv=='recall', test_type, 'R')) %>%
  bind_rows(nt_ip_dat %>% filter(repetitions==max_rep, is_conditionalized!='yes'))

nt_sp_con_compdv <- brm(yi | se(sei) ~ test_type-1 + (test_type-1|id) + (1|es_id),
                        backend='cmdstanr',
                        data = temp, iter = 20000, warmup = 10000, cores = 8,
                        prior=rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                        file = 'models/nt_sp_con_compdv', file_refit = "on_change")

hypothesis(nt_sp_con_compdv, 'test_typeIP - test_typeSP > 0')
hypothesis(nt_sp_con_compdv, 'test_typeSP - test_typeR > 0')

hypothesis(nt_sp_con_compdv, 'test_typeIP - test_typeSP = 0')
hypothesis(nt_sp_con_compdv, 'test_typeSP - test_typeR = 0')


# COMPARISONS (Neutral Words Only) -------------------------------------------------------------
# Comparing DVs (Unconditionalized) ---------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', target_type=='word') %>%
  mutate(test_type = ifelse(dv=='recall', test_type, 'R')) %>%
  filter(test_type != 'R') %>% # There is only one recognition study, so exclude it
  bind_rows(nt_ip_dat %>% filter(repetitions==max_rep, is_conditionalized!='yes', target_type=='word'))

nt_sp_unc_compdv_neutword <- brm(yi | se(sei) ~ test_type-1 + (test_type-1|id) + (1|es_id),
                        backend='cmdstanr',
                        data = temp, iter = 20000, warmup = 10000, cores = 8,
                        prior=rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                        file = 'models/nt_sp_unc_compdv_neutword', file_refit = "on_change")

hypothesis(nt_sp_unc_compdv_neutword, 'test_typeIP - test_typeSP > 0')
hypothesis(nt_sp_unc_compdv_neutword, 'test_typeIP - test_typeSP = 0')

# Comparing DVs (Conditionalized) ---------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', target_type=='word') %>%
  mutate(test_type = ifelse(dv=='recall', test_type, 'R')) %>%
  filter(test_type != 'R') %>% # There is only one recognition study, so exclude it
  bind_rows(nt_ip_dat %>% filter(repetitions==max_rep, is_conditionalized!='no', target_type=='word'))

nt_sp_con_compdv_neutword <- brm(yi | se(sei) ~ test_type-1 + (test_type-1|id) + (1|es_id),
                        backend='cmdstanr',
                        data = temp, iter = 20000, warmup = 10000, cores = 8,
                        prior=rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                        file = 'models/nt_sp_con_compdv_neutword', file_refit = "on_change")

hypothesis(nt_sp_con_compdv_neutword, 'test_typeIP - test_typeSP > 0')
hypothesis(nt_sp_con_compdv_neutword, 'test_typeIP - test_typeSP = 0')
