
source('scripts/1.0 Process Data.R')

# Exclude Clinical Controls -----------------------------------------------

t_sp_dat = t_sp_dat %>% filter(include==1)
t_ip_dat = t_ip_dat %>% filter(include==1)

# SAME PROBE --------------------------------------------------------------
# SP T UNC Base Model Recall --------------------------------------------------------

temp = t_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

t_sp_unc <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
               backend='cmdstanr',
               data = temp, iter = 20000, warmup = 5000, cores = 4,
               prior=rm_priors, control=list(adapt_delta=.99, max_treedepth=15),
               file = 'models/t_sp_unc', file_refit = "on_change")

summarize_agg_and_PI(t_sp_unc, temp_sd, label = "T-SP Unconditionalized Recall")

# SP T UNC Base Model Recall (Student t) --------------------------------------------------------

temp = t_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

t_sp_unc_t <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
                 backend='cmdstanr',
                 family=student,
                 data = temp, iter = 20000, warmup = 5000, cores = 4,
                 prior=c(rm_priors, student_t_df_prior), control=list(adapt_delta=.99, max_treedepth=15),
                 file = 'models/t_sp_unc_t', file_refit = "on_change")

summarize_agg_and_PI(t_sp_unc_t, temp_sd, label = "T-SP Unconditionalized Recall (t)")

# SP T CON Base Model Recall --------------------------------------------------------

temp = t_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv == 'recall')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

t_sp_con <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id), backend='cmdstanr',
               data = temp, iter = 20000, warmup = 5000, cores = 4,
               prior=rm_priors, control=list(adapt_delta=.99, max_treedepth=15),
               file = 'models/t_sp_con', file_refit = "on_change")

summarize_agg_and_PI(t_sp_con, temp_sd, label = "T-SP Conditionalized Recall")

# SP T CON Base Model Recall (Student t) --------------------------------------------------------

temp = t_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv == 'recall')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

t_sp_con_t <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id), backend='cmdstanr',
                 family=student,
                 data = temp, iter = 20000, warmup = 5000, cores = 4,
                 prior=c(rm_priors, student_t_df_prior), control=list(adapt_delta=.99, max_treedepth=15),
                 file = 'models/t_sp_con_t', file_refit = "on_change")

summarize_agg_and_PI(t_sp_con_t, temp_sd, label = "T-SP Conditionalized Recall (t)")

# INDEPENDENT PROBE -------------------------------------------------------
# IP T UNC Base Model --------------------------------------------------------

temp = t_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

t_ip_unc = brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id), backend='cmdstanr',
                  data = temp, iter = 20000, warmup = 5000, cores = 4,
                  prior=rm_priors, control=list(adapt_delta=.99, max_treedepth=15),
                  file = 'models/t_ip_unc', file_refit = "on_change")

summarize_agg_and_PI(t_ip_unc, temp_sd, label = "T-IP Unconditionalized Recall")

# IP T UNC Base Model (Student t) --------------------------------------------------------

temp = t_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

t_ip_unc_t = brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id), backend='cmdstanr',
                 family=student,
                 data = temp, iter = 20000, warmup = 5000, cores = 4,
                 prior=c(rm_priors, student_t_df_prior), control=list(adapt_delta=.99, max_treedepth=15),
                 file = 'models/t_ip_unc_t', file_refit = "on_change")

summarize_agg_and_PI(t_ip_unc_t, temp_sd, label = "T-IP Unconditionalized Recall (t)")

# IP T CON Base Model --------------------------------------------------------

temp = t_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

t_ip_con <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id), backend='cmdstanr',
                     data = temp, iter = 20000, warmup = 5000, cores = 4,
                     prior=rm_priors, control=list(adapt_delta=.99, max_treedepth=15),
                     file = 'models/t_ip_con', file_refit = "on_change")

summarize_agg_and_PI(t_ip_con, temp_sd, label = "T-IP Conditionalized Recall")

# IP T CON Base Model (Student t) --------------------------------------------------------

temp = t_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

t_ip_con_t = brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id), backend='cmdstanr',
                     family = student,
                     data = temp, iter = 20000, warmup = 5000, cores = 4,
                     prior=c(rm_priors, student_t_df_prior), control=list(adapt_delta=.99, max_treedepth=15),
                     file = 'models/t_ip_con_t', file_refit = "on_change")

summarize_agg_and_PI(t_ip_con_t, temp_sd, label = "T-IP Conditionalized Recall (t)")

# RECOGNITION -------------------------------------------------------------

# SP T UNC Base Model Recog --------------------------------------------------------

temp = t_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv =='recognition')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

t_sp_unc_recog = brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
                       backend='cmdstanr',
                       data = temp, iter = 20000, warmup = 5000, cores = 4,
                       prior=rm_priors, control=list(adapt_delta=.999, max_treedepth=15),
                       file = 'models/t_sp_unc_recog', file_refit = "on_change")

summarize_agg_and_PI(t_sp_unc_recog, temp_sd, label = "T-IP Unconditionalized Recog")

# SP T UNC Base Model Recog (Exc. Hart) --------------------------------------------------------

temp = t_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv =='recognition') %>%
  filter(citation != "Hart, 2006")

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

t_sp_unc_recog_exc_hart <- brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
                       backend='cmdstanr',
                       data = temp, iter = 10000, warmup = 5000, cores = 4,
                       prior=rm_priors, control=list(adapt_delta=.999, max_treedepth=15),
                       file = 'models/t_sp_unc_recog_exc_hart', file_refit = "on_change")

summarize_agg_and_PI(t_sp_unc_recog_exc_hart, temp_sd, label = "T-IP Unconditionalized Recog")

# SP T UNC Base Model Recog (Student t) --------------------------------------------------------

temp = t_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv =='recognition')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

t_sp_unc_recog_t = brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
                                backend='cmdstanr',
                                family = student,
                                data = temp, iter = 20000, warmup = 5000, cores = 4,
                                prior=c(rm_priors, student_t_df_prior), control=list(adapt_delta=.999, max_treedepth=15),
                                file = 'models/t_sp_unc_recog_t', file_refit = "on_change")

summarize_agg_and_PI(t_sp_unc_recog_t, temp_sd, label = "T-IP Unconditionalized Recog (t)")

# SP T CON Base Model Recog --------------------------------------------------------

temp = t_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recognition')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

t_sp_con_recog = brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
                       backend='cmdstanr',
                       data = temp, iter = 20000, warmup = 5000, cores = 4,
                       prior=rm_priors, control=list(adapt_delta=.99, max_treedepth=15),
                       file = 'models/t_sp_con_recog', file_refit = "on_change")

summarize_agg_and_PI(t_sp_con_recog, temp_sd, label = "T-IP Conditionalized Recog")

# SP T CON Base Model Recog (Student t) --------------------------------------------------------

temp = t_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recognition')

temp_sd = meta_estimate_typical_diff_sd(temp) # Meta-analytic estimate of typical SD

t_sp_con_recog_t = brm(yi | se(sei) ~ 1 + (1|id) + (1|es_id),
                       family = student,
                       backend='cmdstanr',
                       data = temp, iter = 20000, warmup = 5000, cores = 4,
                       prior= c(rm_priors, student_t_df_prior), control=list(adapt_delta=.99, max_treedepth=15),
                       file = 'models/t_sp_con_recog_t', file_refit = "on_change")

summarize_agg_and_PI(t_sp_con_recog_t, temp_sd, label = "T-IP Conditionalized Recog (t)")
