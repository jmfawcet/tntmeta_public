
# SAME PROBE --------------------------------------------------------------
# SP NT UNC Base Model Recall --------------------------------------------------------

#####
#Target - All studies
#####
temp=nt_sp_dat %>% 
  filter(repetitions==max_rep,is_conditionalized!='yes', dv=='recall') %>%
  mutate(target_type = relevel(factor(target_type), 'word'))

nt_sp_unc_targettype = brm(yi | se(sei) ~ target_type-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                        data = temp, iter = 20000, warmup = 5000, cores = 8,
                        prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                        file = 'models/nt_sp_unc_target', file_refit = "on_change")

hypothesis(nt_sp_unc_targettype, 'target_typeobject - target_typeword > 0')
hypothesis(nt_sp_unc_targettype, 'target_typescene       - target_typeword > 0')
hypothesis(nt_sp_unc_targettype, 'target_typememory      - target_typeword > 0')

hypothesis(nt_sp_unc_targettype, 'target_typeobject - target_typeword = 0')
hypothesis(nt_sp_unc_targettype, 'target_typescene       - target_typeword = 0')
hypothesis(nt_sp_unc_targettype, 'target_typememory      - target_typeword = 0')

#####
# Valence
#####

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall') %>%
  mutate(target_valence = ifelse(target_valence == 'undifferentiated', 'neutral', target_valence)) %>%
  mutate(target_valence = ifelse(target_valence == 'neutral', 'neutral', 'emotional')) %>%
  mutate(target_valence = relevel(factor(target_valence), 'neutral'))

nt_sp_unc_targetvalence = brm(yi | se(sei) ~ target_valence-1  + (1|id) + (1|es_id), backend = 'cmdstanr', 
                           data = temp, iter = 20000, warmup = 5000, cores = 8,
                           prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                           file = 'models/nt_sp_unc_target_val', file_refit = "on_change")

hypothesis(nt_sp_unc_targetvalence, 'target_valenceneutral        - target_valenceemotional > 0')
hypothesis(nt_sp_unc_targetvalence, 'target_valenceneutral        - target_valenceemotional = 0')

# SP NT CON Base Model Recall --------------------------------------------------------

#####
#Target - All studies
#####
temp=nt_sp_dat %>% 
  filter(repetitions==max_rep,is_conditionalized!='no', dv=='recall') %>%
  mutate(target_type = relevel(factor(target_type), 'word')) %>%
  filter(target_type != 'other', target_type != 'object')

nt_sp_con_targettype = brm(yi | se(sei) ~ target_type-1 + (1|id) + (1|es_id), backend = 'cmdstanr', 
                            data = temp, iter = 20000, warmup = 5000, cores = 8,
                            prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                            file = 'models/nt_sp_con_target', file_refit = "on_change")

hypothesis(nt_sp_con_targettype, 'target_typescene       - target_typeword > 0')
hypothesis(nt_sp_con_targettype, 'target_typescene       - target_typeword = 0')

#####
# Valence
#####

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall') %>%
  mutate(target_valence = ifelse(target_valence == 'undifferentiated', 'neutral', target_valence)) %>%
  mutate(target_valence = ifelse(target_valence == 'neutral', 'neutral', 'emotional')) %>%
  mutate(target_valence = relevel(factor(target_valence), 'neutral'))

nt_sp_con_targetvalence = brm(yi | se(sei) ~ target_valence-1  + (1|id) + (1|es_id), backend = 'cmdstanr', 
                               data = temp, iter = 20000, warmup = 5000, cores = 8,
                               prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15),
                               file = 'models/nt_sp_con_target_val', file_refit = "on_change")

hypothesis(nt_sp_con_targetvalence, 'target_valenceemotional        - target_valenceneutral > 0')
hypothesis(nt_sp_con_targetvalence, 'target_valenceemotional        - target_valenceneutral = 0')

# INDEPENDENT PROBE -------------------------------------------------------
# IP NT UNC Base Model --------------------------------------------------------

#####
# Target - All studies
# Only word
#####

#####
# Valence
# Only 1 Emotional
#####

# IP NT CON Base Model --------------------------------------------------------

#####
# Target - All studies
# Only word
#####

#####
# Valence
# Only 5 Emotional
#####

