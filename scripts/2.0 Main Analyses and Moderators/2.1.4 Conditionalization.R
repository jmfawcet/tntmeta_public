
# SAME PROBE --------------------------------------------------------------

temp=nt_sp_dat %>% 
  filter(repetitions==max_rep, dv=='recall') %>%
  #filter(learning_phase_type != 'drop-off') %>% # In case excluding drop-off is desired
  mutate(is_conditionalized = ifelse(is_conditionalized=='no', 'no', 'yes')) %>%
  select(yi, sei, is_conditionalized, es_id, id)

nt_sp_cond <- brm(yi | se(sei) ~ is_conditionalized-1 + (is_conditionalized-1|id) + (1|es_id), backend = 'cmdstanr', 
                  data = temp, iter = 20000, warmup = 5000, cores = 8,
                  prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15), 
                  file = 'models/nt_sp_condmod', file_refit = "on_change")

hypothesis(nt_sp_cond, 'is_conditionalizedyes - is_conditionalizedno> 0')
hypothesis(nt_sp_cond, 'is_conditionalizedyes - is_conditionalizedno=0')

# INDEPENDENT PROBE -------------------------------------------------------

temp=nt_ip_dat %>% 
  filter(repetitions==max_rep, dv=='recall') %>%
  #filter(learning_phase_type != 'drop-off') %>% # In case excluding drop-off is desired
  mutate(is_conditionalized = ifelse(is_conditionalized=='no', 'no', 'yes')) %>%
  select(yi, sei, is_conditionalized, es_id, id)

nt_ip_cond <- brm(yi | se(sei) ~ is_conditionalized-1 + (is_conditionalized-1|id) + (1|es_id), backend = 'cmdstanr', 
                      data = temp, iter = 20000, warmup = 5000, cores = 8,
                      prior = rm_cat_priors, control=list(adapt_delta=.99, max_treedepth=15), 
                      file = 'models/nt_ip_condmod', file_refit = "on_change")

hypothesis(nt_ip_cond, 'is_conditionalizedyes - is_conditionalizedno> 0')
hypothesis(nt_ip_cond, 'is_conditionalizedyes - is_conditionalizedno= 0')

# RECOGNITION -------------------------------------------------------

# Not conducted
