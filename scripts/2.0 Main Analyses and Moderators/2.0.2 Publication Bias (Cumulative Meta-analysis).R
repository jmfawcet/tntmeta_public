source('scripts/1.0 Process Data.R')

# Exclude Clinical Controls -----------------------------------------------

nt_sp_dat = nt_sp_dat %>% filter(include==1)
nt_ip_dat = nt_ip_dat %>% filter(include==1)

# SP Unconditionalized Cumulative Models ------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall')

sample_list = temp %>%
  select(id, sample_id, n) %>%
  arrange(n, id, sample_id) %>%
  unique() %>%
  mutate(up_order = 1:n(), down_order = n():1)

temp %>%
  left_join(sample_list) -> temp

# SP Unc Step up (smallest samples to largest)
# Note: PI's are for a typical comparison in a new study, aligned with our in-text focus
nt_sp_unc_up = NULL
for(i in seq(5, max(temp$up_order)))
{
  c_temp = temp %>% filter(up_order <= i)
  nt_sp_unc_cumu = brm(bf(yi | se(sei) ~ 1 + (1|id) + (1|es_id)), 
                      data = c_temp, iter = 10000, warmup = 5000, cores = 8, backend='cmdstanr',
                      control=list(adapt_delta=.99, max_treedepth=15),
                      prior=rm_priors)
  
  c_ci = posterior_summary(posterior_epred(nt_sp_unc_cumu, newdata = data.frame(sei=0, id=9999, es_id=9999), 
                                           allow_new_levels=TRUE, re.form=NA)) %>%
    data.frame() %>%
    rename(m=Estimate, se=`Est.Error`, lb=Q2.5, ub=Q97.5) %>%
    select(-se)
  c_pi = posterior_summary(posterior_epred(nt_sp_unc_cumu, newdata = data.frame(sei=0, id=9999, es_id=9999), 
                                           allow_new_levels=TRUE, re.form = ~(1 | id))) %>%
    data.frame() %>%
    rename(m=Estimate, se=`Est.Error`, pi_lb=Q2.5, pi_ub=Q97.5) %>%
    select(-m, -se)
  
  nt_sp_unc_up = bind_rows(nt_sp_unc_up, bind_cols(c_ci, c_pi) %>% mutate(step=i, cond='Unc. SP'))
}

saveRDS(nt_sp_unc_up, 'models/cumulative_meta_sp_unc_up.rds')

# SP Unc Step down (largest samples to smallest)
# Note: PI's are for a typical comparison in a new study, aligned with our in-text focus
nt_sp_unc_down = NULL
for(i in seq(5, max(temp$down_order)))
{
  c_temp = temp %>% filter(down_order <= i)
  nt_sp_unc_cumu = brm(bf(yi | se(sei) ~ 1 + (1|id) + (1|es_id)), 
                       data = c_temp, iter = 10000, warmup = 5000, cores = 8, backend='cmdstanr',
                       control=list(adapt_delta=.99, max_treedepth=15),
                       prior=rm_priors)
  
  c_ci = posterior_summary(posterior_epred(nt_sp_unc_cumu, newdata = data.frame(sei=0, id=9999, es_id=9999), 
                                           allow_new_levels=TRUE, re.form=NA)) %>%
    data.frame() %>%
    rename(m=Estimate, se=`Est.Error`, lb=Q2.5, ub=Q97.5) %>%
    select(-se)
  c_pi = posterior_summary(posterior_epred(nt_sp_unc_cumu, newdata = data.frame(sei=0, id=9999, es_id=9999), 
                                           allow_new_levels=TRUE, re.form = ~(1 | id))) %>%
    data.frame() %>%
    rename(m=Estimate, se=`Est.Error`, pi_lb=Q2.5, pi_ub=Q97.5) %>%
    select(-m, -se)
  
  nt_sp_unc_down = bind_rows(nt_sp_unc_down, bind_cols(c_ci, c_pi) %>% mutate(step=i, cond='Unc. SP'))
}

saveRDS(nt_sp_unc_down, 'models/cumulative_meta_sp_unc_down.rds')

# SP Conditionalized Cumulative Models ------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall')

sample_list = temp %>%
  select(id, sample_id, n) %>%
  arrange(n, id, sample_id) %>%
  unique() %>%
  mutate(up_order = 1:n(), down_order = n():1)

temp %>%
  left_join(sample_list) -> temp

# SP Con Step up
# Note: PI's are for a typical comparison in a new study, aligned with our in-text focus
nt_sp_con_up = NULL
for(i in seq(5, max(temp$up_order)))
{
  c_temp = temp %>% filter(up_order <= i)
  nt_sp_con_cumu = brm(bf(yi | se(sei) ~ 1 + (1|id) + (1|es_id)), 
                       data = c_temp, iter = 10000, warmup = 5000, cores = 8, backend='cmdstanr',
                       control=list(adapt_delta=.99, max_treedepth=15),
                       prior=rm_priors)
  
  c_ci = posterior_summary(posterior_epred(nt_sp_con_cumu, newdata = data.frame(sei=0, id=9999, es_id=9999), 
                                           allow_new_levels=TRUE, re.form=NA)) %>%
    data.frame() %>%
    rename(m=Estimate, se=`Est.Error`, lb=Q2.5, ub=Q97.5) %>%
    select(-se)
  c_pi = posterior_summary(posterior_epred(nt_sp_con_cumu, newdata = data.frame(sei=0, id=9999, es_id=9999), 
                                           allow_new_levels=TRUE, re.form = ~(1 | id))) %>%
    data.frame() %>%
    rename(m=Estimate, se=`Est.Error`, pi_lb=Q2.5, pi_ub=Q97.5) %>%
    select(-m, -se)
  
  nt_sp_con_up = bind_rows(nt_sp_con_up, bind_cols(c_ci, c_pi) %>% mutate(step=i, cond='Con. SP'))
}

saveRDS(nt_sp_con_up, 'models/cumulative_meta_sp_con_up.rds')

# SP Con Step down
# Note: PI's are for a typical comparison in a new study, aligned with our in-text focus
nt_sp_con_down = NULL
for(i in seq(5, max(temp$down_order)))
{
  c_temp = temp %>% filter(down_order <= i)
  nt_sp_con_cumu = brm(bf(yi | se(sei) ~ 1 + (1|id) + (1|es_id)), 
                       data = c_temp, iter = 10000, warmup = 5000, cores = 8, backend='cmdstanr',
                       control=list(adapt_delta=.99, max_treedepth=15),
                       prior=rm_priors)
  
  c_ci = posterior_summary(posterior_epred(nt_sp_con_cumu, newdata = data.frame(sei=0, id=9999, es_id=9999), 
                                           allow_new_levels=TRUE, re.form=NA)) %>%
    data.frame() %>%
    rename(m=Estimate, se=`Est.Error`, lb=Q2.5, ub=Q97.5) %>%
    select(-se)
  c_pi = posterior_summary(posterior_epred(nt_sp_con_cumu, newdata = data.frame(sei=0, id=9999, es_id=9999), 
                                           allow_new_levels=TRUE, re.form = ~(1 | id))) %>%
    data.frame() %>%
    rename(m=Estimate, se=`Est.Error`, pi_lb=Q2.5, pi_ub=Q97.5) %>%
    select(-m, -se)
  
  nt_sp_con_down = bind_rows(nt_sp_con_down, bind_cols(c_ci, c_pi) %>% mutate(step=i, cond='Con. SP'))
}

saveRDS(nt_sp_con_down, 'models/cumulative_meta_sp_con_down.rds')


# SP Cumulative Meta-analysis Plot ----------------------------------------

nt_sp_unc_up = readRDS('models/cumulative_meta_sp_unc_up.rds')
nt_sp_unc_down = readRDS('models/cumulative_meta_sp_unc_down.rds')
nt_sp_con_up = readRDS('models/cumulative_meta_sp_con_up.rds')
nt_sp_con_down = readRDS('models/cumulative_meta_sp_con_down.rds')

# Difference waves
nt_sp_unc_diff = nt_sp_unc_up %>%
  mutate(m = m - nt_sp_unc_down$m, lb=NA, ub=NA, pi_lb=NA, pi_ub=NA, steplab='Difference')

nt_sp_con_diff = nt_sp_con_up %>%
  mutate(m = m - nt_sp_con_down$m, lb=NA, ub=NA, pi_lb=NA, pi_ub=NA, steplab='Difference')

# Combine
nt_sp_unc_up %>%
  mutate(steplab='Small to Large') %>%
  bind_rows(nt_sp_unc_down %>% mutate(steplab='Large to Small')) %>%
  bind_rows(nt_sp_con_up %>% mutate(steplab='Small to Large')) %>%
  bind_rows(nt_sp_con_down %>% mutate(steplab='Large to Small')) -> nt_sp_unc_comb

nt_sp_unc_comb %>%
  ggplot(aes(step, m, ymax=ub, ymin=lb)) +
  geom_line() + 
  geom_ribbon(alpha=.5) +
  geom_ribbon(aes(ymax=pi_ub, ymin=pi_lb), alpha=.25) +
  facet_grid(steplab~cond) +
  theme_classic() +
  scale_y_continuous(name='SIF (%)', limits=c(-10, 20)) +
  scale_x_continuous(name='Included Samples (#)') +
  geom_hline(yintercept=0, linetype='dotted') +
  theme(
    strip.background = element_rect(fill = "black", color = "black"),
    strip.text = element_text(color = "white", face = "bold")
  )

ggsave('figures/SP Cumulative Meta.pdf', width=5, height=5)


# IP Unconditionalized Cumulative Models ------------------------------------

temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall')

sample_list = temp %>%
  select(id, sample_id, n) %>%
  arrange(n, id, sample_id) %>%
  unique() %>%
  mutate(up_order = 1:n(), down_order = n():1)

temp %>%
  left_join(sample_list) -> temp

# IP Unc Step up
# Note: PI's are for a typical comparison in a new study, aligned with our in-text focus
nt_ip_unc_up = NULL
for(i in seq(5, max(temp$up_order)))
{
  c_temp = temp %>% filter(up_order <= i)
  nt_ip_unc_cumu = brm(bf(yi | se(sei) ~ 1 + (1|id) + (1|es_id)), 
                       data = c_temp, iter = 10000, warmup = 5000, cores = 8, backend='cmdstanr',
                       control=list(adapt_delta=.99, max_treedepth=15),
                       prior=rm_priors)
  
  c_ci = posterior_summary(posterior_epred(nt_ip_unc_cumu, newdata = data.frame(sei=0, id=9999, es_id=9999), 
                                           allow_new_levels=TRUE, re.form=NA)) %>%
    data.frame() %>%
    rename(m=Estimate, se=`Est.Error`, lb=Q2.5, ub=Q97.5) %>%
    select(-se)
  c_pi = posterior_summary(posterior_epred(nt_ip_unc_cumu, newdata = data.frame(sei=0, id=9999, es_id=9999), 
                                           allow_new_levels=TRUE, re.form = ~(1 | id))) %>%
    data.frame() %>%
    rename(m=Estimate, se=`Est.Error`, pi_lb=Q2.5, pi_ub=Q97.5) %>%
    select(-m, -se)
  
  nt_ip_unc_up = bind_rows(nt_ip_unc_up, bind_cols(c_ci, c_pi) %>% mutate(step=i, cond='Unc. IP'))
}

saveRDS(nt_ip_unc_up, 'models/cumulative_meta_ip_unc_up.rds')

# IP Unc Step down
# Note: PI's are for a typical comparison in a new study, aligned with our in-text focus
nt_ip_unc_down = NULL
for(i in seq(5, max(temp$down_order)))
{
  c_temp = temp %>% filter(down_order <= i)
  nt_ip_unc_cumu = brm(bf(yi | se(sei) ~ 1 + (1|id) + (1|es_id)), 
                       data = c_temp, iter = 10000, warmup = 5000, cores = 8, backend='cmdstanr',
                       control=list(adapt_delta=.99, max_treedepth=15),
                       prior=rm_priors)
  
  c_ci = posterior_summary(posterior_epred(nt_ip_unc_cumu, newdata = data.frame(sei=0, id=9999, es_id=9999), 
                                           allow_new_levels=TRUE, re.form=NA)) %>%
    data.frame() %>%
    rename(m=Estimate, se=`Est.Error`, lb=Q2.5, ub=Q97.5) %>%
    select(-se)
  c_pi = posterior_summary(posterior_epred(nt_ip_unc_cumu, newdata = data.frame(sei=0, id=9999, es_id=9999), 
                                           allow_new_levels=TRUE, re.form = ~(1 | id))) %>%
    data.frame() %>%
    rename(m=Estimate, se=`Est.Error`, pi_lb=Q2.5, pi_ub=Q97.5) %>%
    select(-m, -se)
  
  nt_ip_unc_down = bind_rows(nt_ip_unc_down, bind_cols(c_ci, c_pi) %>% mutate(step=i, cond='Unc. IP'))
}

saveRDS(nt_ip_unc_down, 'models/cumulative_meta_ip_unc_down.rds')

# IP Conditionalized Cumulative Models ------------------------------------

temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall')

sample_list = temp %>%
  select(id, sample_id, n) %>%
  arrange(n, id, sample_id) %>%
  unique() %>%
  mutate(up_order = 1:n(), down_order = n():1)

temp %>%
  left_join(sample_list) -> temp

# IP Con Step up
# Note: PI's are for a typical comparison in a new study, aligned with our in-text focus
nt_ip_con_up = NULL
for(i in seq(5, max(temp$up_order)))
{
  c_temp = temp %>% filter(up_order <= i)
  nt_ip_con_cumu = brm(bf(yi | se(sei) ~ 1 + (1|id) + (1|es_id)), 
                       data = c_temp, iter = 10000, warmup = 5000, cores = 8, backend='cmdstanr',
                       control=list(adapt_delta=.99, max_treedepth=15),
                       prior=rm_priors)
  
  c_ci = posterior_summary(posterior_epred(nt_ip_con_cumu, newdata = data.frame(sei=0, id=9999, es_id=9999), 
                                           allow_new_levels=TRUE, re.form=NA)) %>%
    data.frame() %>%
    rename(m=Estimate, se=`Est.Error`, lb=Q2.5, ub=Q97.5) %>%
    select(-se)
  c_pi = posterior_summary(posterior_epred(nt_ip_con_cumu, newdata = data.frame(sei=0, id=9999, es_id=9999), 
                                           allow_new_levels=TRUE, re.form = ~(1 | id))) %>%
    data.frame() %>%
    rename(m=Estimate, se=`Est.Error`, pi_lb=Q2.5, pi_ub=Q97.5) %>%
    select(-m, -se)
  
  nt_ip_con_up = bind_rows(nt_ip_con_up, bind_cols(c_ci, c_pi) %>% mutate(step=i, cond='Con. IP'))
}

saveRDS(nt_ip_con_up, 'models/cumulative_meta_ip_con_up.rds')

# IP Con Step down
# Note: PI's are for a typical comparison in a new study, aligned with our in-text focus
nt_ip_con_down = NULL
for(i in seq(5, max(temp$down_order)))
{
  c_temp = temp %>% filter(down_order <= i)
  nt_ip_con_cumu = brm(bf(yi | se(sei) ~ 1 + (1|id) + (1|es_id)), 
                       data = c_temp, iter = 10000, warmup = 5000, cores = 8, backend='cmdstanr',
                       control=list(adapt_delta=.99, max_treedepth=15),
                       prior=rm_priors)
  
  c_ci = posterior_summary(posterior_epred(nt_ip_con_cumu, newdata = data.frame(sei=0, id=9999, es_id=9999), 
                                           allow_new_levels=TRUE, re.form=NA)) %>%
    data.frame() %>%
    rename(m=Estimate, se=`Est.Error`, lb=Q2.5, ub=Q97.5) %>%
    select(-se)
  c_pi = posterior_summary(posterior_epred(nt_ip_con_cumu, newdata = data.frame(sei=0, id=9999, es_id=9999), 
                                           allow_new_levels=TRUE, re.form = ~(1 | id))) %>%
    data.frame() %>%
    rename(m=Estimate, se=`Est.Error`, pi_lb=Q2.5, pi_ub=Q97.5) %>%
    select(-m, -se)
  
  nt_ip_con_down = bind_rows(nt_ip_con_down, bind_cols(c_ci, c_pi) %>% mutate(step=i, cond='Con. IP'))
}

saveRDS(nt_ip_con_down, 'models/cumulative_meta_ip_con_down.rds')


# ip Cumulative Meta-analysis Plot ----------------------------------------

nt_ip_unc_up %>%
  mutate(steplab='Small to Large') %>%
  bind_rows(nt_ip_unc_down %>% mutate(steplab='Large to Small')) %>%
  bind_rows(nt_ip_con_up %>% mutate(steplab='Small to Large')) %>%
  bind_rows(nt_ip_con_down %>% mutate(steplab='Large to Small')) -> nt_ip_unc_comb

nt_ip_unc_comb %>%
  ggplot(aes(step, m, ymax=ub, ymin=lb)) +
  geom_line() + 
  geom_ribbon(alpha=.5) +
  geom_ribbon(aes(ymax=pi_ub, ymin=pi_lb), alpha=.25) +
  facet_grid(steplab~cond) +
  theme_classic() +
  scale_y_continuous(name='SIF (%)', limits=c(-10, 20)) +
  scale_x_continuous(name='Included Samples (#)') +
  geom_hline(yintercept=0, linetype='dotted') +
  theme(
    strip.background = element_rect(fill = "black", color = "black"),
    strip.text = element_text(color = "white", face = "bold")
  )

ggsave('figures/IP Cumulative Meta.pdf', width=5, height=5)

