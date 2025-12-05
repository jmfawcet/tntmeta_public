
# SAME PROBE --------------------------------------------------------------
# SP NT UNC Base Model Recall --------------------------------------------------------

# Create the data frame
temp = nt_sp_dat %>% 
  filter(is_conditionalized!='yes', dv=='recall') %>%
  mutate(sd_rep = sd(repetitions), cent_rep = (repetitions - 10)/sd_rep) # Centering on 10

# Fit the linear model and test some hypotheses
nt_sp_unc_rep_lin <- brm(yi | se(sei) ~ 1 + cent_rep + (cent_rep|id) + (1|es_id), backend='cmdstanr',
                         data = temp, iter = 20000, warmup = 5000, cores = 8, 
                         prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                         file = 'models/nt_sp_rep', file_refit = "on_change")

hypothesis(nt_sp_unc_rep_lin, 'cent_rep> 0')
hypothesis(nt_sp_unc_rep_lin, 'cent_rep= 0')

# Fit the nonlinear model
nt_sp_unc_rep_smooth <- brm(yi | se(sei) ~ 1 + s(cent_rep) + (cent_rep|id) + (1|es_id), backend='cmdstanr',
                            data = temp, iter = 20000, warmup = 5000, cores = 8, 
                            prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.999, max_treedepth=15),file = 'models/nt_sp_rep3', file_refit = "on_change")

# Create predictions along the curve for both models
rep_pred = expand.grid(cent_rep = seq(min(temp$cent_rep),max(temp$cent_rep),.1), sei=0, id=9999, es_id=9999) %>%
  mutate(repetitions=cent_rep*temp$sd_rep[1] + 10)
nt_sp_unc_rep_lin_pred = posterior_linpred(nt_sp_unc_rep_lin, newdata=rep_pred, re_formula=NA) %>% 
  posterior_summary() %>%
  bind_cols(rep_pred) %>%
  mutate(dv = 'Same Probe (U)', cond = 'UNC', model = 'Linear')
nt_sp_unc_rep_smooth_pred = posterior_linpred(nt_sp_unc_rep_smooth, newdata=rep_pred, re_formula=NA) %>% 
  posterior_summary() %>%
  bind_cols(rep_pred) %>%
  mutate(dv = 'Same Probe (U)', cond = 'UNC', model = 'Non-Linear')

# Combine predictions from the two models
nt_sp_unc_rep_lin_pred %>%
  bind_rows(nt_sp_unc_rep_smooth_pred) -> nt_sp_unc_rep_plot

nt_sp_unc_rep_raw_plot = temp %>%
  select(repetitions, yi, n) %>%
  mutate(dv = 'Same Probe (U)', cond = 'UNC')

# SP NT CON Base Model Recall --------------------------------------------------------

# Create the data frame
temp = nt_sp_dat %>% 
  filter(is_conditionalized!='no', dv=='recall', !is.na(repetitions)) %>%
  mutate(sd_rep = sd(repetitions), cent_rep = (repetitions - 10)/sd_rep) # Centering on 10

# Fit the linear model and test some hypotheses
nt_sp_con_rep_lin <- brm(yi | se(sei) ~ 1 + cent_rep + (cent_rep|id) + (1|es_id), backend='cmdstanr',
                         data = temp, iter = 20000, warmup = 5000, cores = 8, 
                         prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                         file = 'models/nt_sp_con_rep_lin', file_refit = "on_change")

hypothesis(nt_sp_con_rep_lin, 'cent_rep> 0')
hypothesis(nt_sp_con_rep_lin, 'cent_rep= 0')

# Fit the nonlinear model
nt_sp_con_rep_smooth <- brm(yi | se(sei) ~ 1 + s(cent_rep, k=9) + (cent_rep|id) + (1|es_id), backend='cmdstanr',
                            data = temp, iter = 20000, warmup = 5000, cores = 8, 
                            prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.999, max_treedepth=15),
                            file = 'models/nt_sp_con_rep_smooth', file_refit = "on_change")

# Create predictions along the curve for both models
rep_pred = expand.grid(cent_rep =seq(min(temp$cent_rep),max(temp$cent_rep),.1), sei=0, id=9999, es_id=9999) %>%
  mutate(repetitions=cent_rep*temp$sd_rep[1] + 10)
nt_sp_con_rep_lin_pred = posterior_linpred(nt_sp_con_rep_lin, newdata=rep_pred, re_formula=NA) %>% 
  posterior_summary() %>%
  bind_cols(rep_pred) %>%
  mutate(dv = 'Same Probe (C)', cond = 'CON', model = 'Linear')
nt_sp_con_rep_smooth_pred = posterior_linpred(nt_sp_con_rep_smooth, newdata=rep_pred, re_formula=NA) %>% 
  posterior_summary() %>%
  bind_cols(rep_pred) %>%
  mutate(dv = 'Same Probe (C)', cond = 'CON', model = 'Non-Linear')

# Combine predictions from the two models
nt_sp_con_rep_lin_pred %>%
  bind_rows(nt_sp_con_rep_smooth_pred) -> nt_sp_con_rep_plot

nt_sp_con_rep_raw_plot = temp %>%
  select(repetitions, yi, n) %>%
  mutate(dv = 'Same Probe (C)', cond = 'CON')

# INDEPENDENT PROBE -------------------------------------------------------
# IP NT UNC Base Model --------------------------------------------------------

# Create the data frame
temp = nt_ip_dat %>% 
  filter(is_conditionalized!='yes', dv=='recall') %>%
  mutate(sd_rep = sd(repetitions), cent_rep = (repetitions - 10)/sd_rep) # Centering on 10

# Fit the linear model and test some hypotheses
nt_ip_unc_rep_lin <- brm(yi | se(sei) ~ 1 + cent_rep + (cent_rep|id) + (1|es_id), backend='cmdstanr',
                         data = temp, iter = 20000, warmup = 5000, cores = 8, 
                         prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                         file = 'models/nt_ip_rep', file_refit = "on_change")

hypothesis(nt_ip_unc_rep_lin, 'cent_rep> 0')
hypothesis(nt_ip_unc_rep_lin, 'cent_rep= 0')

# Create predictions along the curve for both models
nt_ip_unc_rep_smooth <- brm(yi | se(sei) ~ 1 + s(cent_rep, k=8) + (cent_rep|id) + (1|es_id), backend='cmdstanr',
                            data = temp, iter = 20000, warmup = 5000, cores = 8, 
                            prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.999, max_treedepth=15),file = 'models/nt_ip_rep3', file_refit = "on_change")

# Create predictions along the curve for both models
rep_pred = expand.grid(cent_rep = seq(min(temp$cent_rep),max(temp$cent_rep),.1), sei=0, id=9999, es_id=9999) %>%
  mutate(repetitions=cent_rep*temp$sd_rep[1] + 10)
nt_ip_unc_rep_lin_pred = posterior_linpred(nt_ip_unc_rep_lin, newdata=rep_pred, re_formula=NA) %>% 
  posterior_summary() %>%
  bind_cols(rep_pred) %>%
  mutate(dv = 'Independent Probe (U)', cond = 'UNC', model = 'Linear')
nt_ip_unc_rep_smooth_pred = posterior_linpred(nt_ip_unc_rep_smooth, newdata=rep_pred, re_formula=NA) %>% 
  posterior_summary() %>%
  bind_cols(rep_pred) %>%
  mutate(dv = 'Independent Probe (U)', cond = 'UNC', model = 'Non-Linear')

# Combine predictions from the two models
nt_ip_unc_rep_lin_pred %>%
  bind_rows(nt_ip_unc_rep_smooth_pred) -> nt_ip_unc_rep_plot

nt_ip_unc_rep_raw_plot = temp %>%
  select(repetitions, yi, n) %>%
  mutate(dv = 'Independent Probe (U)', cond = 'UNC')

# IP NT CON Base Model --------------------------------------------------------

# Create the data frame
temp = nt_ip_dat %>% 
  filter(is_conditionalized!='no', dv=='recall') %>%
  mutate(sd_rep = sd(repetitions), cent_rep = (repetitions - 10)/sd_rep) # Centering on 10

# Fit the linear model and test some hypotheses
nt_ip_con_rep_lin <- brm(yi | se(sei) ~ 1 + cent_rep + (cent_rep|id) + (1|es_id), backend='cmdstanr',
                         data = temp, iter = 20000, warmup = 5000, cores = 8, 
                         prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.99, max_treedepth=15), 
                         file = 'models/nt_ip_con_rep', file_refit = "on_change")

hypothesis(nt_ip_con_rep_lin, 'cent_rep> 0')
hypothesis(nt_ip_con_rep_lin, 'cent_rep= 0')

# Create predictions along the curve for both models
nt_ip_con_rep_smooth <- brm(yi | se(sei) ~ 1 + s(cent_rep, k=6) + (cent_rep|id) + (1|es_id), backend='cmdstanr',
                            data = temp, iter = 20000, warmup = 5000, cores = 8, 
                            prior=c(rm_priors, rm_b_priors), control=list(adapt_delta=.999, max_treedepth=15),
                            file = 'models/nt_ip_con_rep3', file_refit = "on_change")

# Create predictions along the curve for both models
rep_pred = expand.grid(cent_rep = seq(min(temp$cent_rep),max(temp$cent_rep),.1), sei=0, id=9999, es_id=9999) %>%
  mutate(repetitions=cent_rep*temp$sd_rep[1] + 10)
nt_ip_con_rep_lin_pred = posterior_linpred(nt_ip_con_rep_lin, newdata=rep_pred, re_formula=NA) %>% 
  posterior_summary() %>%
  bind_cols(rep_pred) %>%
  mutate(dv = 'Independent Probe (C)', cond = 'CON', model = 'Linear')
nt_ip_con_rep_smooth_pred = posterior_linpred(nt_ip_con_rep_smooth, newdata=rep_pred, re_formula=NA) %>% 
  posterior_summary() %>%
  bind_cols(rep_pred) %>%
  mutate(dv = 'Independent Probe (C)', cond = 'CON', model = 'Non-Linear')

# Combine predictions from the two models
nt_ip_con_rep_lin_pred %>%
  bind_rows(nt_ip_con_rep_smooth_pred) -> nt_ip_con_rep_plot

nt_ip_con_rep_raw_plot = temp %>%
  select(repetitions, yi, n) %>%
  mutate(dv = 'Independent Probe (C)', cond = 'CON')

# Plot --------------------------------------------------------------------

label_levels = c('Same Probe (U)', 'Same Probe (C)', 'Independent Probe (U)', 'Independent Probe (C)')

# Set APA theme
apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.spacing.y = unit(.1,"line"),
        panel.spacing.x = unit(.1,"line"),
        axis.line=element_line(),
        axis.text = element_text(colour='black', size=5),
        text=element_text(family='sans', size = 8.5),
        strip.background =element_rect(fill="black"),
        strip.text.x = element_text(size = 7, colour = 'white', face='bold'),
        strip.text.y = element_text(size = 7, colour = 'white', face='bold'),
        legend.position='none')

nt_sp_unc_rep_plot %>%
  bind_rows(nt_sp_con_rep_plot) %>%
  bind_rows(nt_ip_unc_rep_plot) %>%
  bind_rows(nt_ip_con_rep_plot) %>%
  mutate(dv = factor(dv, levels = label_levels)) -> rep_plot

nt_sp_unc_rep_raw_plot %>%
  bind_rows(nt_sp_con_rep_raw_plot) %>%
  bind_rows(nt_ip_unc_rep_raw_plot) %>%
  bind_rows(nt_ip_con_rep_raw_plot) %>%
  mutate(dv = factor(dv, levels = label_levels)) -> raw_rep_plot

g1 = rep_plot %>%
  ggplot(aes(repetitions, Estimate, ymin=Q2.5, ymax=Q97.5)) +
  geom_point(mapping=aes(x=repetitions, y=yi, size=n), shape=4, alpha=.3, data=raw_rep_plot, inherit.aes = FALSE) +
  geom_smooth(stat='identity', colour='black') +
  facet_grid(model~dv) + 
  xlab('Number of Repetitions') +
  ylab('SIF (%)') +
  scale_size(range=c(.05, 2)) +
  guides(size='none') + apatheme

ggsave(g1, file='figures/Figure 5 Repetitions.tiff', width=9, height=4, dpi=400)
ggsave(g1, file='figures/Figure 5 Repetitions.png', width=9, height=4, dpi=400)


# Model Comparison Code ---------------------------------------------------

# Takes a long time, but in case of interest
# nt_sp_unc_sm_loo = loo(nt_sp_unc_rep_smooth, nt_sp_unc_rep_lin)
# nt_sp_unc_sm_kfold = kfold(nt_sp_unc_rep_smooth, nt_sp_unc_rep_lin, group='id', folds='grouped')
# nt_sp_con_sm_loo = loo(nt_sp_con_rep_smooth, nt_sp_con_rep_lin)
# nt_sp_con_sm_kfold = kfold(nt_sp_con_rep_smooth, nt_sp_con_rep_lin, group='id', folds='grouped')
# nt_ip_unc_sm_loo = loo(nt_ip_unc_rep_smooth, nt_ip_unc_rep_lin)
# nt_ip_con_sm_loo = loo(nt_ip_con_rep_smooth, nt_ip_con_rep_lin)
