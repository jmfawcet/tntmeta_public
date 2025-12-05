
# Functions and Themes ----------------------------------------------------

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
        strip.text.y = element_text(size = 5.5, colour = 'black', face='bold'),
        legend.position='none')

plot_custom_colors <- c(
  'GoldenDream' = "#EBCC2A",
  'BostonBlue'  = "#3B9AB2",
  'Red'         = '#8d0f4a',
  'Black'       = "#000000")

generate_article_estimates = function(m, dat, label = "")
{
  dat_articles = dat %>%
    select(id, citation) %>%
    distinct() %>%
    mutate(sei=0)
  
  m %>%
    posterior_linpred(newdata=dat_articles, re.form=~(1|id)) %>%
    posterior_summary() %>%
    bind_cols(dat_articles) %>%
    mutate(label = label) -> dat_articles
  
  return(dat_articles)
}

calculate_aggregate_estimates = function(m, label = "")
{
  # Overall Estimates
  pi_overall_study = m %>%
    posterior_linpred(newdata=data.frame(sei=0, id=9999), re.form=~(1|id), allow_new_levels=TRUE) %>%
    posterior_summary() %>%
    data.frame() %>%
    select(PI_S_Q2.5 = Q2.5, PI_S_Q97.5 = Q97.5)
  
  pi_overall_study_and_contrast = m %>%
    posterior_linpred(newdata=data.frame(sei=0, id=9999, es_id=9999), allow_new_levels=TRUE) %>%
    posterior_summary() %>%
    data.frame() %>%
    select(PI_SC_Q2.5 = Q2.5, PI_SC_Q97.5 = Q97.5)
  
  c_overall = m %>%
    fixef() %>%
    bind_cols(pi_overall_study, pi_overall_study_and_contrast) %>%
    mutate(citation='Overall')
  
  # Combine
  return(c_overall %>%
      mutate(citation = 'Overall') %>%
      mutate(label = factor(label, levels=label_levels)))
}


# Important Variables -----------------------------------------------------

plot_vars = vars(id, new_id, es_id, citation, year, nt_instruct, n, yi)
label_levels = c('Recognition (U)', 'Recognition (C)')

# Create Raw Data Sets for Each Model -------------------------------------

nt_re_unc_dat_dat = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv =='recognition') %>%
  select_at(plot_vars) %>%
  mutate(label='Recognition (U)')

nt_re_con_dat_dat = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv =='recognition') %>%
  select_at(plot_vars) %>%
  mutate(label='Recognition (C)')

comb_dat = bind_rows(nt_re_unc_dat_dat, nt_re_con_dat_dat) %>%
  mutate(label = factor(label, levels=label_levels))

citation_levels = comb_dat %>%
  select(year, citation) %>%
  distinct() %>%
  arrange(year, citation) %>%
  pull(citation) %>%
  c('Overall') %>% rev()

comb_dat = comb_dat %>%
  mutate(citation = factor(citation, levels=citation_levels))

# Get Article Estimates ---------------------------------------------------

nt_re_unc_articles = generate_article_estimates(nt_sp_unc_recog, nt_re_unc_dat_dat, label='Recognition (U)')
nt_re_con_articles = generate_article_estimates(nt_sp_con_recog, nt_re_con_dat_dat, label='Recognition (C)')

comb_articles = bind_rows(nt_re_unc_articles, nt_re_con_articles, data.frame(citation='Overall', label='Recognition (U)')) %>%
  mutate(label = factor(label, levels=label_levels)) %>%
  mutate(citation = factor(citation, levels=citation_levels))


# Aggregate Estimates -----------------------------------------------------

nt_re_unc_agg = calculate_aggregate_estimates(nt_sp_unc_recog, label='Recognition (U)')
nt_re_con_agg = calculate_aggregate_estimates(nt_sp_con_recog, label='Recognition (C)')

comb_agg = bind_rows(nt_re_unc_agg, nt_re_con_agg) %>%
  mutate(label = factor(label, levels=label_levels), 
         citation = factor(citation, levels=citation_levels)) 
  

# MAIN FOREST PLOT --------------------------------------------------------
point_size =.8

g1 = ggplot(comb_articles, aes(y=citation, x=Estimate, xmin=Q2.5, xmax=Q97.5))+
  # Add CI bars
  geom_errorbarh(height=0, lwd=.3) +
  geom_errorbarh(data=comb_agg, height=0, lwd=.6, mapping=aes(xmin=Q2.5, xmax=Q97.5)) +
  geom_errorbarh(data=comb_agg, height=0, lwd=.3, mapping=aes(xmin=PI_S_Q2.5, xmax=PI_S_Q97.5)) +
  geom_errorbarh(data=comb_agg, height=0, lwd=.15, mapping=aes(xmin=PI_SC_Q2.5, xmax=PI_SC_Q97.5)) +
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  # Add data points and color-code them; set point size based on the sample size of the study
  geom_point(data=comb_articles, color=plot_custom_colors[4], fill='grey', shape=21, size=point_size) +
  geom_point(data=comb_agg, color=plot_custom_colors[4], fill='grey', shape=21, size=point_size)+
  # Specify the limits of the x-axis and relabel it
  scale_x_continuous(limits=c(-20,30), name='SIF (%)', breaks=seq(-20,30,10)) +
  geom_point(data=comb_dat, aes(y=citation, x=yi, size=n), pch=4, alpha=.75, inherit.aes = FALSE)+
  # Give y-axis a meaningful label
  ylab('')+
  scale_size(range=c(.05, 2)) + 
  # Create sub-plots (i.e., facets) based on levels of instructions
  # Allow unique axes (references don't repeat)
  facet_grid(.~label, scales= 'free', space='free')+
  # Apply custom APA theme
  apatheme

g1

ggsave(g1, file='figures/Figure 4 Recognition Forest.tiff', width=5, height=3, dpi=400)
ggsave(g1, file='figures/Figure 4 Recognition Forest.pdf', width=5, height=3, dpi=400)

