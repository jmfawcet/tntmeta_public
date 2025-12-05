
# This script is sadly quite messy, as creating figures of this nature
# involves working with internal elements used by ggplot

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
        legend.position='top')

plot_custom_colors <- c(
  'GoldenDream' = "#EBCC2A",
  'BostonBlue'  = "#3B9AB2",
  'Red'         = '#8d0f4a',
  'Black'       = "#000000")

generate_article_estimates = function(m, dat, label = "")
{
  dat_articles = dat %>%
    select(id, nt_instruct, citation) %>%
    distinct() %>%
    mutate(sei=0)
  
  m %>%
    posterior_linpred(newdata=dat_articles, re.form=~(1|id)) %>%
    posterior_summary() %>%
    bind_cols(dat_articles) %>%
    mutate(facet_labels = str_to_title(nt_instruct), facet_labels=ifelse(facet_labels=='Undifferentiated', 'Unspecified', facet_labels)) %>%
    mutate(label = label) -> dat_articles
  
  return(dat_articles)
}

calculate_aggregate_estimates = function(m1, m2, label = "")
{
  # Overall Estimates
  pi_overall_study = m1 %>%
    posterior_linpred(newdata=data.frame(sei=0, id=9999), re.form=~(1|id), allow_new_levels=TRUE) %>%
    posterior_summary() %>%
    data.frame() %>%
    select(PI_S_Q2.5 = Q2.5, PI_S_Q97.5 = Q97.5)
  
  pi_overall_study_and_contrast = m1 %>%
    posterior_linpred(newdata=data.frame(sei=0, id=9999, es_id=9999), allow_new_levels=TRUE) %>%
    posterior_summary() %>%
    data.frame() %>%
    select(PI_SC_Q2.5 = Q2.5, PI_SC_Q97.5 = Q97.5)
  
  c_overall = m1 %>%
    fixef() %>%
    bind_cols(pi_overall_study, pi_overall_study_and_contrast) %>%
    mutate(citation='Overall')
  
  # Instruction Estimates
  pi_inst_study = m2 %>%
    posterior_linpred(newdata=data.frame(sei=0, nt_instruct = instruction_levels, id=9999), re.form=~(1|id), allow_new_levels=TRUE) %>%
    posterior_summary() %>%
    data.frame() %>%
    select(PI_S_Q2.5 = Q2.5, PI_S_Q97.5 = Q97.5)
  
  pi_inst_study_and_contrast = m2 %>%
    posterior_linpred(newdata=data.frame(sei=0, nt_instruct = instruction_levels, id=9999, es_id=9999), allow_new_levels=TRUE) %>%
    posterior_summary() %>%
    data.frame() %>%
    select(PI_SC_Q2.5 = Q2.5, PI_SC_Q97.5 = Q97.5)
  
  c_inst = m2 %>%
    fixef() %>%
    bind_cols(pi_inst_study, pi_inst_study_and_contrast) %>%
    mutate(citation=instruction_levels)
  
  # Combine
  return(c_inst %>%
    bind_rows(c_overall) %>%
      mutate(citation = ifelse(citation == 'undifferentiated', 'Unspecified', citation),
             citation = str_to_title(citation),
             nt_instruct = citation,
             facet_labels = 'Summary') %>%
      mutate(label = factor(label, levels=label_levels)))
}


# Important Variables -----------------------------------------------------

plot_vars = vars(id, new_id, es_id, citation, year, nt_instruct, n, yi)
label_levels = c('Same Probe (U)', 'Same Probe (C)', 'Independent Probe (U)', 'Independent Probe (C)')
instruction_levels = c('undifferentiated', 'direct suppression', 'thought substitution')
instruction_facet_levels = c('Direct Suppression', 'Thought Substitution', 'Unspecified', 'Summary')
citation_levels = c('Direct Suppression', 'Thought Substitution', 'Unspecified', 'Overall')

# Create Raw Data Sets for Each Model -------------------------------------

nt_sp_unc_dat_inst_dat = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall') %>%
  select_at(plot_vars) %>%
  mutate(nt_instruct = relevel(factor(nt_instruct), 'undifferentiated')) %>%
  mutate(label='Same Probe (U)')

nt_sp_con_dat_inst_dat = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall') %>%
  select_at(plot_vars) %>%
  mutate(nt_instruct = relevel(factor(nt_instruct), 'undifferentiated'))  %>%
  mutate(label='Same Probe (C)')

nt_ip_unc_dat_inst_dat = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall') %>%
  select_at(plot_vars) %>%
  mutate(nt_instruct = relevel(factor(nt_instruct), 'undifferentiated'))  %>%
  mutate(label='Independent Probe (U)')

nt_ip_con_dat_inst_dat = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall') %>%
  select_at(plot_vars) %>%
  mutate(nt_instruct = relevel(factor(nt_instruct), 'undifferentiated'))  %>%
  mutate(label='Independent Probe (C)')

comb_dat = bind_rows(nt_sp_unc_dat_inst_dat, nt_sp_con_dat_inst_dat, 
                     nt_ip_unc_dat_inst_dat, nt_ip_con_dat_inst_dat) %>%
  mutate(facet_labels = str_to_title(nt_instruct), facet_labels=ifelse(facet_labels=='Undifferentiated', 'Unspecified', facet_labels)) %>%
  mutate(label = factor(label, levels=label_levels), facet_labels = factor(facet_labels, levels=instruction_facet_levels))

citation_levels = comb_dat %>%
  select(year, citation) %>%
  distinct() %>%
  arrange(year, citation) %>%
  pull(citation) %>%
  c('Direct Suppression', 'Thought Substitution', 'Unspecified', 'Overall') %>%
  rev()

comb_dat = comb_dat %>%
  mutate(citation = factor(citation, levels=citation_levels))

# Get Article Estimates ---------------------------------------------------

nt_sp_unc_inst_articles = generate_article_estimates(nt_sp_unc_inst_cat, nt_sp_unc_dat_inst_dat, label='Same Probe (U)')
nt_sp_con_inst_articles = generate_article_estimates(nt_sp_con_inst_cat, nt_sp_con_dat_inst_dat, label='Same Probe (C)')
nt_ip_unc_inst_articles = generate_article_estimates(nt_ip_unc_inst_cat, nt_ip_unc_dat_inst_dat, label='Independent Probe (U)')
nt_ip_con_inst_articles = generate_article_estimates(nt_ip_con_inst_cat, nt_ip_con_dat_inst_dat, label='Independent Probe (C)')

comb_inst_articles = bind_rows(nt_sp_unc_inst_articles, nt_sp_con_inst_articles, nt_ip_unc_inst_articles, nt_ip_con_inst_articles) %>%
  mutate(points = facet_labels)  %>%
  mutate(label = factor(label, levels=label_levels), facet_labels = factor(facet_labels, levels=instruction_facet_levels)) %>%
  mutate(citation = factor(citation, levels=citation_levels))


# Aggregate Estimates -----------------------------------------------------

nt_sp_unc_agg = calculate_aggregate_estimates(nt_sp_unc, nt_sp_unc_inst_cat, label='Same Probe (U)')
nt_sp_con_agg = calculate_aggregate_estimates(nt_sp_con, nt_sp_con_inst_cat, label='Same Probe (C)')
nt_ip_unc_agg = calculate_aggregate_estimates(nt_ip_unc, nt_ip_unc_inst_cat, label='Independent Probe (U)')
nt_ip_con_agg = calculate_aggregate_estimates(nt_ip_con, nt_ip_con_inst_cat, label='Independent Probe (C)')

comb_agg = bind_rows(nt_sp_unc_agg, nt_sp_con_agg, nt_ip_unc_agg, nt_ip_con_agg) %>%
  mutate(label = factor(label, levels=label_levels), 
         citation = factor(citation, levels=citation_levels),
         facet_labels = factor(facet_labels, levels=instruction_facet_levels)) 
  

# MAIN FOREST PLOT --------------------------------------------------------
point_size =.8

add_manual_legend <- function(
    title  = "Instruction Type",
    levels = c("Direct Suppression","Thought Substitution","Unspecified","Overall"),
    shapes = c("Direct Suppression"=21, "Thought Substitution"=22, "Unspecified"=24, "Overall"=25),
    fills  = c("Direct Suppression"="#EBCC2A", "Thought Substitution"="#3B9AB2",
               "Unspecified"="#8d0f4a", "Overall"="grey"),
    key_size = 3, order = 1
){
  dummy <- data.frame(type = levels, x = 0, y = 0)
  list(
    ggplot2::geom_point(
      data = dummy,
      ggplot2::aes(x = x, y = y, shape = type, fill = type),
      color = "black", size = key_size, alpha = 0, inherit.aes = FALSE, show.legend = TRUE
    ),
    ggplot2::scale_shape_manual(
      name = title, values = shapes, breaks = levels, labels = levels,
      guide = ggplot2::guide_legend(order = order)
    ),
    ggplot2::scale_fill_manual(
      name = title, values = fills, breaks = levels, labels = levels,
      guide = ggplot2::guide_legend(order = order)
    )
  )
}

g1 = ggplot(comb_inst_articles, aes(y=citation, x=Estimate, xmin=Q2.5, xmax=Q97.5))+
  # Add CI bars
  geom_errorbarh(height=0, lwd=.3) +
  geom_errorbarh(data=comb_agg, height=0, lwd=.6, mapping=aes(xmin=Q2.5, xmax=Q97.5)) +
  geom_errorbarh(data=comb_agg, height=0, lwd=.3, mapping=aes(xmin=PI_S_Q2.5, xmax=PI_S_Q97.5)) +
  geom_errorbarh(data=comb_agg, height=0, lwd=.15, mapping=aes(xmin=PI_SC_Q2.5, xmax=PI_SC_Q97.5)) +
  geom_vline(xintercept=0, color='black', linetype='dashed') +
  # Add data points and color-code them; set point size based on the sample size of the study
  geom_point(data=subset(comb_inst_articles, points=='Direct Suppression'), color=plot_custom_colors[4], fill=plot_custom_colors[1], shape=21, size=point_size)+
  geom_point(data=subset(comb_inst_articles, points=='Thought Substitution'), color=plot_custom_colors[4], fill=plot_custom_colors[2], shape=22, size=point_size)+
  geom_point(data=subset(comb_inst_articles, points=='Unspecified'), color=plot_custom_colors[4], fill=plot_custom_colors[3], shape=24, size=point_size)+
  geom_point(data=subset(comb_agg, citation=='Direct Suppression'), color=plot_custom_colors[4], fill=plot_custom_colors[1], shape=21, size=point_size)+
  geom_point(data=subset(comb_agg, citation=='Thought Substitution'), color=plot_custom_colors[4], fill=plot_custom_colors[2], shape=22, size=point_size)+
  geom_point(data=subset(comb_agg, citation=='Unspecified'), color=plot_custom_colors[4], fill=plot_custom_colors[3], shape=24, size=point_size)+
  geom_point(data=subset(comb_agg, citation=='Overall'), color=plot_custom_colors[4], fill='grey', shape=25, size=point_size)+
  # Specify the limits of the x-axis and relabel it
  scale_x_continuous(limits=c(-10,25), name='SIF (%)', breaks=seq(-10,25,5)) +
  geom_point(data=comb_dat, aes(y=citation, x=yi, size=n), pch=4, alpha=.75, inherit.aes = FALSE)+
  # Give y-axis a meaningful label
  ylab('')+
  scale_size(range=c(.05, 2)) + 
  # Create sub-plots (i.e., facets) based on levels of instructions
  # Allow unique axes (references don't repeat)
  facet_grid(facet_labels~label, scales= 'free', space='free')+
  # Apply custom APA theme
  apatheme + add_manual_legend() + guides(
    fill = guide_legend(
      title = "",
      override.aes = list(
        shape  = c(21, 22, 24, 25),
        fill   = c("#EBCC2A", "#3B9AB2", "#8d0f4a", "grey"),
        colour = "black", alpha = 1, size = 3, stroke = 0.9
      )
    ),
    shape = "none",
    size = 'none'
  )


# Apply different colors to instructions legend on the right
PCP <- ggplot_gtable(ggplot_build(g1))
strip_both <- which(grepl('strip-', PCP$layout$name))
fills <- c(plot_custom_colors[1],plot_custom_colors[2],plot_custom_colors[3],"grey")
k <- 1

for (i in strip_both[-1:-4]) {
  j <- which(grepl('rect', PCP$grobs[[i]]$grobs[[1]]$childrenOrder))
  PCP$grobs[[i]]$grobs[[1]]$children[[j]]$gp$fill <- fills[k]
  PCP$grobs[[i]]$grobs[[1]]$children[[j]]$gp$alpha <- 0.6
  k <- k+1
}
PCP$heights[16] = unit(6, 'null')
plot(PCP)

ggsave(PCP, file='figures/Figure 3 Main Forest.tiff', width=9, height=11, dpi=400)
ggsave(PCP, file='figures/Figure 3 Main Forest.pdf', width=9, height=11, dpi=400)

