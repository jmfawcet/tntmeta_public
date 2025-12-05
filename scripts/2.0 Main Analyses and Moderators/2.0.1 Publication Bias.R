source('scripts/Final Scripts/1.0 Process Data.R')

# All publication models are separated based on test type (SP vs IP) and conditionalization (unconditionalized vs conditionalized) of the data. 
# This means for all tests of publication bias 4 models were run
# We were unable to run publication bias models including recognition data due to insufficient data

# Exclude Clinical Controls -----------------------------------------------

nt_sp_dat = nt_sp_dat %>% filter(include==1)
nt_ip_dat = nt_ip_dat %>% filter(include==1)

# SP UNC (Egger's Test) ----------------------------------------------------------

# The following 4 models are used to run an Egger's test by assessing if the observed SIF effects are moderated by standard error. 

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall')

nt_sp_unc_pub = brm(bf(yi | se(sei) ~ scale(sei) + (1|id) + (1|new_id)), 
                    data = temp, iter = 20000, warmup = 5000, cores = 8, backend='cmdstanr',
                    control=list(adapt_delta=.99, max_treedepth=15),
                    prior=c(rm_priors, rm_b_priors), 
                    file = 'models/nt_sp_unc_bias', file_refit = "on_change")

hypothesis(nt_sp_unc_pub, 'scalesei > 0') #Test of confidence in the direction of the relationship

funnel(temp$yi, sei=temp$sei, level=c(90, 95, 99), refline=0) #Produces funnel plot for visual inspection to supplement statistical models.

# IP UNC (Egger's Test) ------------------------------------------------------------------

temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes')

nt_ip_unc_pub = brm(bf(yi | se(sei) ~ scale(sei) + (1|id) + (1|new_id)), 
                    data = temp, iter = 20000, warmup = 5000, cores = 8, backend='cmdstanr',
                    control=list(adapt_delta=.99, max_treedepth=15),
                    prior=c(rm_priors, rm_b_priors), 
                    file = 'models/nt_ip_unc_bias', file_refit = "on_change")

hypothesis(nt_ip_unc_pub, 'scalesei > 0') #Test of confidence in the direction of the relationship

funnel(temp$yi, sei=temp$sei, level=c(90, 95, 99), refline=0) #Produces funnel plot for visual inspection to supplement statistical models.

# SP CON (Egger's Test) ------------------------------------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall')

nt_sp_con_pub = brm(bf(yi | se(sei) ~ scale(sei) + (1|id) + (1|new_id)), 
                    data = temp, iter = 10000, warmup = 5000, cores = 8, backend='cmdstanr',
                    control=list(adapt_delta=.99, max_treedepth=15),
                    prior=c(rm_priors, rm_b_priors), 
                    file = 'models/nt_sp_con_bias', file_refit = "on_change")

hypothesis(nt_sp_con_pub, 'scalesei > 0') #Test of confidence in the direction of the relationship

funnel(temp$yi, sei=temp$sei, level=c(90, 95, 99), refline=0) #Produces funnel plot for visual inspection to supplement statistical models.

# IP CON (Egger's Test) ------------------------------------------------------------------

temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no')

nt_ip_con_pub = brm(bf(yi | se(sei) ~ scale(sei) + (1|id) + (1|new_id)), 
                    data = temp, iter = 10000, warmup = 5000, cores = 8, backend='cmdstanr',
                    control=list(adapt_delta=.99, max_treedepth=15),
                    prior=c(rm_priors, rm_b_priors), 
                    file = 'models/nt_ip_con_bias', file_refit = "on_change")

hypothesis(nt_ip_con_pub, 'scalesei > 0') #Test of confidence in the direction of the relationship

funnel(temp$yi, sei=temp$sei, level=c(90, 95, 99), refline=0) #Produces funnel plot for visual inspection to supplement statistical models.

# SP UNC (Macaskill's Method.) ----------------------------------------------------------

# The following 4 models are used to assess if the observed SIF effects are moderated by sample size. This is sometimes reffered to as Macaskill's Method.

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes', dv=='recall')

nt_sp_unc_pub_N = brm(bf(yi | se(sei) ~ scale(n) + (1|id) + (1|new_id)), 
                      data = temp, iter = 10000, warmup = 5000, cores = 8, backend='cmdstanr',
                      control=list(adapt_delta=.99, max_treedepth=15),
                      prior=c(rm_priors, rm_b_priors), 
                      file = 'models/nt_sp_unc_bias_N', file_refit = "on_change")

hypothesis(nt_sp_unc_pub_N, 'scalen > 0') #Test of confidence in the direction of the relationship

funnel(temp$yi, sei=temp$sei, ni=temp$n, yaxis='ni') #Produces funnel plot for visual inspection to supplement statistical models.

# IP UNC (Macaskill's Method.) ------------------------------------------------------------------

temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='yes')

nt_ip_unc_pub_N = brm(bf(yi | se(sei) ~ scale(n) + (1|id) + (1|new_id)), 
                      data = temp, iter = 10000, warmup = 5000, cores = 8, backend='cmdstanr',
                      control=list(adapt_delta=.99, max_treedepth=15),
                      prior=c(rm_priors, rm_b_priors), 
                      file = 'models/nt_ip_unc_bias_N', file_refit = "on_change")

hypothesis(nt_ip_unc_pub_N, 'scalen < 0') #Test of confidence in the direction of the relationship

funnel(temp$yi, sei=temp$sei, ni=temp$n, yaxis='ni') #Produces funnel plot for visual inspection to supplement statistical models.

# SP CON (Macaskill's Method.) ------------------------------------------------------------------

temp = nt_sp_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no', dv=='recall')

nt_sp_con_pub_N = brm(bf(yi | se(sei) ~ scale(n) + (1|id) + (1|new_id)), 
                      data = temp, iter = 10000, warmup = 5000, cores = 8, backend='cmdstanr',
                      control=list(adapt_delta=.99, max_treedepth=15),
                      prior=c(rm_priors, rm_b_priors), 
                      file = 'models/nt_sp_con_bias_N', file_refit = "on_change")

hypothesis(nt_sp_con_pub_N, 'scalen > 0') #Test of confidence in the direction of the relationship

funnel(temp$yi, sei=temp$sei, ni=temp$n, yaxis='ni') #Produces funnel plot for visual inspection to supplement statistical models.

# IP CON (Macaskill's Method.) ------------------------------------------------------------------

temp = nt_ip_dat %>% 
  filter(repetitions==max_rep, is_conditionalized!='no')

nt_ip_con_pub_N = brm(bf(yi | se(sei) ~ scale(n) + (1|id) + (1|new_id)), 
                      data = temp, iter = 10000, warmup = 5000, cores = 8, backend='cmdstanr',
                      control=list(adapt_delta=.99, max_treedepth=15),
                      prior=c(rm_priors, rm_b_priors), 
                      file = 'models/nt_ip_con_bias_N', file_refit = "on_change")

hypothesis(nt_ip_con_pub_N, 'scalen > 0') #Test of confidence in the direction of the relationship

funnel(temp$yi, sei=temp$sei, ni=temp$n, yaxis='ni') #Produces funnel plot for visual inspection to supplement statistical models.

# p-curve models -----------------------------------------------------

# Install p-curve package if not already present 

#remotes::install_github("MathiasHarrer/dmetar")

library(dmetar) #Open p-curve package

#SP Unconditionalized

temp = nt_sp_dat %>% 
  filter(is_conditionalized!='yes', dv=='recall', include_pubbias == 1) #Filter based on include_pubbias to avoid including the same effects multiple times

temp = temp %>% select(id, plot_citation, yi, vi, n) #Remove unnecessary variables

temp = temp %>% mutate(studlab = plot_citation, TE = yi, seTE = sqrt(vi)) #Rename variables to fit what pcurve function expects

pcurve(x = temp) #Run p-curve model, produces all values needed to interpret model

#IP Unconditionalized

temp = nt_ip_dat %>% 
  filter(is_conditionalized!='yes', dv=='recall', include_pubbias == 1) #Filter based on include_pubbias to avoid including the same effects multiple times

temp = temp %>% select(id, plot_citation, yi, vi, n) #Remove unnecessary variables

temp = temp %>% mutate(studlab = plot_citation, TE = yi, seTE = sqrt(vi)) #Rename variables to fit what pcurve function expects

pcurve(x = temp) #Run p-curve model, produces all values needed to interpret model

#SP Conditionalized

temp = nt_sp_dat %>% 
  filter(is_conditionalized!='no', dv=='recall', include_pubbias == 1) #Filter based on include_pubbias to avoid including the same effects multiple times

temp = temp %>% select(id, plot_citation, yi, vi, n) #Remove unnecessary variables

temp = temp %>% mutate(studlab = plot_citation, TE = yi, seTE = sqrt(vi)) #Rename variables to fit what pcurve function expects

pcurve(x = temp) #Run p-curve model, produces all values needed to interpret model

#IP Conditionalized

temp = nt_ip_dat %>% 
  filter(is_conditionalized!='no', dv=='recall', include_pubbias == 1) #Filter based on include_pubbias to avoid including the same effects multiple times

temp = temp %>% select(id, plot_citation, yi, vi, n) #Remove unnecessary variables

temp = temp %>% mutate(studlab = plot_citation, TE = yi, seTE = sqrt(vi)) #Rename variables to fit what pcurve function expects

pcurve(x = temp) #Run p-curve model, produces all values needed to interpret model



# z-curve models-----

library(zcurve) #open zcurve package

# SP Unconditionalized
temp <- nt_sp_dat %>%
  filter(dv == 'recall', repetitions == max_rep, is_conditionalized != "yes") %>%
  mutate(z = yi/sqrt(vi), sid = paste0(citation, sample_id)) %>%
  mutate(z = paste0('z=', z))

# Modeling method reported in the paper, effects are clustered based on study

zdat <- zcurve_data(data = temp$z, id = temp$citation)
results <- zcurve_clustered(zdat, method = 'b', bootstrap = 5e3)
plot.zcurve(results, extrapolate = TRUE, CI = TRUE, annotation = TRUE)
summary(results)

# Alternative method of clustering data based on sample ID instead of by study, left in for anyone who would like to compare to the method we chose to use

#zdat <- zcurve_data(data = temp$z, id = temp$sid)
#results <- zcurve_clustered(zdat, method = 'b', bootstrap = 5e3)
#plot.zcurve(results, extrapolate = TRUE, CI = TRUE, annotation = TRUE)
#summary(results)

# SP Conditionalized
temp <- nt_sp_dat %>%
  filter(dv == 'recall', repetitions == max_rep, is_conditionalized != "no") %>%
  mutate(z = yi/sqrt(vi), sid = paste0(citation, sample_id)) %>%
  mutate(z = paste0('z=', z))

# Modeling method reported in the paper, effects are clustered based on study

zdat <- zcurve_data(data = temp$z, id = temp$citation)
results <- zcurve_clustered(zdat, method = 'b', bootstrap = 5e3)
plot.zcurve(results, extrapolate = TRUE, CI = TRUE, annotation = TRUE)
summary(results)

# Alternative method of clustering data based on sample ID instead of by study, left in for anyone who would like to compare to the method we chose to use

#zdat <- zcurve_data(data = temp$z, id = temp$sid)
#results <- zcurve_clustered(zdat, method = 'b', bootstrap = 5e3)
#plot.zcurve(results, extrapolate = TRUE, CI = TRUE, annotation = TRUE)
#summary(results)

# IP Unconditionalized
temp <- nt_ip_dat %>%
  filter(dv == 'recall', repetitions == max_rep, is_conditionalized != "yes") %>%
  mutate(z = yi/sqrt(vi), sid = paste0(citation, sample_id)) %>%
  mutate(z = paste0('z=', z))

# Modeling method reported in the paper, effects are clustered based on study

zdat <- zcurve_data(data = temp$z, id = temp$citation)
results <- zcurve_clustered(zdat, method = 'b', bootstrap = 5e3)
plot.zcurve(results, extrapolate = TRUE, CI = TRUE, annotation = TRUE)
summary(results)

# Alternative method of clustering data based on sample ID instead of by study, left in for anyone who would like to compare to the method we chose to use

#zdat <- zcurve_data(data = temp$z, id = temp$sid)
#results <- zcurve_clustered(zdat, method = 'b', bootstrap = 5e3)
#plot.zcurve(results, extrapolate = TRUE, CI = TRUE, annotation = TRUE)
#summary(results)

# IP Conditionalized
temp <- nt_ip_dat %>%
  filter(dv == 'recall', repetitions == max_rep, is_conditionalized != "no") %>%
  mutate(z = yi/sqrt(vi), sid = paste0(citation, sample_id)) %>%
  mutate(z = paste0('z=', z))

# Modeling method reported in the paper, effects are clustered based on study

zdat <- zcurve_data(data = temp$z, id = temp$citation)
results <- zcurve_clustered(zdat, method = 'b', bootstrap = 5e3)
plot.zcurve(results, extrapolate = TRUE, CI = TRUE, annotation = TRUE)
summary(results)

# Alternative method of clustering data based on sample ID instead of by study, left in for anyone who would like to compare to the method we chose to use

#zdat <- zcurve_data(data = temp$z, id = temp$sid)
#results <- zcurve_clustered(zdat, method = 'b', bootstrap = 5e3)
#plot.zcurve(results, extrapolate = TRUE, CI = TRUE, annotation = TRUE)
#summary(results)



#RoBMA models----

library(RoBMA) #Open RoBMA package


# SP Unconditionalized

temp <- nt_sp_dat %>% 
  filter(dv == "recall", repetitions == max_rep, include_pubbias == 1, is_conditionalized != "yes")

# Run the model

fit <- RoBMA(d = temp$yi_s, 
             se = temp$sei_s, 
             study_names = temp$plot_citation,
             seed = 1,
             priors_effect_null = NULL,
             priors_heterogeneity_null = NULL, 
             priors_effect = prior("normal", 
                                   parameters = list(mean = 0, 
                                                     sd = 0.5)),
             priors_heterogeneity = prior("normal", 
                                          parameters = list(mean = 0, 
                                                            sd = 0.25)),
             priors_bias = list(
               prior_weightfunction(distribution = "one.sided", 
                                    parameters = list(alpha = c(1, 1), 
                                                      steps = c(0.05))),
               prior_weightfunction(distribution = "one.sided", 
                                    parameters = list(alpha = c(1, 1, 1), 
                                                      steps = c(0.05, 0.1)))),
             autofit_control = set_autofit_control(max_Rhat = 1.01,
                                                   min_ESS = 1000,
                                                   sample_extend = 1e4),
             convergence_checks = set_convergence_checks(max_Rhat = 1.01,
                                                         min_ESS = 1000),
             parallel = TRUE)

summary(fit) #Checks the general results of the ROBMA model
summary(fit, type = 'model') #Checks the specific results of the ROBMA models we are interested in
interpret(fit) #Provides a summary of how the creators of the RoBMA package would interpret our results

# SP Conditionalized

temp <- nt_sp_dat %>% 
  filter(dv == "recall", repetitions == max_rep, include_pubbias == 1, is_conditionalized != "no")

# Run the model

fit <- RoBMA(d = temp$yi_s, 
             se = temp$sei_s, 
             study_names = temp$plot_citation,
             seed = 1,
             priors_effect_null = NULL,
             priors_heterogeneity_null = NULL, 
             priors_effect = prior("normal", 
                                   parameters = list(mean = 0, 
                                                     sd = 0.5)),
             priors_heterogeneity = prior("normal", 
                                          parameters = list(mean = 0, 
                                                            sd = 0.25)),
             priors_bias = list(
               prior_weightfunction(distribution = "one.sided", 
                                    parameters = list(alpha = c(1, 1), 
                                                      steps = c(0.05))),
               prior_weightfunction(distribution = "one.sided", 
                                    parameters = list(alpha = c(1, 1, 1), 
                                                      steps = c(0.05, 0.1)))),
             autofit_control = set_autofit_control(max_Rhat = 1.01,
                                                   min_ESS = 1000,
                                                   sample_extend = 1e4),
             convergence_checks = set_convergence_checks(max_Rhat = 1.01,
                                                         min_ESS = 1000),
             parallel = TRUE)

summary(fit) #Checks the general results of the ROBMA model
summary(fit, type = 'model') #Checks the specific results of the ROBMA models we are interested in
interpret(fit) #Provides a summary of how the creators of the RoBMA package would interpret our results

# IP Unconditionalized

temp <- nt_ip_dat %>% 
  filter(dv == "recall", repetitions == max_rep, include_pubbias == 1, is_conditionalized != "yes")

# Run the model

fit <- RoBMA(d = temp$yi_s, 
             se = temp$sei_s, 
             study_names = temp$plot_citation,
             seed = 1,
             priors_effect_null = NULL,
             priors_heterogeneity_null = NULL, 
             priors_effect = prior("normal", 
                                   parameters = list(mean = 0, 
                                                     sd = 0.5)),
             priors_heterogeneity = prior("normal", 
                                          parameters = list(mean = 0, 
                                                            sd = 0.25)),
             priors_bias = list(
               prior_weightfunction(distribution = "one.sided", 
                                    parameters = list(alpha = c(1, 1), 
                                                      steps = c(0.05))),
               prior_weightfunction(distribution = "one.sided", 
                                    parameters = list(alpha = c(1, 1, 1), 
                                                      steps = c(0.05, 0.1)))),
             autofit_control = set_autofit_control(max_Rhat = 1.01,
                                                   min_ESS = 1000,
                                                   sample_extend = 1e4),
             convergence_checks = set_convergence_checks(max_Rhat = 1.01,
                                                         min_ESS = 1000),
             parallel = TRUE)

summary(fit) #Checks the general results of the ROBMA model
summary(fit, type = 'model') #Checks the specific results of the ROBMA models we are interested in
interpret(fit) #Provides a summary of how the creators of the RoBMA package would interpret our results

# IP Conditionalized

temp <- nt_ip_dat %>% 
  filter(dv == "recall", repetitions == max_rep, include_pubbias == 1, is_conditionalized != "no")

# Run the model

fit <- RoBMA(d = temp$yi_s, 
             se = temp$sei_s, 
             study_names = temp$plot_citation,
             seed = 1,
             priors_effect_null = NULL,
             priors_heterogeneity_null = NULL, 
             priors_effect = prior("normal", 
                                   parameters = list(mean = 0, 
                                                     sd = 0.5)),
             priors_heterogeneity = prior("normal", 
                                          parameters = list(mean = 0, 
                                                            sd = 0.25)),
             priors_bias = list(
               prior_weightfunction(distribution = "one.sided", 
                                    parameters = list(alpha = c(1, 1), 
                                                      steps = c(0.05))),
               prior_weightfunction(distribution = "one.sided", 
                                    parameters = list(alpha = c(1, 1, 1), 
                                                      steps = c(0.05, 0.1)))),
             autofit_control = set_autofit_control(max_Rhat = 1.01,
                                                   min_ESS = 1000,
                                                   sample_extend = 1e4),
             convergence_checks = set_convergence_checks(max_Rhat = 1.01,
                                                         min_ESS = 1000),
             parallel = TRUE)

summary(fit) #Checks the general results of the ROBMA model
summary(fit, type = 'model') #Checks the specific results of the ROBMA models we are interested in
interpret(fit) #Provides a summary of how the creators of the RoBMA package would interpret our results

