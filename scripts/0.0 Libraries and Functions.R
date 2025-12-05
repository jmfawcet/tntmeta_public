
# Load Libraries ----------------------------------------------------------

# Some may no longer be needed as our pipeline evolved, but 
# rather than figure out which I have kept them all
library(openxlsx)     # To read in XLSX files
library(metafor)      # Useful for basic meta-analysis functions
library(brms)         # For Bayesian models
library(tidyverse)    # For data manipulation and cleaning
library(emmeans)      # Certain contrasts

# Load functions ----------------------------------------------------------

# Function to estimate the correlation from a t-test
estimate_r_paired <- function(t, n, m1, m2, sd1, sd2) {
  sdd = abs(m1 - m2) * sqrt(n) / abs(t)
  r = (sd1^2 + sd2^2 - sdd^2) / (2 * sd1 * sd2)
  return(r)
}

# This calculates a meta-analytic estimate of the typical 
# SD of the mean difference
meta_estimate_typical_diff_sd = function(dat)
{
  dat = dat %>%
    select(id, es_id, sei, n)
  temp_sd = escalc(measure = 'SDLN', sdi = sei*n**.5, ni = n, append=TRUE, 
                   data=dat)
  temp_sd_rma = rma.mv(yi, vi, 
                       data=temp_sd,
                       random = list(~ 1 | id, ~ 1 | es_id))
  return(predict(temp_sd_rma, transf=exp)[[1]])
}

# Helper function for the summary function
make_row <- function(x, quantity, scale) {
  x <- as.data.frame(x)
  # brms posterior_summary columns: Estimate, Est.Error, Q2.5, Q97.5
  tibble::tibble(
    Quantity = quantity,
    Scale    = scale,
    Mean     = x$Estimate,
    SE       = x$Est.Error,
    CI_low   = x$Q2.5,
    CI_high  = x$Q97.5
  )
}

# Function to summarize the output of base models
summarize_agg_and_PI <- function(fit, temp_sd, label = NULL, digits = 3) {
  stopifnot(!is.null(temp_sd), is.numeric(temp_sd), length(temp_sd) == 1)
  
  nd <- data.frame(sei = 0, id = 9999, es_id = 9999)
  
  # Calculate aggregates
  agg_raw <- posterior_summary(posterior_epred(fit, newdata = nd, allow_new_levels = TRUE, re.form = NA))
  agg_sd  <- posterior_summary(posterior_epred(fit, newdata = nd, allow_new_levels = TRUE, re.form = NA) / temp_sd)
  
  typ_raw <- posterior_summary(posterior_epred(fit, newdata = nd, allow_new_levels = TRUE, re.form = ~(1 | id)))
  typ_sd  <- posterior_summary(posterior_epred(fit, newdata = nd, allow_new_levels = TRUE, re.form = ~(1 | id)) / temp_sd)
  
  new_raw <- posterior_summary(posterior_epred(fit, newdata = nd, allow_new_levels = TRUE))
  new_sd  <- posterior_summary(posterior_epred(fit, newdata = nd, allow_new_levels = TRUE) / temp_sd)
  ####
  
  out <- dplyr::bind_rows(
    make_row(agg_raw, "Aggregate", "raw"),
    make_row(agg_sd,  "Aggregate", "std"),
    make_row(typ_raw, "Typical contrast, New study (PI)", "raw"),
    make_row(typ_sd,  "Typical contrast, New study (PI)", "std"),
    make_row(new_raw, "New contrast, New study (PI)", "raw"),
    make_row(new_sd,  "New contrast, New study (PI)", "std")
  )
  
  if (!is.null(label)) {
    out <- dplyr::mutate(out, Model = label, .before = 1)
  }
  
  # Optional rounding for nice display
  out <- dplyr::mutate(out,
                       dplyr::across(dplyr::all_of(c("Mean", "SE", "CI_low", "CI_high")),
                                     ~ round(., digits))
  )
  
  out
}

# Priors ------------------------------------------------------------------

rm_priors = c(prior(normal(0, 6), class = "Intercept"), 
              prior(normal(0, 2.5), class = "sd"))

rm_cat_priors = c(prior(normal(0, 6), class = "b"), 
              prior(normal(0, 2.5), class = "sd"))

rm_b_priors = c(prior(normal(0, 6), class = "b"))

student_t_df_prior = prior(gamma(2, 0.2), class = "nu")

                           