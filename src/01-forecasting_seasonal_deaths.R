# Predict seasonal deaths and derive excess deaths with uncertainties
#
# Jonas Schöley

# Init ------------------------------------------------------------

library(tidyverse)
library(mgcv)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')

# constants specific to this analysis
cnst <- list(
  start_of_pandemic_date = '2020-02-17',
  n_sample = 100
)

# list containers for analysis artifacts
dat <- list()

# global functions
source('00-global_functions.R')

# Load data -------------------------------------------------------

# data on seasonal death counts and many co-variables
# sourced from https://github.com/jschoeley/mocy
dat$input_data <-
  readRDS('../dat/weeklydeaths.rds') %>%
  mutate(
    iso_week_fct = as.factor(iso_week),
    region_iso_fct = as.factor(region_iso)
  ) %>%
  drop_na(deaths_observed)

# only germany total counts
dat$subset_total <-
  dat$input_data %>%
  filter(sex == 'Total', age_group == 'Total', cv_id == 0,
         region_iso == 'DE')

# data for fitting
dat$training <-
  dat$subset_total %>%
  filter(cv_sample == 'training')

# data for prediction
dat$test <-
  dat$subset_total %>%
  filter(cv_sample == 'test')

# complete series of weekly death counts
dat$training %>%
  ggplot() +
  aes(x = date, y = deaths_observed) +
  geom_point(size = 0.4)

# Fit model -------------------------------------------------------

# fit model to training data
dat$fit <- gam(
  formula =
    deaths_observed ~
    iso_year +
    iso_week_fct +
    offset(log(personweeks)),
  data = dat$training,
  family = nb(link = 'log')
)

# Make predictions ------------------------------------------------

# predict expected deaths from model over training & test data
dat$subset_total$deaths_expected <-
  predict(dat$fit, newdata = dat$subset_total, type = 'response')

# plot predictions over training data
dat$subset_total %>%
  ggplot() +
  aes(x = date) +
  geom_point(aes(y = deaths_observed), size = 0.4) +
  geom_line(aes(y = deaths_expected), color = 'red')

# Simulate epistemic uncertainty in parameter estimates -----------

# create a design matrix for prediction over training and test data
dat$X_prd <- predict(dat$fit, newdata = dat$subset_total, type = 'lpmatrix')
# extract estimated coefficients
dat$beta <- coef(dat$fit)
# draw coefficients from multivariate normal
dat$beta_sim <- MASS::mvrnorm(cnst$n_sample, dat$beta, vcov(dat$fit))
# offset
dat$offset <- exp(attr(dat$X_prd, 'model.offset'))

# derive expected death count for each coefficient sample
dat$mu_sim <-
  dat$beta_sim %>%
  t() %>%
  apply(2, function (betas) dat$X_prd%*%betas + log(dat$offset)
  ) %>%
  exp()
colnames(dat$mu_sim) <- paste0('sim_mu', 1:cnst$n_sample)

# bind predictions to data
dat$subset_total <- bind_cols(dat$subset_total, dat$mu_sim)

# plot observed vs. predicted, plotting each draw
dat$subset_total %>%
  pivot_longer(
    cols = starts_with('sim_'), names_to = 'sim', values_to = 'mu'
  ) %>%
  ggplot() +
  aes(x = date) +
  geom_point(aes(y = deaths_observed), size = 0.4) +
  geom_line(aes(y = mu, group = sim), alpha = 0.02,
            color = 'red')

# Simulate aleatoric uncertainty in outcome -----------------------

# given the sampled expected death counts (NB mu parameter),
# draw counts from NB distribution
dat$count_sim <- apply(
  dat$mu_sim, 1,
  function (lambda)
    rnbinom(n = 100, mu = lambda, size = dat$fit$family$getTheta(TRUE))
) %>% t()
colnames(dat$count_sim) <- paste0('sim_count', 1:cnst$n_sample)

dat$subset_total <- bind_cols(dat$subset_total, dat$count_sim)

dat$subset_total %>%
  # summarise draws into 95% prediction intervals
  rowwise() %>%
  mutate(
   q025 = quantile(c_across(starts_with('sim_count')), probs = 0.025),
   q500 = quantile(c_across(starts_with('sim_count')), probs = 0.500),
   q975 = quantile(c_across(starts_with('sim_count')), probs = 0.975)
  ) %>%
  ggplot() +
  aes(x = date) +
  geom_ribbon(aes(ymin = q025, ymax = q975), color = NA, fill = 'grey80') +
  geom_vline(xintercept = as.Date(cnst$start_of_pandemic_date)) +
  geom_point(aes(y = deaths_observed), size = 0.4) +
  geom_line(aes(y = q500), color = '#025b4c') +
  scale_y_continuous(
    breaks = seq(0, 7e4, 2e3), labels = scales::label_comma()
  ) +
  MyGGplotTheme() +
  labs(x = NULL, y = 'Weekly deaths',
       title = 'Observed vs. expected deaths Germany')

# Transform predictions while propagating uncertainty -------------

# cumulative excess deaths since Jan 2020
dat$subset_total %>%
  # derive cumulated excess deaths since Jan 2020 from samples...
  filter(cv_sample == 'test') %>%
  mutate(deaths_observed_cum = cumsum(deaths_observed)) %>%
  mutate(across(starts_with('sim_count'), ~ deaths_observed_cum-cumsum(.x))) %>%
  # ...and summarise via 95% prediction intervals
  rowwise() %>%
  mutate(
    q025 = quantile(c_across(starts_with('sim_count')), probs = 0.025),
    q500 = quantile(c_across(starts_with('sim_count')), probs = 0.500),
    q975 = quantile(c_across(starts_with('sim_count')), probs = 0.975)
  ) %>%
  ggplot() +
  aes(x = date) +
  geom_ribbon(aes(ymin = q025, ymax = q975), color = NA, fill = 'grey80') +
  geom_line(aes(y = q500), color = '#025b4c') +
  scale_y_continuous(breaks = seq(-2e4, 2e5, 2e4), labels = scales::label_comma()) +
  coord_cartesian(ylim = c(-1e4, 17e4)) +
  MyGGplotTheme() +
  labs(x = NULL, y = 'Excess deaths since Jan 1st 2020',
       title = 'Cumulative excess deaths Germany')