library(readr)
library(dplyr)
library(ggplot2)
library(brms)

## ----------------

set.seed(1001) # Omit or change this if you like

N <- 250

x_1 <- rnorm(N)
x_2 <- rnorm(N)

beta_0 <- 1.25
beta_1 <- 1.75
beta_2 <- 2.25

mu <- beta_0 + beta_1 * x_1 + beta_2 * x_2

y <- mu + rnorm(N, mean=0, sd=1.75)

Df <- tibble(x_1, x_2, y)

# ------ Standard, non-Bayesian, linear regression -------
M_lm <- lm(y ~ x_1 + x_2, data=Df)

# ------ Bayesian linear regression ----------

# Set-up and sample from Bayesian linear model
# using defaults for more or less everything
M_bayes <- brm(y ~ x_1 + x_2, data = Df)

# Overriding defaults
M_bayes <- brm(y ~ x_1 + x_2, 
               data = Df,
               cores = 2, # I have a dual-core
               chains = 4, # 4 chains is typical
               iter = 2500,
               warmup = 1000, # these are initilization etc iterations
               prior = set_prior('normal(0, 100)'), # flat(ish) prior on coefs
               seed = 101011 # for reproducibility
)


# Get the model summary
summary(M_bayes)

# Plot the posteriors etc
plot(M_bayes)

# Just the coefficients
plot(M_bayes, pars = '^b')

# Or just one
plot(M_bayes, pars = '^b_x_1')

# Posterior intervals
stanplot(M_bayes)

stanplot(M_bayes, type='hist', pars='^b_x_1')

## posterior predictive checks
pp_check(M_bayes)

# See plots of e.g. y ~ x_1, y ~ x_2, holding other values constant at means etc
marginal_effects(M_bayes)

# View the posterior samples 
posterior_samples(M_bayes)


# Get predictions of the model, using original predictor values
predict(M_bayes)

# predictions with new data, with new predictors
predict(M_bayes, newdata = data.frame(x_1 = c(0, 1, 2), 
                                      x_2 = c(-1, 1, 2))
)
data.frame(x_1 = c(0, 1, 2), 
           x_2 = c(-1, 1, 2))
# We can view the stan code of this model like so:
stancode(M_bayes)

# We can view the priors of this model like this:
prior_summary(M_bayes)


# Change priors 
newpriors <- c(prior_string("student_t(3, 0, 10)", class = "b"),
               prior_string("student_t(3, 18, 10)", class = "Intercept"),
               prior_string("student_t(3, 0, 10)", class = "sigma"))

M_bayes <- brm(y ~ x_1 + x_2, 
               data = Df,
               cores = 2, # I have a dual-core
               chains = 4, # 4 chains is typical
               iter = 2500,
               warmup = 1000, # these are initilization etc iterations
               prior = newpriors,
               seed = 101011 # for reproducibility
)


### ------- Doing model comparison etc 

Df <- read_csv('data/insulation.csv')

theme_set(theme_classic())

ggplot(Df,
       mapping = aes(x = Temp, y = Gas, col = Insul)
) + geom_point() +
  stat_smooth(method = 'lm', se = F)


# Classical lm
M_lm <- lm(Gas ~ Temp*Insul, data=Df)

M_bayes <- brm(Gas ~ Temp*Insul, 
               data = Df,
               cores = 2, 
               prior = set_prior('normal(0, 100)'), 
               save_all_pars = T 
)

# We'll do a model comparison comparing the above 
# model to an additive, i.e. non-interaction, model

M_lm_additive <- lm(Gas ~ Temp+Insul, data = Df)
M_bayes_additive <- brm(Gas ~ Temp+Insul, 
                        data = Df,
                        cores = 2, 
                        prior = set_prior('normal(0, 100)'), 
                        save_all_pars = T 
)

waic(M_bayes_additive, M_bayes)
loo(M_bayes_additive, M_bayes)
bayes_factor(M_bayes_additive, M_bayes)
