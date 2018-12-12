library(lme4)
library(readr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(brms)

# Where would we be without a probabilistic model of flipping a coin?

# m Heads in n flips
Df <- data.frame(m=139, n=250)

# Binomial likelihood and beta prior
M_1 <- brm(m | trials(n) ~ 1,
           family = binomial(link="identity"),
           prior = set_prior("beta(1, 1)", class = "Intercept"),
           data = Df,
           iter = 1e4,
           cores = 4)

# Use binomial logistic regression
# prior on the log odds scale
M_2 <- brm(m | trials(n) ~ 1, 
           prior = set_prior('normal(0, 100)', class = 'Intercept'),
           family = binomial(),
           iter = 1e4,
           cores = 4,
           data=Df)
