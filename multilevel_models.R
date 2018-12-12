library(lme4)
library(readr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(brms)

Df <- sleepstudy # rename the data frame

# Visualize it
ggplot(Df,
       aes(x=Days, y=Reaction, col=Subject)
) + geom_point() +
  stat_smooth(method='lm', se=F, size=0.5) +
  facet_wrap(~Subject) +
  theme_classic()


# Random intercepts model with lmer
M_0_lmer <- lmer(Reaction ~ Days + (1|Subject),
                 data = Df)

# Random intercepts model with brms
M_1_lmer <- brm(Reaction ~ Days + (1|Subject),
                cores = 2,               
                prior = set_prior('normal(0, 100)'), # flat(ish) prior on coefs
                save_all_pars = T,
                data = Df)

# Random intercepts and random slopes model with lmer
M_1_lmer <- lmer(Reaction ~ Days + (Days|Subject),
                 data = Df)

# Random intercepts and random slopes model with brms
M_1 <- brm(Reaction ~ Days + (Days|Subject),
           cores = 2,               
           prior = set_prior('normal(0, 100)'),  
           save_all_pars = T,
           data = Df)

# Nested multilevel linear models
Df2 <- read_csv('data/science.csv')

M_2 <- brm(like ~ sex + PrivPub + (1|school) + (1|Class), 
           cores = 2,               
           prior = set_prior('normal(0, 100)'),  
           save_all_pars = T,
           data = Df2)


# Ordinal logistic (because "like" is ordinal really)
M_3 <- brm(like ~ sex + PrivPub + (1|school) + (1|Class),
           cores = 2,
           prior = set_prior('normal(0, 100)'), 
           save_all_pars = T,
           family=cumulative("logit"),
           data = Df2)

# consider using control = list(adapt_delta = 0.95)

# Multilevel logistic regression
Df %<>% mutate(fast_rt = Reaction < median(Reaction))

M_4 <- brm(fast_rt ~ Days + (Days|Subject),
           family = bernoulli(),
           cores = 2,               
           prior = set_prior('normal(0, 100)'),  
           save_all_pars = T,
           data = Df)
