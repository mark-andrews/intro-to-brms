library(brms)
library(ggplot2)

# Plot the histogram of eruptions of old faithful
ggplot(faithful,
       aes(x = eruptions)
) + geom_histogram(col='white', bins=30) + theme_classic()

M <- brm(eruptions ~ 1,
         data = faithful,
         family = mixture(gaussian, gaussian),
         chains = 4,
         cores = 2)

