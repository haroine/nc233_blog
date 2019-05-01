#' Code for blog post 
#' "Shattering childhood illusions with R and Bayes theorem"
#' (A small stats problem based on the game of Clue. 
#' All we know from one player's game history is that Mrs. White was 
#' the murderer in only 2 occasions. How realistic is it that this
#' player played 1000 games?)

library(tidyverse)
source("clue/clue_functions.R")

## Parameters
k_mrs_white <- 2 # Number of times Mrs. White was the murderer
prob <- 1/6 # Probability of Mrs. White being the murderer for one game
x <- 1:200 # Reduction of the problem to a finite number of games

## Likelihood
dlikelihood <- dbinom(k_mrs_white, x, prob)

## Priors

# Uniformative prior
# dprior1 <- dunifdisc(x,10,2000)
dprior1 <- dunifdisc(x,3,100)
plot_clue_prior(x, dprior1)


## First interval

# dposterior2 <- dlikelihood * dprior2
dposterior1 <- dlikelihood * dprior1
dposterior1 <- dposterior1 / sum(dposterior1)
plot_clue_posterior(x, dposterior1)

which.max(dposterior1)

threshold_val <- 0.975
which(cumsum(dposterior1) > (threshold_val))[1]

cumsum(dposterior1)[max_prior]

dposterior2 <- dposterior2 / sum(dposterior2)
plot_clue_posterior(x, dposterior2)

which.max(dposterior2)

threshold_val <- 0.95
which(cumsum(dposterior2) > (threshold_val))[1]
cumsum(dposterior2)[max_prior]

## Other Data generation mechanism

# Simulation
set.seed(9)
N_sim_games <- 40
sim_murderer <- runifdisc(N_sim_games, 6)

plot_murderer <- ggplot(tibble(x=1:N_sim_games, y=sim_murderer), aes(y, stat(count))) +
  geom_histogram(aes(y =..count..),
                 bins=6, fill="white",colour="black") +
  ylab("Frequency - murderer") +
  xlab("Character #") +
  scale_x_continuous(breaks = 1:6)
print(plot_murderer)



gumbelclue_2 <- readRDS("clue/dcluegumbel_2.rds")
gumbelclue_2 <- gumbelclue_2[x]

dposterior_gen <- gumbelclue_2 * dprior1
dposterior_gen <- dposterior_gen / sum(dposterior_gen)

plot_clue_posterior(x, dposterior_gen)



dposterior_gen <- gumbelclue_2 * dprior2
# dposterior_gen <- gumbelclue_2 * dprior1
dposterior_gen <- dposterior_gen / sum(dposterior_gen)

plot_clue_posterior(x, dposterior_gen)

which.max(dposterior_gen)

threshold_val <- 0.975
which(cumsum(dposterior_gen) > (threshold_val))[1]

cumsum(dposterior_gen)[max_prior]

######## Other prior

# Informative prior
library(e1071)
# prior_probs <- (1:1000)**4
# prior_probs <- c(prior_probs, rev(prior_probs))

max_prior <- 30
prior_probs <- (1:max_prior)**4
prior_probs <- c(prior_probs, rev(prior_probs), rep(0, 200-2*max_prior))

dprior2 <- ddiscrete(x, prior_probs)
plot_clue_prior(x, dprior2)

dposterior2 <- gumbelclue_2 * dprior2
dposterior2 <- dposterior2 / sum(dposterior2)

plot_clue_posterior(x, dposterior2)

which.max(dposterior2)

threshold_val <- 0.975
which(cumsum(dposterior2) > (threshold_val))[1]