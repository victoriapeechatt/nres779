
## NRES Week 1 ##

## Packages ----

library(tidyverse) # install if you haven't!

## Exploring chaos with the discrete logistic equation ----

# Point 1.

# define function for equation 1
dist_logistic <- function(x, lambda){lambda * x *(1 - x)}

# possible lambda values
lambdas <- seq(0.25, 4, by = 0.25)

# initial population size
x0 <- 0.01

# set empty vector of population sizes to fill
xs <- c(x0, rep(NA,29))

# holder dataset
sim_data <- data.frame(t = c(), x = c(), lambda = c())

# loop to run simulation
for(i in 1:length(lambdas)){

  # generate a new sequence of population sizes from empty vector
  xs_sim <- xs

  # loop to simulate change in population size
  for(j in 2:length(xs_sim)){

    # simulate new population size
    xs_sim[j] <- dist_logistic(x = xs_sim[j-1], lambda = lambdas[i])

  }

  # prepare return dataset
  it_data <- data.frame(t = seq(1,30, by = 1),
                        x = xs_sim,
                        lambda = rep(lambdas[i], length(xs_sim)))

  # bind return dataset
  sim_data <- rbind(sim_data, it_data)

}

# plotting
sim_data %>%  ggplot(aes(x = t, y = x)) +
  geom_line() +
  facet_wrap(~lambda, scales = "free_y")

# I didn't do the lambda = whatever on the panels because I wanted to go to bed

# Point 2.

# new possible lambda values
lambdas_2 <- seq(1, 4, by = 0.05)

# new vector to store population sizes
xs_2 <- c(x0, rep(NA,99))

# holder dataset
sim_data_2 <- data.frame(t = c(), x = c(), lambda = c())

# loop to run simulation
for(i in 1:length(lambdas_2)){

  # generate a new sequence of population sizes from empty vector
  xs_sim <- xs_2

  # loop to simulate change in population size
  for(j in 2:length(xs_sim)){

    # simulate new population size
    xs_sim[j] <- dist_logistic(x = xs_sim[j-1], lambda = lambdas_2[i])

  }

  # prepare return dataset
  it_data <- data.frame(t = seq(1,100, by = 1),
                        x = xs_sim,
                        lambda = rep(lambdas_2[i], length(xs_sim)))

  # bind return dataset
  sim_data_2 <- rbind(sim_data_2, it_data)

}

# filter data for x > 50
sim_data_2 <- sim_data_2 %>% filter(t > 50)

# plotting
sim_data_2 %>%
  ggplot(aes(x = lambda, y = x)) +
  geom_point() +
  xlab(expression(lambda)) + ylab(expression(x[t]))





