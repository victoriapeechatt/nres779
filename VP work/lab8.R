##### Lab 8: JAGS 
##### Victoria Peechatt 
library("ESS575")
library("rjags")
library("tidyverse")

sink("lab8.R") # This is the file name for the jags code
cat("
  model{
        # priors
            K ~ dunif(0, 4000)
            r ~ dunif (0, 2)
            sigma ~ dunif(0, 2)
            tau <- 1 / sigma^2
            max_pop_rate <- K/2
            pop_rate <- r * (1- (N/K))
        # likelihood
            for(i in 1:n){
            mu[i] <- r - r / K * x[i]
            y[i] ~ dnorm(mu[i], tau)
            }
            
        }
",fill = TRUE)
sink()

## Logistic example for Primer
Logistic = Logistic[order(Logistic$PopulationSize),]

# You need a list for every chain 
inits = list(
  list(K = 1500, r = .2, sigma = 1),
  list(K = 1000, r = .15, sigma = .1),
  list(K = 900, r = .3, sigma = .01))

# Left side of =  jags
# Right side of = is name in R
# execution of JAGS is about 5 times faster 
# on double precision than on integers.
# The n is required in the JAGS program to index the for structure

data = list(
  n = nrow(Logistic),
  x = as.double(Logistic$PopulationSize),
  y = as.double(Logistic$GrowthRate), 
  N = seq(from=0, to=1100, by=10))

# iterations in the chain for adaptation: 
# it is the number of iterations that JAGS will use to choose the sampler 
# and to assure optimum mixing of the MCMC chain.
# iterations in the chain for burnin
# iterations to keep in the final chain 
n.adapt = 5000 
n.update = 10000
n.iter = 10000

# Call to JAGS
# MCMC required generating random numbers, 
# so we use set.seed(1) to assure that our results are exactly reproducible

set.seed(1)

# JAGS model with specifications
# statement sets up the MCMC chain
jm = jags.model("lab8.R", data = data, inits = inits,
                n.chains = length(inits), n.adapt = n.adapt)

update(jm, n.iter = n.update)

# zm: creates the chains and stores them as an MCMC list
# variable names: JAGS which variables to “monitor”. 
# These are the variables for which you want posterior distributions.

zm = coda.samples(jm, 
                  variable.names = c("K", "r", "sigma", "tau", 
                                     "max_pop_rate", "pop_rate"),
                  n.iter = n.iter, n.thin = 1)
summary(zm)
posterior = data.frame(
  Mean = summary(zm)$stat[1:5,1],
  SD = summary(zm)$stat[1:5,2],
  Q1 = summary(zm)$quantile[1:5,1],
  Med = summary(zm)$quantile[1:5,3],
  Q4 = summary(zm)$quantile[1:5,5]
)
posterior
##### 2. #####
##### Plot the median population growth rate and 95% credible intervals as a function of N (maybe 0, 10, 20, . . . , 1100?).
##### What does this curve tell you about the difficulty of sustaining harvest in populations?
  
par(mfrow=c(1,1))
N = seq(from=0, to=1100, by = 10)
plot(x = N, y=summary(zm)$quantile[3:113,3], type = "l", ylim = c(0,0.3))
points(x = N, y=summary(zm)$quantile[3:113,5], type = "l", col = "red")
points(x = N, y=summary(zm)$quantile[3:113,1], type = "l", col = "red")



zj = jags.samples(jm, variable.names=c("K", "r", "sigma", 
                                       "max_pop_rate", "pop_rate"),
                  n.iter = n.iter, thin = 1)

hat = summary(zj$pop_rate, quantile, c(.025, .5, .975))$stat

##### 3. #####
##### What is the probability that the intrinsic rate of increase (r) 
##### exceeds 0.22? What is the probability that r falls between 0.18 and 0.22?
pr.gt.0.22 = 1 - ecdf(zj$r)(0.22)
pr.gt.0.22

pr.lt.22 = ecdf(zj$r)(0.22)
pr.lt.18 = ecdf(zj$r)(0.18)
pr.bt.18.22 = pr.lt.22 - pr.lt.18
pr.bt.18.22

plot(zm)

df = as.data.frame(rbind(zm[[1]],zm[[2]],zm[[3]]))
df
max_sigma = df$sigma[which.max(df$sigma)]
max_sigma


##### Part III 

data = IslandLizards
