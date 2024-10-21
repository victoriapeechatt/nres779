library("ESS575")
library("rjags")
library("tidyverse")

sink("lab8_3.R") # This is the file name for the jags code
cat("
  model{
        # priors
            beta0 ~ dnorm(0,10000)
            beta1 ~ dnorm(0,10000)
        # likelihood
            for(i in 1:n){
            p[i] <- exp(beta0 + beta1*x[i])/(1+ exp(beta0 + beta1*x[i]))
            y[i] ~ dbinom(p[i],1)
            }
            
        }
",fill = TRUE)
sink()

Lizards = IslandLizards
Lizards$perimeterAreaRatio = scale(Lizards[,1])

# You need a list for every chain 
inits = list(
  list(beta0 = 1, beta1 = 2),
  list(beta0 = 0.01, beta1 = 0.4),
  list(beta0 = 0.00002, beta1 = 0.000005))

# Left side of =  jags
# Right side of = is name in R
# execution of JAGS is about 5 times faster 
# on double precision than on integers.
# The n is required in the JAGS program to index the for structure

data = list(
  n = nrow(Lizards),
  x = as.double(Lizards$perimeterAreaRatio),
  y = as.double(Lizards$presence))

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
jm = jags.model("lab8_3.R", data = data, inits = inits,
                n.chains = length(inits), n.adapt = n.adapt)

update(jm, n.iter = n.update)

# zm: creates the chains and stores them as an MCMC list
# variable names: JAGS which variables to “monitor”. 
# These are the variables for which you want posterior distributions.

zm = coda.samples(jm, 
                  variable.names = c("beta0", "beta1"),
                  n.iter = n.iter, n.thin = 1)
summary(zm)
plot(zm)
