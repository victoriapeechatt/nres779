#### Lab 5 #### 

#### Simulating data ####

set.seed(1992)

n = 100
mu.init = 100 
var.init = 25
sig.init = sqrt(var.init)
y = rnorm(n = 100,mean = mu.init, sd = sig.init)
hist(y, breaks = 25)

#### Draw mean function, using conjugate normal ####

draw.mean = function(y,mu.prior, var.prior, var.star){
  a = (mu.prior/var.prior) + (sum(y)/var.star)
  b = (1/var.prior)  + (length(y)/var.star)
  conj_mean = a/b
  conj_var = 1/b
  conj_sd = sqrt(conj_var)
  rand_mu = rnorm(1, mean = conj_mean, sd = conj_sd)
  return(rand_mu)
}

#### Draw variance function, using conjugate gamma ####
draw.var = function(shape,rate,y,mu){
  alpha = shape + length(y)/2
  beta = rate + (sum((y-mu)^2)/2)
  rand_var = 1/(rgamma(1, shape = alpha, rate = beta))
  return(rand_var)
}

#### MCMC settings ####

n.iterations = 10000
chains = 3
mu.prior = 0
var.prior = 10000
a.prior = 0.01
b.prior = 0.01

#### Initializing storage matrices ####

mean.samples = matrix(c(0,0,0), nrow = n.iterations, ncol = chains)
var.samples = matrix(c(10000, 10000, 10000), nrow = n.iterations, ncol = chains)

#### Running MCMC , Gibbs Sampler #### 

for (c in 1:chains){
  for (k in 2:n.iterations){
    mean.samples[k,c] = draw.mean(y = y, mu.prior = mu.prior,var.prior = var.prior,var.star = var.samples[k-1,c])
    var.samples[k,c] = draw.var(y = y, mu = mean.samples[k,c], shape = a.prior, rate = b.prior)
  }
}

#### Plotting trace plots and marginal posterior distributions ####

### comparing with/without burn in ###

hist(mean.samples)
hist(var.samples)
burnin = 1000
mean.samples.sub = mean.samples[(burnin+1):n.iterations, ]
var.samples.sub = var.samples[(burnin+1):n.iterations, ]

### mean and variance of parameter estimations ###

### mean and variance of mu estimation

hist(mean.samples.sub)
abline(v = mean(mean.samples.sub))
abline(v = mu.init, col = "red")
mean(mean.samples.sub)
sd(mean.samples.sub)
sum(var(mean.samples.sub))

## comparing to standard error of original data 

sig.init/sqrt(length(y))

### mean and variance of variance estimation

hist(var.samples.sub)
abline(v = mean(var.samples.sub))
abline(v = var.init, col = "red")
mean(var.samples.sub)
sum(var(var.samples.sub))
sd(var.samples.sub)

### Trace plots for variance estimation ###

par(mfrow = c(1,1))
plot(1:length(var.samples.sub[,1]), var.samples.sub[,1], type = "l", col= "darkcyan", ylab = "Variance estimate", xlab = "Iteration number")
points(1:length(var.samples.sub[,2]), var.samples.sub[,2], type = "l", col= "maroon")
points(1:length(var.samples.sub[,3]), var.samples.sub[,3], type = "l", col= "darkolivegreen")

### Trace plots for mean estimation ###

par(mfrow = c(1,1))
plot(1:length(mean.samples.sub[,1]), mean.samples.sub[,1], type = "l", col= "darkcyan", ylab = "Mean estimate", xlab = "Iteration number")
points(1:length(mean.samples.sub[,2]), mean.samples.sub[,2], type = "l", col= "maroon")
points(1:length(mean.samples.sub[,3]), mean.samples.sub[,3], type = "l", col= "darkolivegreen")

