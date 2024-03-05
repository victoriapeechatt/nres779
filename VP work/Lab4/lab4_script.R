#### Lab 4 ####

#### The Problem ####
set.seed(2021)

y = rpois(n = 50, lambda = 6.4)
hist(y)

library("plyr")
plot(count(y), type  = "h")

mu.prior = 10.2
sigma.prior = 0.5
step = 0.01
theta = seq(0,15,step)
alpha = mu.prior^2/sigma.prior^2
beta = mu.prior/sigma.prior^2

#### The Prior distribution ####

## theta ~ gamma(shape, rate)
## theta ~ gamma(alpha, beta)
## theta ~ gamma()
## alpha = mean^2 / sigma^2 
## beta = mean / sigma^2 
alpha
beta
## theta ~ gamma(416.16, 40.8)
theta.prior=0
calc_theta.prior = function(theta,mu.prior,sigma.prior){
  alpha = mu.prior^2/sigma.prior^2
  beta = mu.prior/sigma.prior^2
  for (i in 1:length(theta)){
    theta.prior[i] = dgamma(theta[i],shape = alpha, rate = beta)
  }
  return(theta.prior)
}
prior_sim = calc_theta.prior(theta,mu.prior = 10.2,sigma.prior = 0.5)
prior_sim
plot(theta, prior_sim)

check_mom = rgamma(100000,shape=alpha, rate=beta)
mean(check_mom)
var(check_mom)

#### The Likelihood ####
## [y|theta] = 

calc_likelihood = function(theta,y){
  likelihood = 0
  for (i in 1:length(theta)){
    likelihood[i] = prod(dpois(y,theta[i],log = FALSE))
  }
  return(likelihood)
}

likeli_sim = calc_likelihood(theta = theta, y=y)
likeli_sim
plot(theta, likeli_sim,xlim = c(0,15))

#### The Joint Distribution ####

joint = function(prior_sim,likeli_sim){
  joint = 0
  for (i in 1:length(theta)){
    joint[i] = prior_sim[i]*likeli_sim[i]
  }
  return(joint)
}

sim_joint = joint(prior_sim,likeli_sim)

plot(theta, sim_joint)

calc_norm_const = function(sim_joint,step,theta){
  ncon = 0
  for (i in 1:length(theta)){
    ncon[i] = sim_joint[i]*step
  }
  norm_const = sum(ncon)
  return(norm_const)
}
sim_norm_const = calc_norm_const(sim_joint, step, theta)

#### The Posterior Distribution ####

calc_posterior = function(sim_joint, sim_norm_const){
  post = 0
  for (i in 1:length(theta)){
    post[i] = sim_joint[i]/sim_norm_const
  }
  return(post)
}

sim_posterior = calc_posterior(sim_joint,sim_norm_const)

plot(theta,sim_posterior)

#### Putting it all together ####

scaling_likeli = function(likeli_sim,sim_posterior){
  scaled_likeli = 0
  max_likeli_in = which.max(likeli_sim)
  max_likeli = likeli_sim[max_likeli_in]
  max_post_in = which.max(sim_posterior)
  max_post = sim_posterior[max_post_in]
  for (i in 1:length(theta)){
    scaled_likeli[i] = (likeli_sim[i]/max_likeli)*max_post
  }
  return(scaled_likeli)
}

#### Prior, Likelihood, Posterior Plots ####
sim_scaled_likeli = scaling_likeli(likeli_sim, sim_posterior)
plot(theta,sim_scaled_likeli, col = "green")
points(theta, sim_posterior, col = "red")
points(theta, prior_sim, col = "blue")

