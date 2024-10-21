##### Lab 6 ##### 
##### Victoria Peechatt 
##### 3/27/2024

##### Part V #####

##### Number 1 #####
prob_dens_1 = dnorm(x = 10,mean = 12, sd = 1.5)
prob_dens_1
prob_dens_2 = dnorm(x = 12, mean = 10, sd = 1.5)
prob_dens_2

moment_match_gamma = function(mu, sigma){
  shape = mu^2/sigma^2 
  rate = mu/sigma^2 
  moments = matrix(nrow = 1,ncol = 2)
  moments = c(shape,rate)
}

param_3 = moment_match_gamma(mu = 12,sigma = 1.5)
prob_dens_3 = dgamma(x = 10, shape = param_3[1], rate = param_3[2])
prob_dens_3 

param_4 = moment_match_gamma(mu = 10, sigma = 1.5)
prob_dens_4 = dgamma(x = 12, shape = param_4[1], rate = param_4[2])
prob_dens_4

##### Part 7: The Deterministic Model ##### 

g = function(alpha, gamma, c, L){
  mu = alpha * (L - c) / (alpha/gamma + (L - c))
  return(mu)
}

##### Part 8: Data Simulation #####

getData = function(alpha, gamma, c, sigma){
  set.seed(4)
  par(mfrow=c(1, 1))
  L = sort(runif(50, min = 12, max = 100))
  mu = g(alpha = alpha, gamma = gamma, c = c, L = L)
  y = rgamma(length(mu), mu^2/sigma^2, mu/sigma^2)
  plot(L, y, pch = 16, cex = 1.4, col = "blue")
  lines(L, mu, lwd = 2)
  model = nls(y ~ g( alpha = alpha, gamma = gamma, c = c, L = L),
              start = list(alpha = 50, gamma = 4, c = 2))
  s = summary(model)
  p = coef(model)
  y.hat = g(alpha = p[1], gamma = p[2], c = p[3], L = L)
  lines(L, y.hat, col = "red", lwd = 2)
  legend(40, 18, c("Generating values", "NLS fit"),
         lty = c("solid", "solid"), col = c("black", "red"),
         bty = "n", lwd = 2)
  return(list(
    x = L,
    y = y,
    nls.alpha = p[1],
    nls.gamma = p[2],
    nls.c = p[3],
    nls.sigma = s$sigma,
    gen.alpha = alpha,
    gen.gamma = gamma,
    gen.c = c,
    gen.sigma = sigma))
}

##### Part 9: The Setup #####

setup = function(n.iter, n.chain, parameter.names, dim.x){
  # Set up storage for chains
  x = list()
  for(i in 1:length(parameter.names)){
    x[[i]] = array(NA, dim = c(dim.x[i], n.iter, n.chain))
  }
  # Assign parameter names to elements of the list
  names(x) = parameter.names
  # Enter initial values for parameters here
  x$alpha[1, 1, 1] = 40
  x$c[1, 1, 1] = 10
  x$gamma[1, 1, 1] = 3
  x$sigma[1, 1, 1] = 5
  # Enter tuning parameters here
  tune=list(
    alpha=1,
    c = 0.25,
    gamma=0.5,
    sigma=2
  )
  x$tune = tune
  return(x)
}

# 10th value of sigma: 
# x$sigma[1,10]

##### Part 10: The Proposal Function #####

q = function(theta, mu, tune, type){
  sigma = tune
  if (type == "density") 
    return (dgamma(theta, shape = mu^2/sigma^2, rate = mu/sigma^2))
  if (type == "draw") 
    return (rgamma(1, shape = mu^2/sigma^2, rate = mu/sigma^2))
}

##### Part 11: The Prior Function #####

prior = function(param, theta){
  if(param == "alpha") return( dunif(theta, min = 0, max = 500, log = TRUE))
  if(param == "c") return(dunif(theta, min = 0, max = 200, log = TRUE))
  if(param == "gamma") return(dunif(theta, min = 0, max = 200, log = TRUE))
  if(param == "sigma" ) return(dunif(theta, min = 0, max = 200, log = TRUE))
}

##### Part 12: The Likelihood Function #####

Like = function(y, L, alpha, gamma,c ,sigma){
  mu=g(alpha = alpha, gamma = gamma, c = c, L = L)
  LogL = dgamma(x = y, shape = mu^2/sigma^2, rate = mu/sigma^2, log = TRUE)
  return(sum(LogL[is.finite(LogL)]))
}

##### Part 13: The Choose Function ##### 

choose = function(x, z, Like_z, Like_x, param, tune){
  # These are both logs so we add rather than multiply
  numerator = Like_z + prior(param, theta = z)
  denominator = Like_x + prior(param, theta = x)
  q.ratio = q(theta = x, mu = z, tune = tune, type = "density") /
    q(theta = z, mu = x, tune = tune, type = "density")
  # Because these are logs, we exponentiate the difference to get the ratio.
  R = exp(numerator - denominator) * q.ratio
  if (R > runif(1, min = 0, max = 1)) new = z else new = x
  return(new)
}

##### Part 14: A Few Plotting Functions ##### 

tracePlots = function(data, x, n.iter, burnin){
  par(mfrow = c(2, 2))
  plot(x$gamma[1,burnin:n.iter,1], type = "l", xlab = "Iteration",
       col = "chartreuse4", ylab = expression(gamma))
  abline(h = mean(x$gamma[1,burnin:n.iter,1]), col = "red")
  plot(x$alpha[1,burnin:n.iter,1], type = "l", xlab = "Iteration",
       col = "chartreuse4", ylab = expression(alpha))
  abline(h = mean(x$alpha[1,burnin:n.iter,1]), col = "red")
  plot(x$c[1,burnin:n.iter,1], type = "l", xlab = "Iteration",
       col = "chartreuse4", ylab = expression(c))
  abline(h = mean(x$c[1,burnin:n.iter,1]), col = "red")
  plot(x$sigma[1,burnin:n.iter,1], type = "l", xlab = "Iteration",
       col = "chartreuse4", ylab = expression(sigma))
  abline(h = mean(x$sigma[1,burnin:n.iter,1]), col="red")
}

predictPlots = function(data, x, n.iter, burnin){
  par(mfrow = c(1, 1))
  q.y.hat = apply(x$y.hat[, burnin:n.iter, 1], 1,
                  function(x) quantile(x, c(.025, .5, .975),na.rm = TRUE))
  plot(data$x, data$y, xlab = "Light level",
       ylab = "Growth rate", main = "Prediction of growth rate",
       pch = 16, cex = 1.4, col = "blue")
  lines(data$x, g(alpha = data$nls.alpha,
                  gamma = data$nls.gamma, c = data$nls.c, L = data$x),
        col="blue", lwd = 4)
  lines(data$x,q.y.hat[2,], col = "orange", lwd = 2)
  lines(data$x,q.y.hat[1,], lty = "dashed", col = "orange", lwd = 2)
  lines(data$x,q.y.hat[3,], lty = "dashed", col = "orange", lwd = 2)
  legend(40,18, c("Median", "2.5% quantile",
                  "97.5% quantile", "NLS fit"),
         lty = c("solid", "dashed", "dashed"),
         col = c("orange", "orange", "orange", "blue"),
         lwd = c(2, 2, 2, 4),
         bty = "n")
}

plot_density = function(p, v1, v2, param, burnin, n.iter){
  hist(p[burnin:n.iter], breaks = 40, xlab = "Value of parameter",
       freq = FALSE, main = param, col = "gray")
  abline(v = v1, col = "red", lwd = 3)
  abline(v = v2, col = "blue", lwd = 2)
  abline(v = median(p[burnin:n.iter]), col = "orange", lwd = 2)
}

densityPlots = function(data, x, n.iter, burnin){
  par(mfrow = c(2, 2))
  plot_density(p = x$alpha, v1 = data$gen.alpha,
               v2 = data$nls.alpha, param = expression(alpha),
               burnin = burnin, n.iter = n.iter)
  plot_density(p = x$gamma, v1 = data$gen.gamma,
               v2 = data$nls.gamma, param = expression(gamma),
               burnin = burnin, n.iter = n.iter)
  plot_density(p = x$c, v1 = data$gen.c, v2 = data$nls.c,
               param = expression(c), burnin = burnin,
               n.iter = n.iter)
  plot_density(p = x$sigma, v1 = data$gen.sigma, v2 = data$nls.sigma,
               param = expression(sigma),
               burnin = burnin, n.iter = n.iter)
}

##### Part 15: The Problem: #####

data = getData(alpha = 38.5, gamma = 1.7, sigma = 2, c = 8)

n.iter = 50000
x = setup(n.iter = n.iter, n.chain = 1,
          parameter.names = c("alpha", "c", "gamma", "sigma",
                              "y.hat", "growth_ratio"), 
          dim.x = c(1, 1, 1, 1, 50, 1))

tune = x$tune

# Initialize MCMC chains
for (i in 2:n.iter) {
  
    # Propose new value for alpha
    new.alpha = q(mu = x$alpha[1, i-1, 1], tune = x$tune$alpha, type = "draw")
    
    # Calculate likelihoods
    Like_x = Like(y = data$y, L = data$x, 
                  alpha = x$alpha[1, i-1,1], gamma = x$gamma[1, i-1, 1], 
                  c = x$c[1,i-1, 1], sigma = x$sigma[1, i-1, 1])
    Like_z = Like(y = data$y, L = data$x, 
                  alpha = new.alpha, gamma = x$gamma[1, i-1, 1], 
                  c = x$c[1, i - 1, 1], sigma = x$sigma[1, i - 1, 1])

    # Choose new alpha
    x$alpha[1,i,1] = choose(x = x$alpha[1,i-1,1], 
                            z = new.alpha, 
                            Like_z = Like_z, 
                            Like_x = Like_x, 
                            param = "alpha", 
                            tune = tune$alpha)
    
    # Propose new value for gamma
    new.gamma = q(mu = x$gamma[1, i-1, 1], tune = x$tune$gamma, type = "draw")
    
    # Calculate likelihoods
    Like_x = Like(y = data$y, L = data$x, 
                  alpha = x$alpha[1, i-1,1], gamma = x$gamma[1, i-1, 1], 
                  c = x$c[1,i-1, 1], sigma = x$sigma[1, i-1, 1])
    Like_z = Like(y = data$y, L = data$x, 
                  alpha = x$alpha[1,i-1,1], gamma = new.gamma, 
                  c = x$c[1, i - 1, 1], sigma = x$sigma[1, i - 1, 1])
    
    #Choose new gamma
    x$gamma[1,i,1] = choose(x = x$gamma[1,i-1,1], 
                            z = new.gamma, 
                            Like_z = Like_z, 
                            Like_x = Like_x, 
                            param = "gamma", 
                            tune = tune$gamma)
    
    # Propose new value for c
    new.c = q(mu = x$c[1, i-1, 1], tune = x$tune$c, type = "draw")
    
    # Calculate likelihoods
    Like_x = Like(y = data$y, L = data$x, 
                  alpha = x$alpha[1, i-1,1], gamma = x$gamma[1, i-1, 1], 
                  c = x$c[1,i-1, 1], sigma = x$sigma[1, i-1, 1])
    Like_z = Like(y = data$y, L = data$x, 
                  alpha = x$alpha[1,i-1,1], gamma = x$gamma[1,i-1,1], 
                  c = new.c, sigma = x$sigma[1, i-1, 1])
    
    #Choose new c
    x$c[1,i,1] = choose(x = x$c[1,i-1,1],
                        z = new.c, 
                        Like_z = Like_z, 
                        Like_x = Like_x, 
                        param = "c", 
                        tune = tune$c)
    
    # Propose new value for sigma
    new.sigma = q(mu = x$sigma[1, i-1, 1], tune = x$tune$sigma, type = "draw")
    
    # Calculate likelihoods
    Like_x = Like(y = data$y, L = data$x, 
                  alpha = x$alpha[1, i-1,1], gamma = x$gamma[1, i-1, 1], 
                  c = x$c[1,i-1, 1], sigma = x$sigma[1, i-1, 1])
    Like_z = Like(y = data$y, L = data$x, 
                  alpha = x$alpha[1,i-1,1], gamma = x$gamma[1,i-1,1], 
                  c = x$c[1,i-1,1], sigma = new.sigma)
    
    #Choose new sigma
    x$sigma[1,i,1] = choose(x = x$sigma[1,i-1,1], 
                            z = new.sigma, 
                            Like_z = Like_z, 
                            Like_x = Like_x, 
                            param = "sigma", 
                            tune = tune$sigma)
    
    #Derived quantities 
    x$growth_ratio[1,i-1,1] = x$alpha[1,i-1,1]/x$gamma[1,i-1,1]
    
    x$y.hat[,i-1,1] = g(alpha = x$alpha[1,i-1,1], 
                                gamma = x$gamma[1,i-1,1], 
                                c = x$c[1,i-1,1],L = data$x)
}

# Plotting trace plots 
burnin = 15000
tracePlots(data = data, x = x, n.iter = n.iter, burnin = burnin)

# Plotting predicted quantities 
predictPlots(data = data, x = x, n.iter = n.iter, burnin = burnin)

# Plotting density plots 
densityPlots(data = data, x = x, n.iter = n.iter, burnin = burnin)




