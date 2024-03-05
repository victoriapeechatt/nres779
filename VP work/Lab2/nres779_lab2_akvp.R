### Lab 2

### Section 7 

## 7.3
set.seed(1234)
data1 = rpois(100000,33.32)
summary(data1)
mean(data1)
var(data1)
qpois(0.05,33.32, lower.tail = TRUE)
1 - qpois(0.95,33.32, lower.tail = TRUE)


## 7.4

probs = c(0.07,0.13,0.15,0.23,0.42)
data2 = rmultinom(1, size = 80, prob = probs)
liker_scale = c("strongly disagree", 
                "disagree",
                "neither a nor d",
                "agree",
                "strongly agree")
data2 = data.frame(liker_scale,data2)
data2

## 7.5 

dnorm(94, mean = 103.4, sd = 23.3) # y_i ~ N(103.4, 23.3)
#do separate 
pnorm(110, mean = 103.4, sd = 23.3)-pnorm(90, mean = 103.4, sd = 23.3)

### 7.6 

dbinom(4,24,0.12) # y_i ~ Binomial(0.12, n=24) 

### 7.7 

probs = c(0.56,0.06,0.16,0.22)
data3 = c(65,4,25,26)
dmultinom(data3,120, prob = probs)

### 7.8 

dnorm(1.5, mean = 1.9, sd = 1.4)
qnorm(0.975, mean = 1.9, sd = 1.4)
qnorm(0.025, mean = 1.9, sd = 1.4)

### 8.0 

library(MASS)
DrawRates=function(n,int,int.sd,inf,inf.sd,rho.rates){
  
  covar=rho.rates*int.sd*inf.sd
  
    Sigma=matrix(c(int.sd^2,
                   covar,
                   covar,
                   inf.sd^2),2,2)
    
      
      mu=c(int,inf)
      
        x=(mvrnorm(n=n,mu=mu,Sigma))
        
          
          #x[x[,2]<0]=0 #do not allow for deflation
          
          
          return(x)
        }
mu.int=0.0531
sd.int=0.0746
mu.inf=0.03
sd.inf=0.015
rho=0.5
n=10000
x=DrawRates(n,int=mu.int,int.sd=sd.int,inf=mu.inf,inf.sd=sd.inf,rho.rates=rho)
plot(x[,1],x[,2],pch=19,cex=.05,xlab="Rate of return",ylab="Rate of inflation")


### Section 9
set.seed(1234)
Normal_Distribution = rnorm(10000,mean = 0, sd = 1)
Lognormal_Distribution = rlnorm(10000, meanlog = 0, sdlog = 1)

par(mfrow = c(1,2))
hist(Normal_Distribution, ylim = c(0,8000), xlab = "mean", ylab = "Distribution of mean")
hist(Lognormal_Distribution, ylim = c(0,8000), xlim = c(0,15), xlab = "mean", ylab = "Distribution of mean")

mean(Normal_Distribution)
var(Normal_Distribution)
mean(Lognormal_Distribution)
var(Lognormal_Distribution)

exp(0 + 1^2/2)
(exp(1^2)-1)*(exp(2*0+1^2))
### 9.3.a
par(mfrow = c(1,1))
set.seed(1950)
phi_values = seq(0,1,0.001)
data5 = dnorm(phi_values, mean = 0.04, sd = 0.01)
plot(phi_values, data5, type="l",xlim = c(0,0.1),ylab = bquote("Distribution of" ~ phi), xlab = expression(phi))

### 9.3.b
data5 = dbinom(5,size = 50,prob = 0.04)
data5

### 9.3.c

ys = seq(1,10)
data = dbinom(ys,  size = 50, prob = 0.04)
plot(ys, data, type = "l",
     ylab = "Probability of occupation out of 50 patches",
     xlab = "Number of Patches",
     main = "Probability distribution of 1-10 occupied patches of 50")

### 9.3.d
## at least 5, means 4 or less
data = pbinom(4,50,0.04, lower.tail = FALSE)
data

### 9.3.e 

ys = seq(1:50)
data = pbinom(ys,50,0.04, lower.tail = TRUE)
plot(ys, data, type = "l",
     xlim = c(0,10),
     xlab = "Minimum number of occupied patches",
     ylab  ="Probability")

### 9.3.f 

data_sim = rbinom(n=75, size = 1,prob = 0.04)
data_sim


### 9.4

# y_i \sim Normal(\mu_i, \sigma^2)
# mu_i = \beta_0 + \beta_1x_i

beta0 = 0.01
beta1 = 0.09
x = seq(0.01,0.2,0.01)
y = 0

for (i in 1:length(x)) {
  y[i] = beta0 + beta1* x[i] + rnorm(1,0.001,0.0009)
}
data = data.frame(x  = x, y=y)
plot(data, pch=16, xlab = "Soil water", 
     ylab = "Plant Growth", 
     main = "Simulation of Plant Growth Data")
abline(a=beta0, b=beta1, col = "blue")
     
### 9.5
set.seed(1234)
par(mfrow = c(1,2))
n = 100000
mu = 100
var = 400 
disp = mu^2/(var-mu)
phi = disp/(mu+disp)

Dispersion_and_Mean_Data = rnbinom(size=disp, n=n, mu = mu)
hist(Dispersion_and_Mean_Data, xlab = expression(mu))

Probability_and_Dispersion_Data = rnbinom(size=disp, prob = phi, n=n)
hist(Probability_and_Dispersion_Data, xlab = expression(mu))

