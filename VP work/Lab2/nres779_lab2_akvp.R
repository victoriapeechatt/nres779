### Lab 2

### Section 7 

## 7.3
data1 = rpois(100000,33.32)
summary(data1)
mean(data1)
var(data1)

## 7.4

probs = c(0.07,0.13,0.15,0.23,0.42)
data2 = rmultinom(1, size = 80, prob = probs)
data2
liker_scale = c("strongly disagree", 
                "disagree",
                "neither a nor d",
                "agree",
                "strongly agree")
plot(1:5,data2)

## 7.5 

dnorm(94, mean = 103.4, sd = 23.3) # y_i ~ N(103.4, 23.3)
#do separate 
pnorm(90:110, mean = 103.4, sd = 23.3)

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

### Section 9
set.seed(1234)
data4 = rnorm(10000,mean = 0, sd = 1)
data5 = rlnorm(10000, meanlog = 0, sdlog = 1)

par(mfrow = c(1,2))
hist(data4, ylim = c(0,10000))
hist(data5, ylim = c(0,10000))

mean(data4)
var(data4)
mean(data5)
var(data5)


### 9.3.a
set.seed(1950)
phi_values = seq(0,1,0.001)
data5 = dnorm(phi_values, mean = 0.04, sd = 0.01)
plot(phi_values, data5, type="l",xlim = c(0,0.1))

### 9.3.b
data5 = dbinom(5,size = 50,prob = 0.04)
data5

### 9.3.c

ys = seq(1:10)
data = dbinom(ys,  size = 50, prob = 0.04)
plot(ys, data, type = "l")

### 9.3.d

data = pbinom(5,50,0.04, lower.tail = FALSE)
data

### 9.3.e 

ys = seq(1:50)
data = pbinom(ys,50,0.04, lower.tail = TRUE)
plot(ys, data, type = "l")

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
plot(data)
lm(y~x)
abline(a=beta0, b=beta1, col = "blue")
     
### 9.5
set.seed(1234)
par(mfrow = c(1,2))
n = 100000
mu = 100
var = 400
disp = mu^2/(var-mu)
phi = disp/(mu+disp)

data = rnbinom(size=disp, n=n, mu = mu)
hist(data)

data2 = rnbinom(size=disp, prob = phi, n=n)
hist(data2)

