## Lab 3 - Likelihood 


## Perry's script 
d=read.csv("Documents/GitHub/NRES779/VP work/HemlockData.csv")
x=d$Light
y=d$ObservedGrowthRate
plot(x,y,
     ylab="Growth Rate (cm/yr)",
     xlab = ("Light Availability"),
     pch=16)

model=nls(y~a*(x-c)/(a/s+x-c), trace = TRUE,
          start=c(a=50,s=2,c=8))
summary(model)
 

#Estimate Std. Error t value Pr(>|t|)

p=(coef(model))
a.hat=p[1]
a.hat
s.hat=p[2]
s.hat
c.hat=p[3]
c.hat
yhat=predict(model)
lines(x,yhat,col="red")

## Incorporaating Priors into MLE 

## keeping other parameters as best one from model 
c = c.hat
s = s.hat
sigma2 = summary(model)$sigma

## initializing alpha values and total likelihood 
aseq = seq(1,100,by=0.001)
llh = numeric(length(aseq))
i = 1 

for(a in aseq){
  #calculate expected value with function
  expect = a*(x-c)/(a/s+x-c)
  #likelihood of data
  likeli = prod(dnorm(y,expect,sd = sigma2,log = FALSE))
  #log likelihood of data 
  likeli = log(likeli)
  #likelihood of alpha 
  alikeli = dnorm(a, mean = 35, sd = 4.25, log = FALSE)
  #log likelihood of alpha 
  alikeli = log(alikeli)
  #total likelihood 
  llh[i] = likeli + alikeli
  
  #index
  i = i + 1
  
}

#Maximum likelihood 
a_best = aseq[which.max(llh)]
a_best
