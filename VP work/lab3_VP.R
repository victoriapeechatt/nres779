
d=read.csv("Documents/GitHub/NRES779/VP work/HemlockData.csv")
x=d$Light
y=d$ObservedGrowthRate
plot(x,y,
     ylab="Growth Rate (cm/yr)",
     xlab = ("Light Availability"),
     pch=16)

likelihood_a = function(a, mean)
  
  dnorm(a,mean = 35, sd=4.25,log = TRUE)

model=nls(y~a*(x-c)/(a/s+x-c), trace = TRUE,
          start=c(a=a,s=2,c=8))
summary(model)

#Estimate Std. Error t value Pr(>|t|)

p=(coef(model))
a.hat=p[1]
s.hat=p[2]
c.hat=p[3]
yhat=predict(model)
lines(x,yhat,col="red")


#### 

likelihood_a = dnorm(1,mean = 35, sd=4.25,log = TRUE)

model= a*(x-c)/(a/s+x-c)

summary(model)

