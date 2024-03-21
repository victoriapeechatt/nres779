sample1 = 1/(rgamma(100,shape = 0.01, rate= 0.01))
hist(sample1)

library("invgamma")
sample2 = rinvgamma(100, shape = 0.01, rate = 0.01)
hist(sample2)


plot(density(sample1))

plot(density(sample1))

plot(density(sample2))

library(LaplacesDemon)
sample3 = rhalfcauchy(100, 10)
hist(sample3)
plot(density(sample3))
