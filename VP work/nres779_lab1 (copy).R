## NRES Week #1

# Question 1

# Define a logistic equation function
disc_log_eq = function(lambda, x){lambda * x * (1 - x)}

# Generate sequences for lambdas and times
lambdas = seq(0.25, 4.0, by = 0.25)
times = seq(2, 30, by = 1.0)

# Initialize a matrix for storing results
x = matrix(0.01, 16, 30)

# Populate the matrix using logistic equation
for (i in 1:length(lambdas)) {
  for (t in 1:length(times)) {
    x[i, t + 1] = lambdas[i] * x[i, t] * (1 - x[i, t])
  }
}

# Create a 4x4 grid of plots for each lambda
par(mfrow = c(4, 4))
z = as.character(lambdas)
for (i in 1:length(lambdas)) {
  titlet = bquote(lambda == .(z[i]))
  plot(1:30, x[i,], main = titlet, xlab = "Time (1-30)", ylab = expression(x[t]), type = "l")
}

# Question 2

# Generate sequences for lambdas2 and times2
lambdas2 = seq(1, 4.0, by = 0.05)
times2 = seq(2, 100, by = 1)

# Initialize a matrix for storing results
xsim = matrix(0.01, 61, 100)

# Populate the matrix using logistic equation
for (i in 1:length(lambdas2)) {
  for (t in 1:length(times2)) {
    xsim[i, t + 1] = lambdas2[i] * xsim[i, t] * (1 - xsim[i, t])
  }
}

# Extract the second half of the matrix and add lambdas2 as a column
xsimhalved = xsim[, 51:100]
xsimhalved = cbind(xsimhalved, lambdas2)

# Plotting Bifurcation Diagram
par(mfrow = c(1, 1))
simgraph = plot(1, type = "n", xlim = c(1, 4), ylim = c(0, 1), xlab = expression(gamma), ylab = expression(x[t]), main = "Bifurcation Diagram")

# Plot points for each lambda in different colors
for (i in 1:50) {
  points(xsimhalved[1:61, 51], xsimhalved[1:61, i], col = i, pch = 20)
}

# Question 3

# Read data from CSV
  data <- read.csv("Documents/UNR/2024_SPRING/NRES779/Labs/RMNP elk time series.csv")
  Nobs = data$Population_size
  Nyear = data$Year
  Nerror = data$SE
  rates <- seq(0.1, 0.3, by = 0.01)
  Kcap <- seq(800, 1200, by = 5)
  Ninit <- seq(200, 500, by = 5)
  counter = 1
  N = 0
  SSEarray = array(0, dim = c(1, 4, length(Ninit) * length(Kcap) * length(rates)))

## Loops to go through different values of Ninit, Kcap, and Rate and finding SSE
for (n in 1:length(Ninit)) {
  for (k in 1:length(Kcap)) {
    for (r in 1:length(rates)) {
      SSE <- 0
      
      # Loop through data points and calculate SSE
      for (i in 1:34) {
        N[1] = Ninit[n]
        SSE[i] = (Nobs[i] - N[i])^2
        N[i + 1] = N[i] + rates[r] * N[i] * (1 - (N[i] / Kcap[k]))
      }
      
      # Sum of squared errors
      SSE = sum(SSE)
      SSEarray[1, 1, counter] = Ninit[n]
      SSEarray[1, 2, counter] = Kcap[k]
      SSEarray[1, 3, counter] = rates[r]
      SSEarray[1, 4, counter] = SSE
      counter = counter + 1
    }
  }
}

# Find the index of the minimum SSE in the SSEarray
min_SSE_index <- which.min(SSEarray[1, 4, ])
min_SSE = SSEarray[1,, min_SSE_index, drop = FALSE]
min_SSE

best_n = SSEarray[1, 1, min_SSE_index]
best_k = SSEarray[1, 2, min_SSE_index]
best_r = SSEarray[1, 3, min_SSE_index]

## Plotting SSE as a function of each parameter while the others are their best value
par(mfrow = c(1, 3))

indices_n <- which(SSEarray[1, 2, ] == best_k & SSEarray[1, 3, ] == best_r)
if (length(indices_n) > 0) {
  plot(SSEarray[1, 1, indices_n], SSEarray[1, 4, indices_n], type = "l", col = "blue", xlab = "Ninit", ylab = "SSE", main = "SSE vs. Ninit")
} 

indices_k <- which(SSEarray[1, 1, ] == best_n & SSEarray[1, 3, ] == best_r)
if (length(indices_k) > 0) {
  plot(SSEarray[1, 2, indices_k], SSEarray[1, 4, indices_k], type = "l", col = "red", xlab = "Kcap", ylab = "SSE", main = "SSE vs. Kcap")
} 

indices_r <- which(SSEarray[1, 1, ] == best_n & SSEarray[1, 2, ] == best_k)
if (length(indices_r) > 0) {
  plot(SSEarray[1, 3, indices_r], SSEarray[1, 4, indices_r], type = "l", col = "green", xlab = "Rates", ylab = "SSE", main = "SSE vs. Rates")
} 

## Plotting data with model
par(mfrow = c(1, 1))

# Fit the logistic model to the data
Nfit = best_n
for (i in 1:(length(Nyear)-1)){
  
  Nfit[i+1] = Nfit[i] + best_r * Nfit[i] * (1 - (Nfit[i]/best_k))
  
}

# Plot the points, errors, and best fit 
plot(Nyear[1], Nobs[1], col = "black", pch = 16, xlim = range(Nyear), 
     ylim = c(0,1800), 
     xlab = "Time (years)", 
     ylab = "Population Size of Elk", 
     main = "Logistic model of growth of the Rocky Mountain National Park elk population")

for (i in 1:length(Nyear)) {
  
  points(Nyear[i], Nobs[i], col = "black", pch = 16)
  arrows(Nyear[i], Nobs[i] - Nerror[i], Nyear[i], Nobs[i] + Nerror[i], angle = 180, code = 3, length = 0.1)
}

lines(Nfit ~ Nyear, col = 'blue')
##

