#############################################
### Calculating 2 types of intervals for
### any probability density function in R
#############################################

###
### First I will generate data from a 
### probability density function
### In this example, I am going to use the
### gamma distribution. In your analysis,
### you will use your MCMC chains for each
### marginal posterior instead
###

# Set parameters for the gamma distribution
shape = 1  # Shape parameter of the gamma distribution
rate = 1   # Rate parameter of the gamma distribution

# Generate gamma-distributed random numbers for posterior sampling
n.iter = 10000  # Number of iterations for sampling
Y = rgamma(n.iter, shape, rate)  # Generate random numbers from the gamma distribution

# Plot a histogram to visualize the distribution of generated random numbers
hist(Y, breaks = 100)

# Sort the generated random numbers for posterior density calculation
y.sort = sort(Y)

# Display the first few sorted values (optional, for demonstration purposes)
head(y.sort)

# Calculate the posterior density for each sorted value (used in HPD calculation)
post.den = dgamma(y.sort, shape, rate)

# Normalize the posterior density to sum up to 1
norm.post.den = post.den / sum(post.den)

# Ensure that the sum of normalized posterior density is close to 1 (optional, for validation)
sum(norm.post.den)

# Calculate the cumulative sum of the normalized posterior density
# This step is essential for computing the highest posterior density (HPD) interval.
# The cumulative sum allows us to determine the range of values that encompass a certain probability mass.
# Specifically, it helps identify the lower and upper bounds of the HPD interval by finding the values
# at which the cumulative sum reaches certain thresholds, such as 0.025 and 0.975 for a 95% HPD interval.
cum.post.den = cumsum(norm.post.den)

cum.post.den = cumsum(norm.post.den)

# Plot the cumulative distribution function to visualize the posterior distribution
plot(cum.post.den, type = 'l')

# Find the index corresponding to the lower bound of HPD interval
lower.index = head(which(cum.post.den >= 0.025), 1)

# Find the index corresponding to the upper bound of HPD interval
upper.index = tail(which(cum.post.den <= 0.975), 1)

# Calculate the lower and upper bounds of the highest posterior density (HPD) interval
hpd.lower = y.sort[lower.index]
hpd.upper = y.sort[upper.index]

# Display the lower and upper bounds of the HPD interval
hpd.lower
hpd.upper

# Plot the density estimate of the generated random numbers
plot(density(Y))

# Add vertical lines representing the HPD interval on the density plot
abline(v = hpd.lower, col = 2, lwd = 3)
abline(v = hpd.upper, col = 2)

# Calculate the lower and upper bounds of the symmetric posterior density (SPD) interval
spd.lower = quantile(Y, 0.025)
spd.upper = quantile(Y, 0.975)

# Add vertical lines representing the SPD interval on the density plot
abline(v = spd.lower, col = 4)
abline(v = spd.upper, col = 4)
