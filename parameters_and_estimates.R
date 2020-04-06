#Parameters and Estimates

# 1. se versus p
# `N` represents the number of people polled
N <- 25

# Create a variable `p` that contains 100 proportions ranging from 0 to 1 using the `seq` function
p <- seq(0,1,length.out=100)

# Create a variable `se` that contains the standard error of each sample average
se <- sqrt(p * (1-p)/N)

# Plot `p` on the x-axis and `se` on the y-axis
plot(x=p,y=se)

# 2. Multiple plots of se versus p
# The vector `p` contains 100 proportions of Democrats ranging from 0 to 1 using the `seq` function
p <- seq(0, 1, length = 100)

# The vector `sample_sizes` contains the three sample sizes
sample_sizes <- c(25, 100, 1000)

# Write a for-loop that calculates the standard error `se` for every value of `p` for each of the three samples sizes `N` in the vector `sample_sizes`. Plot the three graphs, using the `ylim` argument to standardize the y-axis across all three plots.
for (N in sample_sizes) {
  se<-sqrt(p * (1-p)/N)  
  plot(x=p, y=se, ylim=c(0,0.1))

}
 
# 3. Expected value of d
#E[Xbar−(1−Xbar)] = E[2Xbar−1] = 2E[Xbar]−1 = 2p−1 = p−(1−p)
 
# 4. Standard error of d
#SE[Xbar−(1−Xbar)]=SE[2Xbar−1] =2SE[Xbar] = 2 * sqrt*p(1−p)/N)

# 5. Standard error of the spread
# `N` represents the number of people polled
N <- 25

# `p` represents the proportion of Democratic voters
p <- 0.45

# Calculate the standard error of the spread. Print this value to the console.
2* sqrt(p*(1-p)/N)