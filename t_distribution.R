#The t-distribution

# 1 - Using the t-Distribution
# Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'.
pt(-2,3) + (1-pt(2,3))

# 2 - Plotting the t-distribution
# Generate a vector 'df' that contains a sequence of numbers from 3 to 50
df <- seq(3,50)

# Make a function called 'pt_func' that calculates the probability that a value is more than |2| for any degrees of freedom 
pt_func <- function(df) {
  pt(-2,df) + (1-pt(2,df))
}

# Generate a vector 'probs' that uses the `pt_func` function to calculate the probabilities
probs <- sapply(df, pt_func)
probs

# Plot 'df' on the x-axis and 'probs' on the y-axis
plot(x=df, y=probs)

# 3 - Sampling From the Normal Distribution
# Load the neccessary libraries and data
library(dslabs)
library(dplyr)
data(heights)

# Use the sample code to generate 'x', a vector of male heights
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'
mu <- mean(x)
N <- 15
B <- 10000

# Use the `set.seed` function to make sure your answer matches the expected result after random sampling
set.seed(1)

# Generate a logical vector 'res' that contains the results of the simulations
res <- replicate(B, {
s <- sample(x,N,replace=TRUE) 
  interval <- c(mean(s)-qnorm(0.975)*sd(s)/sqrt(N), mean(s)+qnorm(0.975)*sd(s)/sqrt(N))
  between(mu, interval[1], interval[2])
}
)
mean(res)

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
# 4 - Sampling from the t-Distribution
# The vector of filtered heights 'x' has already been loaded for you. Calculate the mean.
mu <- mean(x)

# Use the same sampling parameters as in the previous exercise.
set.seed(1)
N <- 15
B <- 10000

# Generate a logical vector 'res' that contains the results of the simulations using the t-distribution
res <- replicate(B, {
s <- sample(x,N,replace=TRUE) 
  interval <- c(mean(s)-qt(0.975, N-1)*sd(s)/sqrt(N), mean(s)+qt(0.975,N-1)*sd(s)/sqrt(N))
  between(mu, interval[1], interval[2])
  })

# Calculate the proportion of times the simulation produced values within the 95% confidence interval. Print this value to the console.
mean(res)