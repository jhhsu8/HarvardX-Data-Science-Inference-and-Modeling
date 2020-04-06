#Confidence Intervals and p-Values
# 1. Confidence interval for p
# Load the data
data(polls_us_election_2016)

# Generate an object `polls` that contains data filtered for polls that ended on or after October 31, 2016 in the United States
polls <- polls_us_election_2016 %>% filter(enddate >= '2016-10-31' & state =='U.S.')
polls

# How many rows does `polls` contain? Print this value to the console.
nrow(polls)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls[1,'samplesize']
N

# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
X_hat <- polls[1,'rawpoll_clinton']/100
X_hat

# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat

# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(X_hat - qnorm(0.975)*se_hat, X_hat + qnorm(0.975)*se_hat)
ci

# 2. Pollster results for p
# The `polls` object that filtered all the data by date and nation has already been loaded. Examine it using the `head` function.
head(polls)

# Create a new object called `pollster_results` that contains columns for pollster name, end date, X_hat, se_hat, lower confidence interval, and upper confidence interval for each poll.
x <- polls[,'rawpoll_clinton']/100
N <- polls[,'samplesize']
se <- sqrt(x*(1-x)/N)
l = x - qnorm(0.975)*se
u = x + qnorm(0.975)*se
pollster_results <-polls %>% mutate(X_hat = x, se_hat = se, lower = l, upper = u) %>% select(pollster,enddate,X_hat, se_hat, lower, upper)
pollster_results

# 3. Comparing to actual results - p
# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)

# Add a logical variable called `hit` that indicates whether the actual value exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`
p=0.482
avg_hit <- pollster_results %>% mutate(hit = p >= lower & p <= upper) %>% summarize(mean(hit))
avg_hit
# 4. Confidence interval for d
# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.") 
polls <- polls %>% mutate(d_hat = polls[,'rawpoll_clinton']/100 -polls[,'rawpoll_trump']/100 )

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls[1,'samplesize']
N
# Assign the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
d_hat <- polls[1,'d_hat']
# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <-(d_hat + 1)/2
# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- 2 * (sqrt(X_hat*(1-X_hat)/N))
# Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(d_hat - qnorm(0.975)*se_hat, d_hat + qnorm(0.975)*se_hat)
ci

# 5. Pollster results for d
# The subset `polls` data with 'd_hat' already calculated has been loaded. Examine it using the `head` function.
head(polls)
# Create a new object called `pollster_results` that contains columns for pollster name, end date, d_hat, lower confidence interval of d_hat, and upper confidence interval of d_hat for each poll.
N <- polls[,'samplesize']
d_hat <- polls[,'d_hat']
X_hat <-(d_hat + 1)/2
se_hat <- 2 * (sqrt(X_hat*(1-X_hat)/N))
se_hat
lower <- d_hat - qnorm(0.975)*se_hat
upper <- d_hat + qnorm(0.975)*se_hat
pollster_results <- polls %>% mutate(d_hat = d_hat, lower = lower, upper = upper) %>% select(pollster,enddate,d_hat, lower, upper)

# 6. Comparing to actual results - d
# The `pollster_results` object has already been loaded. Examine it using the `head` function.
head(pollster_results)
# Add a logical variable called `hit` that indicates whether the actual value (0.021) exists within the confidence interval of each poll. Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value. Save the result as an object called `avg_hit`.
d=0.021
avg_hit <- pollster_results %>% mutate(hit = d >= lower & d <= upper) %>% summarize(mean(hit))
avg_hit

# 7. Comparing to actual results by pollster
# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)
# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster.
polls <- polls %>% mutate(errors = d_hat - 0.021)
polls
polls %>% ggplot(aes(x = d_hat, y = errors)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 8. Comparing to actual results by pollster - multiple polls
# The `polls` object has already been loaded. Examine it using the `head` function.
head(polls)
# Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. Then make a plot of the error stratified by pollster, but only for pollsters who took 5 or more polls.
polls <- polls %>% mutate(errors = d_hat - 0.021) %>% group_by(pollster) %>% filter(n()>=5)
polls %>% ggplot(aes(x = d_hat, y = errors)) +
  geom_point() 
