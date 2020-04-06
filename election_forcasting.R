#Election Forecasting

# 1 - Confidence Intervals of Polling Data
# Load the libraries and data
library(dplyr)
library(dslabs)
data("polls_us_election_2016")

# Create a table called `polls` that filters by  state, date, and reports the spread
polls <- polls_us_election_2016 %>% 
  filter(state != "U.S." & enddate >= "2016-10-31") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
head(polls)

# Create an object called `cis` that has the columns indicated in the instructions
p <- polls$rawpoll_clinton/100
X_hat <- (polls$spread + 1)/2
se  <- 2 * sqrt(X_hat *(1-X_hat)/polls$samplesize)
p <- polls %>% mutate(lower=spread - qnorm(0.975)*se, upper=spread + qnorm(0.975)*se)
cis <- p %>% select(state, startdate, enddate, pollster, grade, spread, lower, upper)

# 2 - Compare to Actual Results
# Add the actual results to the `cis` data set
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
head(add)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of confidence intervals that contain the actual value. 
#Print this object to the console.
p_hits <- ci_data %>% mutate(hit= lower <= actual_spread & upper >= actual_spread)  %>% summarize(avg=mean(hit))
p_hits
# 3 - Stratify by Pollster and Grade
# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each pollster that has at least 5 polls.
p_hits <- ci_data %>% group_by(pollster) %>% filter(n()>=5) %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) 
%>% summarize(proportion_hits=mean(hit),n=n(), grade=grade[[1]]) %>% arrange(desc(proportion_hits))
p_hits

# 4 - Stratify by State
# The `cis` data have already been loaded for you
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has more than 5 polls.
p_hits <- ci_data %>% group_by(state) %>% filter(n()>=5) %>% mutate(hit = lower <= actual_spread & upper >= actual_spread) 
%>% summarize(proportion_hits=mean(hit),n=n()) %>% arrange(desc(proportion_hits))
p_hits

# 5- Plotting Prediction Results
# The `p_hits` data have already been loaded for you. Use the `head` function to examine it.
head(p_hits)

# Make a barplot of the proportion of hits for each state
p_hits %>% ggplot(aes(y=proportion_hits, x=state)) + geom_bar(stat="identity") + coord_flip()

# 6 - Predicting the Winner
# The `cis` data have already been loaded. Examine it using the `head` function.
head(cis)

# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error=spread-actual_spread, hit = sign(actual_spread)== sign(spread))

# Examine the last 6 rows of `errors`
tail(errors)

# 7 - Plotting Prediction Results
# Create an object called `errors` that calculates the difference between the predicted and actual spread and indicates if the correct winner was predicted
errors <- cis %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))

# Create an object called `p_hits` that summarizes the proportion of hits for each state that has 5 or more polls
p_hits <- errors %>% group_by(state) %>% filter(n()>=5) %>% summarize(proportion_hits=mean(hit),n=n()) 

# Make a barplot of the proportion of hits for each state
p_hits %>% arrange(order(proportion_hits))%>% ggplot(aes(y=proportion_hits, x=state)) + geom_bar(stat = "identity") + coord_flip()

# 8 - Plotting the Errors
# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate a histogram of the error
hist(errors$error)

# Calculate the median of the errors. Print this value to the console.
median(errors$error)

# 9- Plot Bias by State
# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Create a boxplot showing the errors by state for polls with grades B+ or higher
errors %>% filter(grade %in% c('A+', 'A', 'A-', 'B+')) 
%>% ggplot(aes(y=error, x=reorder(state,-error))) + geom_boxplot() + geom_point() + coord_flip()

# 10 - Filter Error Plot
# The `errors` data have already been loaded. Examine them using the `head` function.
head(errors)

# Create a boxplot showing the errors by state for states with at least 5 polls with grades B+ or higher
errors %>% group_by(state) %>% filter(n()>=5,grade %in% c('A+', 'A', 'A-', 'B+')) %>% ungroup() 
%>% ggplot(aes(y=error, x=reorder(state,-error))) + geom_boxplot() + geom_point() + coord_flip()