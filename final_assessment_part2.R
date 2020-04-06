#Brexit poll analysis - Part 2

# suggested libraries
library(tidyverse)
# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
    mutate(x_hat = (spread + 1)/2)
# final proportion voting "Remain"
p <- 0.481

#Question 4: Confidence intervals for polls in June
#How many polls are in june_polls?
june_polls <- brexit_polls %>%
              filter(enddate >= "2016-06-01")
    nrow(june_polls)
#What proportion of polls have a confidence interval that covers the value 0?
d = -0.038
june_polls <- brexit_polls %>% filter(enddate >="2016-06-01") %>% mutate(se_x_hat = 2 * sqrt(x_hat * (1-x_hat)/samplesize)) 
%>% mutate(lower=spread - qnorm(0.975)*se_x_hat, upper=spread + qnorm(0.975)*se_x_hat) %>% mutate(hit = d <=upper & d >=lower)
june_polls %>% summarize(n=n())
#What proportion of polls predict "Remain" (confidence interval entirely above 0)?
ind <- june_polls$lower >= 0
mean(ind)
 #What proportion of polls have a confidence interval covering the true value of  𝑑 ?
ind <- june_polls$hit
mean(ind)

#Question 5: Hit rate by pollster
#Group and summarize the june_polls object by pollster to find the proportion of hits 
#for each pollster and the number of polls per pollster. Use arrange() to sort by hit rate.
x <- june_polls %>% group_by(pollster) %>% summarize(proportion_hits=mean(hit), n=n()) 
%>% arrange(desc(proportion_hits))

#Question 6: Boxplot of Brexit polls by poll type
#Make a boxplot of the spread in june_polls by poll type.
june_polls %>% ggplot(aes(group=poll_type, x=poll_type,  y=spread)) + geom_boxplot() + geom_point()

#Question 7: Combined spread across poll type
#Calculate the confidence intervals of the spread combined across all polls in june_polls, grouping by poll type.
#Use this code to begin your analysis:
combined_by_type <- june_polls %>%
        group_by(poll_type) %>%
        summarize(N = sum(samplesize),
                  spread = sum(spread*samplesize)/N,
                  p_hat = (spread + 1)/2)
#What is the lower bound of the 95% confidence interval for online voters?
type <- combined_by_type %>% mutate(lower_bound=spread - qnorm(0.975)*2 * sqrt(p_hat * (1-p_hat)/N), 
  upper_bound=spread + qnorm(0.975)*2 * sqrt(p_hat * (1-p_hat)/N))
type %>%
    filter(poll_type == "Online") %>%
    select(lower_bound)
#What is the upper bound of the 95% confidence interval for online voters?
type %>%
    filter(poll_type == "Online") %>%
    select(upper_bound)