#Brexit poll analysis - Part 3

# suggested libraries
library(tidyverse)
# load brexit_polls object and add x_hat column
library(dslabs)
data(brexit_polls)
brexit_polls <- brexit_polls %>%
    mutate(x_hat = (spread + 1)/2)
# final proportion voting "Remain"
p <- 0.481

#Question 9: Chi-squared p-value
#Define brexit_hit, with the following code, which computes the confidence intervals for all Brexit polls in 2016 
#and then calculates whether the confidence interval covers the actual value of the spread  𝑑=−0.038 :
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

#Use brexit_hit to make a two-by-two table of poll type and hit status. 
#Then use the chisq.test() function to perform a chi-squared test to determine whether the difference in hit rate is significant.
# is the p-value of the chi-squared test comparing the hit rate of online and telephone polls?
table <- brexit_hit %>% group_by(poll_type, hit) %>% summarize(n=n())%>% spread(hit, 1-hit) 
two_by_two <- tibble(hit = c("FALSE", "TRUE"),
Online = c(table$`FALSE`[1], table$`TRUE`[1]),
Telephone = c(table$`FALSE`[2], table$`TRUE`[2]))
chisq_test <- two_by_two %>% select(-hit) %>% chisq.test()
chisq_test$p.value

#Determine which poll type has a higher probability of producing a confidence interval 
#that covers the correct value of the spread. Also determine whether this difference is statistically significant at a p-value cutoff of 0.05. Which of 
# online > telephone
hit_rate <- brexit_hit %>%
    group_by(poll_type) %>%
    summarize(avg = mean(hit))
hit_rate$avg[hit_rate$poll_type == "Online"] > hit_rate$avg[hit_rate$poll_type == "Telephone"]
# statistically significant
chisq_test$p.value$p.value < 0.05

#Question 10: Odds ratio of online and telephone poll hit rate
#Use the two-by-two table constructed in the previous exercise to calculate the odds ratio between the hit rate of online and telephone polls 
#to determine the magnitude of the difference in performance between the poll types.
two_by_two$Online[two_by_two$hit == 'TRUE']/(two_by_two$Online[two_by_two$hit == 'FALSE'])
two_by_two$Telephone[two_by_two$hit == 'TRUE']/(two_by_two$Telephone[two_by_two$hit == 'FALSE'])

#Question 11: Plotting spread over time

#Use brexit_polls to make a plot of the spread (spread) over time (enddate) colored by poll type (poll_type). 
#Use geom_smooth() with method = "loess" to plot smooth curves with a span of 0.4. 
#Include the individual data points colored by poll type. 
#Add a horizontal line indicating the final value of  𝑑=−.038 .
brexit_polls %>%
              ggplot(aes(enddate, spread, color = poll_type)) +
              geom_smooth(method = "loess", span = 0.4) +
              geom_point() +
              geom_hline(aes(yintercept = -.038))
 
#Question 12: Plotting raw percentages over time
#Use the following code to create the object brexit_long, 
#which has a column vote containing the three possible votes on a Brexit poll ("remain", "leave", "undecided") 
#and a column proportion containing the raw proportion choosing that vote option on the given poll:
brexit_long <- brexit_polls %>%
    gather(vote, proportion, "remain":"undecided") %>%
    mutate(vote = factor(vote))
    
#Make a graph of proportion over time colored by vote. Add a smooth trendline with geom_smooth() and method = "loess" with a span of 0.3.
brexit_long <- brexit_polls %>%
  gather(vote, proportion, "remain":"undecided") %>%
  ggplot(aes(enddate, proportion, color = vote)) +
  geom_smooth(method = "loess", span = 0.3) +
  geom_point() 