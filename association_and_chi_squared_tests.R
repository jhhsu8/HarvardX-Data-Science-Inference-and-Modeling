#Association and Chi-Squared Tests
 
# 1 - Comparing Proportions of Hits
# The 'errors' data have already been loaded. Examine them using the `head` function.
head(errors)

# Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
totals <- errors %>% filter(grade %in% c('A-','C-')) %>% group_by(grade, hit) %>% summarize(n=n())%>% spread(hit, 1-hit)
 totals <- tibble(hit=c(FALSE,TRUE),
`C-`= c(totals$`FALSE`[1],totals$`TRUE`[1]),
`A-`= c(totals$`FALSE`[2],totals$`TRUE`[2]))

# Print the proportion of hits for grade A- polls to the console
a <- totals[totals$hit == TRUE, 'A-']/(totals[totals$hit == TRUE, 'A-'] + totals[totals$hit == FALSE, 'A-'])
a[,1]

# Print the proportion of hits for grade C- polls to the console
c <- totals[totals$hit == TRUE, 'C-']/(totals[totals$hit == TRUE, 'C-'] + totals[totals$hit == FALSE, 'C-'])
c[,1]

# 2 - Chi-squared Test
# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Perform a chi-squared test on the hit data. Save the results as an object called 'chisq_test'.
chisq_test <- totals %>% select(-hit) %>% chisq.test()

# Print the p-value of the chi-squared test to the console
chisq_test$p.value

# 3 - Odds Ratio Calculation
# The 'totals' data have already been loaded. Examine them using the `head` function.
head(totals)

# Generate a variable called `odds_C` that contains the odds of getting the prediction right for grade C- polls
odds_C <- totals$`C-`[2]/totals$`C-`[1]
odds_C

# Generate a variable called `odds_A` that contains the odds of getting the prediction right for grade A- polls
odds_A <- totals$`A-`[2]/totals$`A-`[1]
odds_A

# Calculate the odds ratio to determine how many times larger the odds ratio is for grade A- polls than grade C- polls
odds_A/odds_C