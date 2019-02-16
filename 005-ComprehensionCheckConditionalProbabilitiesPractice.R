'Comprehension Check: Conditional Probabilities Practice'
'Q1'
'We are now going to write code to compute conditional probabilities for being male in the heights dataset. Round the heights to the closest inch. Plot the estimated conditional probability  for each .
Part of the code is provided here:'

library(dslabs)
library(magrittr)
library(dplyr)
library(ggplot2)
data("heights")
#MISSING CODE
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

'Q2'
'In the plot we just made in Q1 we see high variability for low values of height. This is because we have few data points. This time use the quantile (\ 0.1,0.2,\dots,0.9 \)and the cut function to assure each group has the same number of points. Note that for any numeric vector x, you can create groups based on quantiles like this: cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE).
Part of the code is provided here:'

ps <- seq(0, 1, 0.1)
heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

'Q3'
'You can generate data from a bivariate normal distrubution using the MASS package using the following code.'

Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
ps <- seq(0, 1, 0.1)
dat %>% 
  #MISSING CODE
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)
