#Cross-validation   Comprehension Check: Bootstrap
#Q1
#The createResample function can be used to create bootstrap samples. For example, we can create 10 bootstrap samples for the mnist_27 dataset like this:
#set.seed(1995)
#indexes <- createResample(mnist_27$train$y, 10)
#How many times do 3, 4, and 7 appear in the first resampled index?

library(dslabs)
library(caret)
library(tidyverse)
set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)

#Expected Answer : 1, 4, 0 

#Q2
#We see that some numbers appear more than once and others appear no times. This has to be this way for each dataset to be independent. Repeat the exercise for all the resampled indexes.
#What is the total number of times that 3 appears in all of the resampled indexes?

x=sapply(indexes, function(ind){
  sum(ind == 3)
})
sum(x)

#11

#Q3
#Generate a random dataset using the following code:
#y <- rnorm(100, 0, 1)
#Estimate the 75th quantile, which we know is qnorm(0.75), with the sample quantile: quantile(y, 0.75).
#Set the seed to 1 and perform a Monte Carlo simulation with 10,000 repetitions, generating the random dataset and estimating the 75th quantile each time. What is the expected value and standard error of the 75th quantile?

y <- rnorm(100, 0, 1)
set.seed(1)
B <- 10000
q_75 <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(q_75)
sd(q_75)

#Expected Value: 0.666
#Std Error: 0.135


#Q4
#In practice, we can't run a Monte Carlo simulation. Use the sample:
#set.seed(1)
#y <- rnorm(100, 0, 1)
#Set the seed to 1 again after generating y and use 10 bootstrap samples to estimate the expected value and standard error of the 75th quantile.

set.seed(1)
indexes <- createResample(y, 10)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

#Expected Value: 0.731
#Std Error: 0.0742


#Q5
#Repeat the exercise from Q4 but with 10,000 bootstrap samples instead of 10. Set the seed to 1.
set.seed(1)
indexes <- createResample(y, 10000)
q_75_star <- sapply(indexes, function(ind){
  y_star <- y[ind]
  quantile(y_star, 0.75)
})
mean(q_75_star)
sd(q_75_star)

#Expected Value: 0.674
#Std Error: 0.0931


#Q6
#Compare the SD values obtained using 10 vs 10,000 bootstrap samples.
#What do you observe?

#Answer: The SD is roughly the same in both cases. 
