#Comprehension Check: Trees and Random Forests
#Q1
#Create a simple dataset where the outcome grows 0.75 units on average for every increase in a predictor, using this code:
  
library(tidyverse)
library(dslabs)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(Lahman)
library(HistData)
library(caret)
library(e1071)
library(matrixStats)

library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)
#Which code correctly uses rpart to fit a regression tree and saves the result to fit?
fit <- rpart(y~., data = dat)

#Q2
#Which of the following plots has the same tree shape obtained in Q1?

plot(fit, margin = 0.1)
text(fit, cex = 0.75)

#Answer 4.

#Q3
#Below is most of the code to make a scatter plot of y versus x along with the predicted values based on the fit.

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x,y_hat),col=2) #col=2 means red
#  Which line of code should be used to replace #BLANK in the code above?


#Q4
#Now run Random Forests instead of a regression tree using randomForest from the __randomForest__ package, and remake the scatterplot with the prediction line. Part of the code is provided for you below.

library(randomForest)
fit <- randomForest(y ~., data = dat) 
  dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = 2)
#What code should replace #BLANK in the provided code?
#randomForest(y ~ x, data = dat) correct
  
  
#Q5
#Use the plot function to see if the Random Forest from Q4 has converged or if we need more trees.
#Which is the correct plot to assess whether the Random Forest has converged?  

  plot(fit)
  
#Answer: 3
  
#Q6
#It seems that the default values for the Random Forest result in an estimate that is too flexible (unsmooth). Re-run the Random Forest but this time with a node size of 50 and a maximum of 25 nodes. Remake the plot.
#Part of the code is provided for you below.
  
  library(randomForest)
  fit <- randomForest(y ~ x, data = dat, nodesize = 50, maxnodes = 25)
    dat %>% 
    mutate(y_hat = predict(fit)) %>% 
    ggplot() +
    geom_point(aes(x, y)) +
    geom_step(aes(x, y_hat), col = 2)
#  What code should replace #BLANK in the provided code?
  