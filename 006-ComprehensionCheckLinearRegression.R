'Comprehension Check: Linear Regression'
'Q1'
'Create a data set using the following code:'
library(dplyr)
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

'Use the caret package to partition the dataset into test and training sets of equal size. Train a linear model and calculate the RMSE. Repeat this exercise 100 times and report the mean and standard deviation of the RMSEs. (Hint: You can use the code shown in a previous course inside a call to replicate using a seed of 1.'
library(caret)

set.seed(1)
rmse_dat <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  fit <- lm(y~x, data=train_set)
  y_hat <- predict(fit, test_set)
  
  RMSE<-sqrt(mean((y_hat - test_set$y)^2))
  
})

mean(rmse_dat)
sd(rmse_dat)

'Q2
Now we will repeat the above but using larger datasets. Repeat the previous exercise but for datasets with n <- c(100, 500, 1000, 5000, 10000). Save the average and standard deviation of RMSE from the 100 repetitions using a seed of 1. Hint: use the sapply or map functions.'

set.seed(1)
RSME_fun <- function(a){
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = a, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  rmse_dat <- replicate(100, {
    test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
    
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    
    fit <- lm(y~x, data=train_set)
    y_hat <- predict(fit, test_set)
    
    RMSE<-sqrt(mean((y_hat - test_set$y)^2))
    
  })
  
  list(mean(rmse_dat), sd(rmse_dat))
}

a <- c(100, 500, 1000, 5000, 10000)
set.seed(1)
sapply(a, RSME_fun)


'Q3
What happens to the RMSE as the size of the dataset becomes larger?'
#On average, the RMSE does not change much as n gets larger, but the variability of the RMSE decreases. 

'Q4
Now repeat the exercise from Q1, this time making the correlation between x and y larger, as in the following code:'


set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


set.seed(1)
rmse_dat <- replicate(100, {
  test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)
  
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  
  fit <- lm(y~x, data=train_set)
  y_hat <- predict(fit, test_set)
  
  RMSE<-sqrt(mean((y_hat - test_set$y)^2))
  
})

mean(rmse_dat) #0.9099808
sd(rmse_dat) #0.06244347

'Q5
Which of the following best explains why the RMSE in question 4 is so much lower than the RMSE in question 1?'
#When we increase the correlation between x and y, x has more predictive power and thus provides a better estimate of y.

'Q6
1 point possible (graded)
Create a data set using the following code.
Note that y is correlated with both x_1 and x_2 but the two predictors are independent of each other, as seen by cor(dat).
Use the caret package to partition into a test and training set of equal size. Compare the RMSE when using just x_1, just x_2 and both x_1 and x_2. Train a linear model for each.
Which of the three models performs the best (has the lowest RMSE)?'

set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y~x_1, data=train_set)
y_hat <- predict(fit, test_set)

RMSE<-sqrt(mean((y_hat - test_set$y)^2))
mean(RMSE)
#0.6708231
sd(RMSE)
#Na

fit <- lm(y~x_2, data=train_set)
y_hat <- predict(fit, test_set)

RMSE<-sqrt(mean((y_hat - test_set$y)^2))
mean(RMSE)
#0.6775359
sd(RMSE)

fit <- lm(y~x_1 + x_2, data=train_set)
y_hat <- predict(fit, test_set)

RMSE<-sqrt(mean((y_hat - test_set$y)^2))
mean(RMSE)
#0.3552544
sd(RMSE)
#x_1 and x_2

'Q7
Report the lowest RMSE of the three models tested in Q6.'
set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y~x_1 + x_2, data=train_set)
y_hat <- predict(fit, test_set)

RMSE<-sqrt(mean((y_hat - test_set$y)^2))
mean(RMSE) #0.3070962

'Q8
Repeat the exercise from q6 but now create an example in which x_1 and x_2 are highly correlated.
set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
data.frame() %>% setNames(c("y", "x_1", "x_2"))
Use the caret package to partition into a test and training set of equal size. Compare the RMSE when using just x_1, just x_2, and both x_1 and x_2.
Compare the results from q6 and q8. What can you conclude?'

set.seed(1)
n <- 1000
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

set.seed(1)
test_index <- createDataPartition(dat$y, times = 1, p = 0.5, list = FALSE)

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

fit <- lm(y~x_1, data=train_set)
y_hat <- predict(fit, test_set)
RMSE<-sqrt(mean((y_hat - test_set$y)^2))
mean(RMSE)

fit_x2 <- lm(y~x_2, data=train_set)
y_hat <- predict(fit, test_set)
RMSE<-sqrt(mean((y_hat - test_set$y)^2))
mean(RMSE)

fit_x1_x2 <- lm(y~x_1 + x_2, data=train_set)
y_hat <- predict(fit, test_set)
RMSE<-sqrt(mean((y_hat - test_set$y)^2))
mean(RMSE)

#Adding extra predictors can improve RMSE substantially, but not when the added predictors are highly correlated with other predictors. 

