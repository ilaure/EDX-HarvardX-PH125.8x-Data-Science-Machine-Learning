#Comprehension Check: Nearest Neighbors
#Q1
#Previously, we used logistic regression to predict sex based on height. Now we are going to use knn to do the same. Set the seed to 1, then use the caret package to partition the dslabs "heights" data into a training and test set of equal size. Use the sapply function to perform knn with k values of seq(1, 101, 3) and calculate F_1 scores.

library(caret)
library(dslabs)
library(tidyverse)
set.seed(1)
data("heights")
test_index <- createDataPartition(heights$sex, times = 1, p=0.5, list= FALSE)
train_set <- heights[-test_index,]
test_set <-  heights[test_index,]
ks <- seq(1, 101, 3)
result <- sapply(ks, function(k){
  
  fit <- knn3(sex ~ height, train_set, k = k )
  y_hat <- predict(fit, test_set ,type = "class") %>%
    factor(levels(train_set$sex))
  F_meas(data = y_hat, reference = test_set$sex)
})
plot(ks,result)
#What is the max value of F_1?
max(result)
#0.6019417
#At what value of k does the max occur?
ks[which.max(result)]
#46

#Q2
#Next we will use the same gene expression example used in the Comprehension Check: Distance exercises. You can load it like this:
  

library(dslabs)
data("tissue_gene_expression")

#Split the data into training and test sets, and report the accuracy you obtain. Try it for k = 1, 3, 5, 7, 9, 11. Set the seed to 1.

set.seed(1)
library(caret)
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
train_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[train_index,], y[train_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[-train_index,]),
                   type = "class")
  mean(y_hat == y[-train_index])
})

#0.9784946 0.9677419 0.9892473 0.9677419 0.9569892 0.9569892
