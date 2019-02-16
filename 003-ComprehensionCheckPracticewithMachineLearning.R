'Comprehension Check: Practice with Machine Learning'
'We will practice building a machine learning algorithm using a new dataset, iris, that provides multiple predictors for us to use to train. To start, we will remove the setosa species and we will focus on the versicolor and virginica iris species using the following code:'
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
'The following questions all involve work with this dataset.'

'Q1'
set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE) # line of code
test <- iris[test_index,]
train <- iris[-test_index,]

'Which code should be used in place of # line of code above?'
#test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)

library(dplyr)
train %>% group_by(Species) %>% summarize(mean(Sepal.Length), sd(Sepal.Length))
train %>% group_by(Species) %>% summarize(mean(Sepal.Width), sd(Sepal.Width))
train %>% group_by(Species) %>% summarize(mean(Petal.Length), sd(Petal.Length))
train %>% group_by(Species) %>% summarize(mean(Petal.Width), sd(Petal.Width))

'Q2'
'Next we will figure out the singular feature in the dataset that yields the greatest overall accuracy. You can use the code from the introduction and from Q1 to start your analysis.
Using only the train iris data set, which of the following is the singular feature for which a smart cutoff (simple search) yields the greatest overall accuracy?'
#Petal.Length

'Q3'
'Using the smart cutoff value calculated on the training data, what is the overall accuracy in the test data?'
library(purrr)
cutoff <- seq(1.3, 7, by = 0.01)
accuracy <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") 
  mean(y_hat == train$Species)
})

plot(cutoff,accuracy)
max(accuracy)
best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

library(magrittr)
y_hat <- ifelse(test$Petal.Length > best_cutoff, "virginica", "versicolor")  
mean(y_hat == test$Species)
foo <- function(x){
  rangedValues <- seq(range(x)[1],range(x)[2],by=0.1) #range(x) gives min max, range(x)[1] gives min, range(x)[2] max
  sapply(rangedValues,function(i){
    y_hat <- ifelse(x>i,'virginica','versicolor')
    mean(y_hat==test$Species)
  })
}
predictions <- apply(test[,-5],2,foo) #train[,-5] means excluding column 5 in table, 2 means manipulate on columns
sapply(predictions,max)
#0.90

'Q4'
'Notice that we had an overall accuracy greater than 96% in the training data, but the overall accuracy was lower in the test data. This can happen often if we overtrain. In fact, it could be the case that a single feature is not the best choice. For example, a combination of features might be optimal. Using a single feature and optimizing the cutoff as we did on our training data can lead to overfitting.
Given that we know the test data, we can treat it like we did our training data to see if the same feature with a different cutoff will optimize our predictions.
Which feature best optimizes our overall accuracy?'
y_hat <- ifelse(iris$Petal.Width > 1.8 , "virginica", "versicolor")%>%
  factor(levels = levels(test$Species))
mean(y == y_hat)
#0.84
#Petal.Width correct

'Q5'
'Now we will perform some exploratory data analysis on the data.
Notice that Petal.Length and Petal.Width in combination could potentially be more information than either feature alone.
Optimize the combination of the cutoffs for Petal.Length and Petal.Width in the train data and report the overall accuracy when applied to the test dataset. For simplicity, create a rule that if either the length OR the width is greater than the length cutoff or the width cutoff then virginica or versicolor is called. (Note, the F1 will be similarly high in this example.)
What is the overall accuracy for the test data now?'

cutoff_width <- sort(train$Petal.Width)
cutoff_length <- sort(train$Petal.Length)

accuracy <- map2_dbl(cutoff_length, cutoff_width, function(x, y) {
  y_hat <- ifelse(train$Petal.Width > y | train$Petal.Length > x, "virginica","versicolor") %>%
    factor(levels = levels(train$Species))
  mean(y_hat == train$Species)
})

max(accuracy)
cutoff_width[which.max(accuracy)]
cutoff_length[which.max(accuracy)]

y_hat <- ifelse(test$Petal.Width > 1.5 & test$Petal.Length > 4.8, "virginica","versicolor") %>%
  factor(levels = levels(test$Species))
mean(y_hat == test$Species)

library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species
plot(iris,pch=21,bg=iris$Species)
set.seed(2)
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
test <- iris[test_index,]
train <- iris[-test_index,]
petalLengthRange <- seq(range(train[,3])[1],range(train[,3])[2],by=0.1)
petalWidthRange <- seq(range(train[,4])[1],range(train[,4])[2],by=0.1)
cutoffs <- expand.grid(petalLengthRange,petalWidthRange)
id <- sapply(seq(nrow(cutoffs)),function(i){
  y_hat <- ifelse(train[,3]>cutoffs[i,1] | train[,4]>cutoffs[i,2],'virginica','versicolor')
  mean(y_hat==train$Species)
}) %>% which.max

optimalCutoff <- cutoffs[id,] %>% as.numeric
y_hat <- ifelse(test[,3]>optimalCutoff[1] & test[,4]>optimalCutoff[2],'virginica','versicolor')
mean(y_hat==test$Species) #0.92