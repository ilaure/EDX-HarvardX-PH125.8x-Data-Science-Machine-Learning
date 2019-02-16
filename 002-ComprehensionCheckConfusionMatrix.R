'Comprehension Check: Confusion Matrix'
'The following questions all ask you to work with the dataset described below.'
'The reported_heights and heights datasets were collected from three classes taught in the Departments of Computer Science and Biostatistics, as well as remotely through the Extension School. The Biostatistics class was taught in 2016 along with an online version offered by the Extension School. On 2016-01-25 at 8:15 AM, during one of the lectures, the instructors asked student to fill in the sex and height questionnaire that populated the reported_height dataset. The online students filled out the survey during the next few days, after the lecture was posted online. We can use this insight to define a variable which we will call type, to denote the type of student, inclass or online.'
'The code below sets up the dataset for you to analyze in the following exercises:'

library(dslabs)
library(dplyr)
library(lubridate)

data("reported_heights")
dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)
dat
y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

'QUESTION 1'
'What is the propotion of females in class and online? (That is, calculate the proportion of the in class students who are female and the proportion of the online students who are female.)'

inclass_females <- ifelse(y=='Female'& x =='inclass',TRUE,FALSE)
sum(inclass_females)
total_inclass <- ifelse(x =='inclass',TRUE,FALSE)
sum(total_inclass)online_females <- ifelse(y=='Female'& x =='online',TRUE,FALSE)
sum(online_females)
total_online <- ifelse(x =='online',TRUE,FALSE)
sum(total_online)
proportion_inclass <- sum(inclass_females)/sum(total_inclass)
proportion_online <- sum(online_females)/sum(total_online)
proportion_inclass #In class 0.667
proportion_online #Online 0.378

'QUESTION 2'
'If you used the type variable to predict sex, what would the prediction accuracy be?'
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- dat[-test_index, ]
test_set <- dat[test_index, ]
y_hat <- ifelse(x == "inclass", "Female", "Male")
mean(y_hat == y) #0.633

'QUESTION 3'
'Write a line of code using the table function to show the confusion matrix, assuming the prediction is y_hat and the truth is y.'
table(predicted = y_hat, actual = y)

#actual
#predicted Female Male
#Female     26   13
#Male       42   69

'QUESTION 4'
'What is the sensitivity of this prediction?'
'Sensitivity = TP/(TP + FN)'
Sensitivity = 26/(26+42)
Sensitivity #0.382

'QUESTION 5'
'What is the specificity of this prediction?'
'Specificity = TN/(TN + FP)'
Specificity = 69/(69 + 13)
Specificity #0.841

'QUESTION6'
'What is the prevalence (% of females) in the dat dataset defined above?'
dat %>% count(sex)
68/(68+82) #0.453