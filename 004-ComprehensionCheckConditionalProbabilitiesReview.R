'Comprehension Check: Conditional Probabilities Review'
'Q1'
'In a previous module, we covered Bayes theorem and the Bayesian paradigm. Conditional probabilities are a fundamental part of this previous covered rule.
We first review a simple example to go over conditional probabilities.
Assume a patient comes into the doctor’s office to test whether they have a particular disease.
The test is positive 85% of the time when tested on a patient with the disease (high sensitivity): 
  The test is negative 90% of the time when tested on a healthy patient (high specificity): 
  The disease is prevalent in about 2% of the community: 
  Using Bayes theorem, calculate the probability that you have the disease if the test is positive.'

'P(disease|test+) = P(test+|disease)x ( P(disease)/P(test+) )
= (P(test+|disease)P(disease))/ (P(test+|disease)P(disease) | P(test+|healthy)P(healthy))
= (0.85x0.02)/(0.85x0.02+0.1x0.98) = 0.1478261'

#0.148

'The following 4 questions (Q2-Q5) all relate to implementing this calculation using R.
We have a hypothetical population of 1 million individuals with the following conditional probabilities as described below:
The test is positive 85% of the time when tested on a patient with the disease (high sensitivity): 
The test is negative 90% of the time when tested on a healthy patient (high specificity): 
The disease is prevalent in about 2% of the community: 
Here is some sample code to get you started:'

set.seed(1)
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))

'Q2'
'What is the probability that a test is positive?'
table(disease, test)
'test
disease      0      1
0 882426  97656
1   3065  16853'

#probability = (TP + FP)/total = (16853+97656)/1000000
mean(test) #0.114509

'Q3'
'What is the probability that an individual has the disease if the test is negative?'
3065/1000000 #0.003065
mean(disease[test==0])

'Q4'
'What is the probability that you have the disease if the test is positive?'
mean(disease[test==1]==1) #0.1471762

'Q5'
'If the test is positive, what is the relative risk of having the disease?'
mean(disease[test==1]==1)/2*100 #7.35881
