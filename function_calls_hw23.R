#cleanup before start
rm(list=ls(all=T))

# path to the code folder
setwd(".")

# add all the code files and install packages
source('installPackages.R')
source('200180777_hw23.R')

# path to the datasets
path_to_data = '../data'

#Qn1 Simple linear regression
SLR(paste(path_to_data, '/hw23R-linear.txt', sep= ""))

#Qn1B MLR
MLR(paste(path_to_data, '/hw23R-linear.txt', sep= ""))

#Qn2 LOgistic Regression
LogisticRegression(paste(path_to_data, '/hw23R-logistic.txt', sep= ""))

# Qn3 Improved Logistic Regression
LogisticRegressionImproved(paste(path_to_data, '/hw23R-logistic.txt', sep= ""))

#Qn4 BigSLR
BigSLR(paste(path_to_data, '/slr-90m-data.csv', sep= ""))

#Qn5 Z test. Chnage the input values. The example given in the Question is stated here
ZTest(c(50, 95, 120, 85, 45, 90, 70, 60, 70, 50, 40, 80, 70,
          90, 75, 60, 90, 90, 75, 85, 80, 60, 110, 65, 80, 85, 85, 45,
          60, 95, 110, 70, 75, 55, 80, 55),
      'left-tailed',0.1, 80,19.2)

# CLT Give the input to the population distribution, sample size and number of samples
CLT(populationDistribution,sampleSize,numberOfSamples)