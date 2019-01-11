rm(list=ls(all=T))
install.packages('biglm')
install.packages('data.table')
library('biglm')
library('data.table')
source('installPackages.R')
setwd("./code/")
getwd()
path =  '../data/hw23R-logistic.txt'
logdata <- read.csv(file = path, header = TRUE, sep = ",")

# generate test and train data sets
#train <-(logdata$Year <2005)
#test <- (logdata$Year ==2005)

#calculate logistic regression 
mylogit  <- glm(Direction  ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = logdata, family = "binomial")
#test.probs <- predict.glm(mylogit, logdata[!train,], type = "response")
#pred<- ifelse(test.probs > 0.5 , "Up", "Down")
#error<-mean ( pred != logdata$Direction[!train])

#calculate accuracy for the Logistic Regression 
#print("Accuracy:")
#print(1-error)
# fill in the list with the coeffes you compute
result <- list("Intercept" =coef(summary(mylogit))[1,] ,"Lag1Coeff" =coef(summary(mylogit))[2,], "Lag2Coeff" =coef(summary(mylogit))[3,], "Lag3Coeff" =coef(summary(mylogit))[4,],"Lag4Coeff" =coef(summary(mylogit))[5,], "Lag5Coeff" =coef(summary(mylogit))[6,],"VolumeCoeff"= coef(summary(mylogit))[7,] )
return(result)