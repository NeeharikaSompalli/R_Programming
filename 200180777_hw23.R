require(data.table)
require(lm.beta)
require(biglm)
require(ff)

#Q1a
# return: string(“mpg”, “cylinders”, “displacement”, “horsepower”, “weight”, “acceleration”, “year”, “origin”), 
# represents the covariate providing the best prediction

SLR <- function(path='../data/hw23R-linear.txt'){
  auto <- read.csv(file = path, header = TRUE, sep = ",")
  plot(auto)
  correlations= cor(auto)
  
  #plot for Cylinders vs MPG
  plot(auto$cylinders, auto$mpg , xlab = "Cylinders", ylab = "MPG")
  plcyl= lm(auto$mpg~ auto$cylinders)
  abline(plcyl)
  coefplcyl<- coef(summary(plcyl))[ , "Std. Error"][2]
  
  #plot for Displacement vs MPG
  plot(auto$displacement,auto$mpg,  xlab = "Displacement", ylab = "MPG")
  pldis= lm(auto$mpg~ auto$displacement)
  abline(pldis)
  coefpldis<- coef(summary(pldis))[ , "Std. Error"][2]
  
  #plot for Horsepower vs MPG
  plot( auto$horsepower, auto$mpg,xlab = "Horsepower", ylab = "MPG")
  plhp= lm(auto$mpg~ auto$horsepower)
  abline(plhp)
  coefplhp<- coef(summary(plhp))[ , "Std. Error"][2]
  
  #plot for Weight vs MPG
  plot(auto$weight, auto$mpg , xlab = "Weight", ylab = "MPG")
  plwt= lm(auto$mpg~ auto$weight)
  abline(plwt)
  coefplwt<- coef(summary(plwt))[ ,  "Std. Error"][2]
  
  
  #plot for Acceleration vs MPG
  plot(auto$acceleration,auto$mpg,  xlab = "Acceleration", ylab = "MPG")
  placc= lm(auto$mpg~ auto$acceleration)
  abline(placc)
  coefplacc<- coef(summary(placc))[ , "Std. Error"][2]
  
  #plot for Year vs MPG
  plot( auto$year, auto$mpg,xlab = "Year", ylab = "MPG")
  plyr= lm(auto$mpg~ auto$year)
  abline(plyr)
  coefplyr<- coef(summary(plyr))[ , "Std. Error"][2]
  
  
  #plot for Origin vs MPG
  plot( auto$origin, auto$mpg,xlab = "Origin", ylab = "MPG")
  plorgn= lm(auto$mpg~ auto$origin)
  abline(plorgn)
  coefplorgn<- coef(summary(plorgn))[ , "Std. Error"][2]
  
  
  # all the beta values printed
  print(lm.beta(plcyl))
  print(lm.beta(pldis))
  print(lm.beta(plhp))
  print(lm.beta(plwt))
  print(lm.beta(placc))
  print(lm.beta(plyr))
  print(lm.beta(plorgn))
  
  
  # Check the error for each attribute
  minimum <-min(coefplcyl, coefpldis,coefplhp, coefplwt,coefplacc,coefplyr,coefplorgn)
  
  if(minimum == coefplcyl)
  {return ("Cylinders")
  }else if(minimum == coefpldis)
  { return ("Displacement")
  }else if (minimum == coefplhp)
  { return ("Horse Power")} else if (minimum == coefplwt)
  { return ("Weight")} else if (minimum == coefplacc)
  { return ("Acceleration")}else if (minimum == coefplyr)
  { return ("Year")}else { return ("Origin")}
}

#Q1b 
# return: list of four variables, Intercept, MPGCoeff, CylindersCoeff, DispCoeff, HPCoeff, WeightCoeff, AccCoeff, YearCoeff, OriginCoeff

MLR <- function(path='../data/hw23R-linear.txt'){
  auto <- read.csv(file = path, header = TRUE, sep = ",")
  cv_mlr  <- lm(mpg  ~ cylinders+displacement+horsepower+weight+acceleration+year+origin, data = auto)
  
  #calculate the beta value by MLR
  print(lm.beta(cv_mlr))
  
  summary_mlr<- summary(cv_mlr)
  # fill in the list with the coeffes you compute
  result <- list("Intercept"=coef(summary(cv_mlr))[1,],"CylindersCoeff"=coef(summary(cv_mlr))[2,], "DispCoeff"=coef(summary(cv_mlr))[3,], "HPCoeff"=coef(summary(cv_mlr))[4,], "WeightCoeff"=coef(summary(cv_mlr))[5,], "AccCoeff"=coef(summary(cv_mlr))[6,], "YearCoeff"=coef(summary(cv_mlr))[7,], "OriginCoeff"=coef(summary(cv_mlr))[8,])
  return(result)
}


#Q2
# return: list of four variables, Intercept， Lag1oeff，Lag2Coeff，Lag3Coeff, Lag4Coeff, Lag5Coeff, VolumeCoeff

LogisticRegression <- function(path='../data/hw23R-logistic.txt'){
  logdata <- read.csv(file = path, header = TRUE, sep = ",")
  
  # generate test and train data sets
  train <-(logdata$Year <2005)
  test <- (logdata$Year ==2005)
  
  #calculate logistic regression 
  mylogit  <- glm(Direction  ~ Lag1+Lag2+Lag3+Lag4+Lag5+Today+Volume, data = logdata, family = "binomial")
  test.probs <- predict.glm(mylogit, logdata[!train,], type = "response")
  pred<- ifelse(test.probs > 0.5 , "Up", "Down")
  error<-mean ( pred != logdata$Direction[!train])
  
  #calculate accuracy for the Logistic Regression 
  print("Accuracy:")
  print(1-error)
  # fill in the list with the coeffes you compute
  result <- list("Intercept" =coef(summary(mylogit))[1,] ,"Lag1Coeff" =coef(summary(mylogit))[2,], "Lag2Coeff" =coef(summary(mylogit))[3,], "Lag3Coeff" =coef(summary(mylogit))[4,],"Lag4Coeff" =coef(summary(mylogit))[5,], "Lag5Coeff" =coef(summary(mylogit))[6,],"TodayCoeff"= coef(summary(mylogit))[7,],"VolumeCoeff"= coef(summary(mylogit))[8,] )
  return(result)
}

#Q3
# return: float of training accuracy 

LogisticRegressionImproved <- function(path='../data/hw23R-logistic.txt'){
  logdata <- read.csv(file = path, header = TRUE, sep = ",")
  #divide the dataset into training and testing data set
  train <-(logdata$Year <2005)
  test <- (logdata$Year ==2005)
  
  # Fit with all coefficients
  mylogit  <- glm(Direction  ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data = logdata, family = "binomial", subset= train)
  test.probs <- predict.glm(mylogit, logdata[!train,], type = "response")
  pred<- ifelse(test.probs > 0.5 , "Up", "Down")
  error1 <-mean ( pred != logdata$Direction[!train])
  
  # Refit with Lag1 and Lag 2
  mylogit  <- glm(Direction  ~ Lag1+Lag2, data = logdata, family = "binomial", subset= train)
  test.probs <- predict.glm(mylogit, logdata[!train,], type = "response")
  pred<- ifelse(test.probs > 0.5 , "Up", "Down")
  error2 <-mean ( pred != logdata$Direction[!train])
  
  # Refit with Lag2 and Lag 3
  mylogit  <- glm(Direction  ~ Lag2+Lag3, data = logdata, family = "binomial", subset= train)
  test.probs <- predict.glm(mylogit, logdata[!train,], type = "response")
  pred<- ifelse(test.probs > 0.5 , "Up", "Down")
  error3 <-mean ( pred != logdata$Direction[!train])
  
  # Refit with Lag3 and Lag 4
  mylogit  <- glm(Direction  ~ Lag3+Lag4, data = logdata, family = "binomial", subset= train)
  test.probs <- predict.glm(mylogit, logdata[!train,], type = "response")
  pred<- ifelse(test.probs > 0.5 , "Up", "Down")
  error4 <-mean ( pred != logdata$Direction[!train])
  
  # Refit with Lag4 and Lag 5
  mylogit  <- glm(Direction  ~ Lag4+Lag5, data = logdata, family = "binomial", subset= train)
  test.probs <- predict.glm(mylogit, logdata[!train,], type = "response")
  pred<- ifelse(test.probs > 0.5 , "Up", "Down")
  error5 <-mean ( pred != logdata$Direction[!train])
  
  # Refit with Lag5 and Volume
  mylogit  <- glm(Direction  ~ Lag5+Volume, data = logdata, family = "binomial", subset= train)
  test.probs <- predict.glm(mylogit, logdata[!train,], type = "response")
  pred<- ifelse(test.probs > 0.5 , "Up", "Down")
  error6 <-mean ( pred != logdata$Direction[!train])
  
  # Refit with Lag1,2,3
  mylogit  <- glm(Direction  ~ Lag1+Lag2+Lag3, data = logdata, family = "binomial", subset= train)
  test.probs <- predict.glm(mylogit, logdata[!train,], type = "response")
  pred<- ifelse(test.probs > 0.5 , "Up", "Down")
  error7 <-mean ( pred != logdata$Direction[!train])
  
  # Refit with Lag3,4,5 
  mylogit  <- glm(Direction  ~ Lag3+Lag4+Lag5, data = logdata, family = "binomial", subset= train)
  test.probs <- predict.glm(mylogit, logdata[!train,], type = "response")
  pred<- ifelse(test.probs > 0.5 , "Up", "Down")
  error8 <-mean ( pred != logdata$Direction[!train])
  
  # Refit with Lag3,4,5 
  mylogit  <- glm(Direction  ~ Lag3+Lag4+Lag5, data = logdata, family = "binomial", subset= train)
  test.probs <- predict.glm(mylogit, logdata[!train,], type = "response")
  pred<- ifelse(test.probs > 0.5 , "Up", "Down")
  error9 <-mean ( pred != logdata$Direction[!train])
  
  # Refit with Lag1,2,3,4 
  mylogit  <- glm(Direction  ~ Lag1+Lag2+Lag3+Lag4, data = logdata, family = "binomial", subset= train)
  test.probs <- predict.glm(mylogit, logdata[!train,], type = "response")
  pred<- ifelse(test.probs > 0.5 , "Up", "Down")
  error10 <-mean ( pred != logdata$Direction[!train])
  
  # Refit with Lag2,3,4,5
  mylogit  <- glm(Direction  ~ Lag2+Lag3+Lag4+Lag5, data = logdata, family = "binomial", subset= train)
  test.probs <- predict.glm(mylogit, logdata[!train,], type = "response")
  pred<- ifelse(test.probs > 0.5 , "Up", "Down")
  error11 <-mean ( pred != logdata$Direction[!train])
  
  # Refit with Lag2,3,4,5
  mylogit  <- glm(Direction  ~ Lag2+Lag3+Lag4+Volume, data = logdata, family = "binomial", subset= train)
  test.probs <- predict.glm(mylogit, logdata[!train,], type = "response")
  pred<- ifelse(test.probs > 0.5 , "Up", "Down")
  error12 <-mean ( pred != logdata$Direction[!train])
  
  # Refit with Lag2,3,4,Volume
  mylogit  <- glm(Direction  ~ Lag3+Lag4+Lag5+Volume, data = logdata, family = "binomial", subset= train)
  test.probs <- predict.glm(mylogit, logdata[!train,], type = "response")
  pred<- ifelse(test.probs > 0.5 , "Up", "Down")
  error13 <-mean ( pred != logdata$Direction[!train])
  
  #Minimum error calculation  
  minerror <- min(c(error1, error2, error3, error4, error5, error6, error7, error8, error9,error10, error11, error12, error13))
  return(1-minerror)
}

#Q4
# return: list of two variables, Intercept， xCoeff
BigSLR <- function(path='../data/slr-90m-data.csv'){
  #SLR for bigdata 
  set.seed(123)
  big_data <-fread(path)
  #summary(big_data)
  model  <- biglm(y ~ x, data = big_data)
  
  #result output to be returned
  result <- list("Intercept"=coef(model)[1], "xCoeff"=coef(model)[2])
  
  #4b
  
  #5 samples
  sample1=  big_data[sample(nrow(big_data), replace =F, size =0.01*nrow(big_data))]
  sample2= big_data[sample(nrow(big_data), replace =F, size =0.02*nrow(big_data))]
  sample3= big_data[sample(nrow(big_data), replace =F, size =0.03*nrow(big_data))]
  sample4= big_data[sample(nrow(big_data), replace =F, size =0.04*nrow(big_data))]
  sample5= big_data[sample(nrow(big_data), replace =F, size =0.05*nrow(big_data))]
  
  #regression lines
  reg1= lm(sample1$y ~ sample1$x)
  reg2= lm(sample2$y ~ sample2$x)
  reg3= lm(sample3$y ~ sample3$x)
  reg4= lm(sample4$y ~ sample4$x)
  reg5= lm(sample5$y ~ sample5$x)
  print(reg1)
  print(reg2)
  print(reg3)
  print(reg4)
  print(reg5)
  #generating a plot
  plot.new()
  plot.window(xlim =c(-6, 6), ylim = c(-4, 6))
  axis(1)
  axis(2)
  title(xlab="X")
  title(ylab="Y")
  
  #generating all the regression lines
  abline(reg1, col = "blue")
  abline(reg2, col = "orange")
  abline(reg3, col = "black")
  abline(reg4, col = "red")
  abline(reg5, col = "green")
  
  #legend details
  legend("topright", col = c("blue", "orange", "black", "red","green"), 
         legend =c("1% sample", "2% sample","3% sample","4% sample","5% sample"), lwd =2)
  return(result)
}

#Q5
# return: string ("reject" to reject null hypothesis, "not-reject" to not reject null hypothesis)
ZTest <- function(x, test_type, alpha, pop_mean, pop_sd){
z_value <- (mean(x)-pop_mean)/(pop_sd/sqrt(length(x)))

#left-tailed test, right-tailed test, two-tailed test
if(test_type=="left-tailed")
{
  z_alpha <- qnorm(alpha)
  if(z_value < z_alpha)
  {
    return("reject")
  }
  else
  {
    return("not-reject")
  }
}else if (test_type=="right-tailed"){
  z_alpha <-qnorm(1-alpha)
  if(z_value > z_alpha)
  {
    return("reject")
  }
  else
  {
    return("not-reject")
  }
}else if( test_type=="two-tailed"){
  z_half_alpha <- qnorm(alpha/2)
  critical_values <- c(-z_half_alpha, z_half_alpha)
  if((z_value < -z_half_alpha) ||(z_value > z_half_alpha))
  {
    return("reject")
  }
  else
  {
    return("not-reject")
  }
}else{
  return("Invalid operation")
}

}

#Q6
# populationDistribution: string('uniform','normal')
# sampleSize: integer (~30)
# numberOfSamples: integer (>100)
# return: list of two variables, mean and se

CLT <- function(populationDistribution, sampleSize, numberOfSamples){
  set.seed(123)
  samplelist<-c()
  sample.mean <- numeric(numberOfSamples)
  
  # check for sample size and number of samples
  if(sampleSize >=30 && numberOfSamples >100 )
  {
    if(populationDistribution == 'uniform' || populationDistribution == 'runif' )
    {
      for(i in 1:numberOfSamples)
      {
        sampl=runif(sampleSize)
        samplelist=c(samplelist,mean(sampl))
      }
    }else if( populationDistribution == 'normal'|| populationDistribution == 'rnorm')
    {
      for(i in 1:numberOfSamples)
      {
        sampl=rnorm(sampleSize)
        samplelist=c(samplelist,mean(sampl))
      }
    }else if( populationDistribution == 'poisson'|| populationDistribution == 'rpois')
    {
      for(i in 1:numberOfSamples)
      { sampl=rpois(sampleSize,2)
      samplelist=c(samplelist,mean(sampl))
      }
    }
    else if( populationDistribution == 'binomial'|| populationDistribution == 'rbinom')
    {
      for(i in 1:numberOfSamples)
      { sampl=rbinom(10,sampleSize, 0.5)
      samplelist=c(samplelist,mean(sampl))
      }
    }else if( populationDistribution == 'exponential'|| populationDistribution == 'rexp')
    {
      for(i in 1:numberOfSamples)
      {
        sampl=rexp(sampleSize)
        samplelist=c(samplelist,mean(sampl))
      }
    }else if( populationDistribution == 'geometric'|| populationDistribution == 'rgeom')
    {
      for(i in 1:numberOfSamples)
      {
        sampl=rgeom(sampleSize,0.5)
        samplelist=c(samplelist,mean(sampl))
      }
    }
    else
    {
      for(i in 1:numberOfSamples)
      {
        sampl= do.call(populationDistribution, list(sampleSize))
        samplelist=c(samplelist,mean(sampl))
      }
    }
    
    #Histogram 
    hist(samplelist, main="", xlab = "Sample means", col = "darkred")
    
    # fill in the list with the mean and std you compute
    result <- list("mean"=mean(samplelist), "se"=sd(samplelist)/sqrt(numberOfSamples))
    return(result)
  }else
  {
    print("Enter sample size > 25 and number of samples > 100")
    return()
  }
  }

