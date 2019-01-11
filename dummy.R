rm(list=ls(all=T))
install.packages('lm.beta')
library('lm.beta')
source('installPackages.R')
setwd("./code/")
getwd()
df = data.frame(x = numeric(), y = character(20))
path_to_data = '../data/hw23R-linear.txt'
auto<-read.csv(file = path_to_data, header =TRUE, sep = ",")
plot(auto)
correlations= cor(auto)

plot(auto$cylinders, auto$mpg , xlab = "Cylinders", ylab = "MPG")
plcyl= lm(auto$mpg~ auto$cylinders)
abline(plcyl)
coefplcyl<- coef(summary(plcyl))[ , "Std. Error"][2]


plot(auto$displacement,auto$mpg,  xlab = "Displacement", ylab = "MPG")
pldis= lm(auto$mpg~ auto$displacement)
abline(pldis)
coefpldis<- coef(summary(pldis))[ , "Std. Error"][2]


plot( auto$horsepower, auto$mpg,xlab = "Horsepower", ylab = "MPG")
plhp= lm(auto$mpg~ auto$horsepower)
abline(plhp)
coefplhp<- coef(summary(plhp))[ , "Std. Error"][2]


plot(auto$weight, auto$mpg , xlab = "Weight", ylab = "MPG")
plwt= lm(auto$mpg~ auto$weight)
abline(plwt)
coefplwt<- coef(summary(plwt))[ ,  "Std. Error"][2]

plot(auto$acceleration,auto$mpg,  xlab = "Acceleration", ylab = "MPG")
placc= lm(auto$mpg~ auto$acceleration)
abline(placc)
coefplacc<- coef(summary(placc))[ , "Std. Error"][2]


plot( auto$year, auto$mpg,xlab = "Year", ylab = "MPG")
plyr= lm(auto$mpg~ auto$year)
abline(plyr)
coefplyr<- coef(summary(plyr))[ , "Std. Error"][2]

plot( auto$origin, auto$mpg,xlab = "Origin", ylab = "MPG")
plorgn= lm(auto$mpg~ auto$origin)
abline(plorgn)
coefplorgn<- coef(summary(plorgn))[ , "Std. Error"][2]

print(lm.beta(plcyl))
print(lm.beta(pldis))
print(lm.beta(plhp))
print(lm.beta(plwt))
print(lm.beta(placc))
print(lm.beta(plyr))
print(lm.beta(plorgn))


minimum <-min(coefplcyl, coefpldis,coefplhp, coefplwt,coefplacc,coefplyr,coefplorgn)
if(minimum == coefplcyl)
{return ("Cylinders")
  }else if(minimum == coefpldis)
          { return ("Displacement")
  }else if (minimum == coefplhp)
  { return ("Horse Power")} else if (minimum == coefplwt)
  { return ("Horse Power")} else if (minimum == coefplacc)
  { return ("Horse Power")}else if (minimum == coefplyr)
            { return ("Horse Power")}else { return ("Origin")}


