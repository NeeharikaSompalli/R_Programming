rm(list=ls(all=T))
install.packages('biglm')
install.packages('data.table')
library('biglm')
library('data.table')
source('installPackages.R')
setwd("./code/")
getwd()
path =  '../data/slr-90m-data.csv'
big_data <-fread(path)
summary(big_data)
model  <- biglm(y ~ x, data = big_data)
result <- list("Intercept"=coef(model)[1], "xCoeff"=coef(model)[2])

set.seed(123)
big_data <-fread(path)
summary(big_data)
model  <- biglm(y ~ x, data = big_data)
result <- list("Intercept"=coef(model)[1], "xCoeff"=coef(model)[2])


#4b

sample1=  big_data[sample(nrow(big_data), replace =F, size =0.01*nrow(big_data))]
sample2= big_data[sample(nrow(big_data), replace =F, size =0.02*nrow(big_data))]
sample3= big_data[sample(nrow(big_data), replace =F, size =0.03*nrow(big_data))]
sample4= big_data[sample(nrow(big_data), replace =F, size =0.04*nrow(big_data))]
sample5= big_data[sample(nrow(big_data), replace =F, size =0.05*nrow(big_data))]

reg1= lm(sample1$y ~ sample1$x)
reg2= lm(sample2$y ~ sample2$x)
reg3= lm(sample3$y ~ sample3$x)
reg4= lm(sample4$y ~ sample4$x)
reg5= lm(sample5$y ~ sample5$x)

plot.new()
plot.window(xlim =c(-6, 6), ylim = c(-4, 6))
axis(1)
axis(2)
title(xlab="X")
title(ylab="Y")
abline(reg1, col = "blue")
abline(reg2, col = "orange")
abline(reg3, col = "black")
abline(reg4, col = "red")
abline(reg5, col = "green")
legend("topright", col = c("blue", "orange", "black", "red","green"), 
       legend =c("1% sample", "2% sample","3% sample","4% sample","5% sample"), lwd =2)


# fill in the list with the coeffes you compute
# from the next line, you should infer that model should be the variable name of your model

