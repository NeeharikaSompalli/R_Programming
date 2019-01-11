big_data <-fread(path)
model  <- biglm(y ~ x, data = big_data)
summary(big_data)
#plot(1, type = "n", xlab ="X", ylab = "Y", xlim =c(-6, 6), ylim = c(-4, 6))
plot.new()
plot.window(xlim =c(-6, 6), ylim = c(-4, 6))
axis(1)
axis(2)
title(xlab="X")
title(ylab="Y")

sample1=as.data.frame(sapply(big_data[], sample, as.integer(dim(big_data)*0.01)))
reg1= lm(sample1$y ~ sample1$x)

sample2=as.data.frame(sapply(big_data[], sample, as.integer(dim(big_data)*0.02)))
reg2= lm(sample2$y ~ sample2$x)

sample3=as.data.frame(sapply(big_data[], sample, as.integer(dim(big_data)*0.03)))
reg3= lm(sample3$y ~ sample3$x)

sample4=as.data.frame(sapply(big_data[], sample, as.integer(dim(big_data)*0.04)))
reg4= lm(sample4$y ~ sample4$x)

sample5=as.data.frame(sapply(big_data[], sample, as.integer(dim(big_data)*0.05)))
reg5= lm(sample5$y ~ sample5$x)

abline(reg1, col = "blue")
abline(reg2, col = "orange")
abline(reg3, col = "black")
abline(reg4, col = "red")
abline(reg5, col = "green")

# fill in the list with the coeffes you compute
# from the next line, you should infer that model should be the variable name of your model
result <- list("Intercept"=coef(model)[1], "xCoeff"=coef(model)[2])
return(result)