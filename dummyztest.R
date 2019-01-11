

x<-c(50, 95, 120, 85, 45, 90, 70, 60, 70, 50, 40, 80, 70,
  90, 75, 60, 90, 90, 75, 85, 80, 60, 110, 65, 80, 85, 85, 45,
  60, 95, 110, 70, 75, 55, 80, 55)
test_type<-'left-tailed'
alpha<-0.1
pop_mean <-80
pop_sd<-19.2
z_value <- (mean(x)-pop_mean)/(pop_sd/sqrt(length(x)))
print(z_value)
if(test_type=="left-tailed")
{
  z_alpha <- qnorm(alpha)
  print(z_alpha)
  if(z_value < z_alpha)
  {
    print("reject")
  }
  else
  {
    print("not-reject")
  }
}else if (test_type=="right-tailed"){
  z_alpha <-qnorm(1-alpha)
  if(z_value > z_alpha)
  {
    print("reject")
  }
  else
  {
    print("not-reject")
  }
}else if( test_type=="two-tailed")
  {z_half_alpha <- qnorm(alpha/2)
  critical_values <- c(-z_half_alpha, z_half_alpha)
  if((z_value < -z_half_alpha) ||(z_value > z_half_alpha))
  {
    print("reject")
  }
  else
  {
    print("not-reject")
  }
}else
{
  print("Not vald")
}