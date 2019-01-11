numberOfSamples=4000
sampleSize=26
populationDistribution= 'exponential'
set.seed(123)
samplelist<-c()
sample.mean <- numeric(numberOfSamples)
if(sampleSize >25 && numberOfSamples >100 )
{
  if(populationDistribution == 'uniform')
  {
    for(i in 1:numberOfSamples)
    {
      sampl=runif(sampleSize)
      samplelist=c(samplelist,mean(sampl))
    }
  }else if( populationDistribution == 'normal')
  {
    for(i in 1:numberOfSamples)
    {
      sampl=rnorm(sampleSize)
     samplelist=c(samplelist,mean(sampl))
    }
  }else if( populationDistribution == 'poisson')
  {
    for(i in 1:numberOfSamples)
    { sampl=rpois(sampleSize)
      samplelist=c(samplelist,mean(sampl))
    }
  }
  else if( populationDistribution == 'binomial')
  {
    for(i in 1:numberOfSamples)
    { sampl=rbinom(sampleSize)
      samplelist=c(samplelist,mean(sampl))
    }
  }else if( populationDistribution == 'exponential')
  {
    for(i in 1:numberOfSamples)
    {
      sampl=rexp(sampleSize)
      samplelist=c(samplelist,mean(sampl))
    }
  }else if( populationDistribution == 'geometric')
  {
    for(i in 1:numberOfSamples)
    {
      sampl=rgeom(sampleSize)
      samplelist=c(samplelist,mean(sampl))
    }
  }
  else
  {
    for(i in 1:numberOfSamples)
  {
    do.call(populationDistribution, sampleSize)
  }
  }
  results <- c()
  for ( i in 1:numberOfSamples)
  {
    results<-c(results, mean(samplelist[[i]]))
  }
  hist(samplelist, main="", xlab = "Sample means", col = "darkred")
  # fill in the list with the mean and std you compute
  result <- list("mean"=mean(samplelist), "se"=sd(samplelist)/sqrt(numberOfSamples))
  return(result)
}else
{
  print("Enter sample size > 25 and number of samples > 100")
  return()
}
