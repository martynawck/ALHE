## evolutionary 

evo_N <-100
evo_p_cross <- 0.5

select<-function(history, model)
{
   return (historyPop(history, evo_N)) 
}

vector.is.empty <- function(x) 
  return(length(x) ==0 )

modelUpdate<-function(selectedPoints, oldModel)
{
  return (oldModel)
}

variation<-function(selectedPoints, model)
{ 
  newGen<-data.frame(sigma_x=numeric(evo_N), sigma_y=numeric(evo_N), x=numeric(evo_N), y=numeric(evo_N))
  for (i in 1:evo_N)
  {
    a<-runif(1)
    if (a < evo_p_cross) 
    {
      # wybierz k=2 rodziców
      parent_1 <- roulette_sel(selectedPoints)
      parent_2 <- roulette_sel(selectedPoints)
      # krzyżowanie + mutacja
      newPoint <- crossover(parent_1, parent_2)
    } 
    else 
    {
      #tylko mutacja      
      newPoint <- roulette_sel(selectedPoints)
    }
    newGen[i,] <- mutation(newPoint)
  }
  
  return (newGen)
}

roulette_sel<-function(points) 
{
  points <- evaluateList(points, f1)
  maxVal <- sum(points$quality)
  tmp <- sample(0:maxVal, 1)
  i <- 0
  sumQ <- 0
  while ( tmp > sumQ )
  {
    i<-i+1
    sumQ<-sumQ + points$quality[i]
  }
  return (points[i,])
}

crossover<-function(parent1, parent2)
{
  a<-runif(1)
  parent1$x<-a*parent1$x+(1-a)*parent2$x
  parent1$y<-a*parent1$y+(1-a)*parent2$y
  parent1$sigma_x<-a*parent1$sigma_x+(1-a)*parent2$sigma_x
  parent1$sigma_y<-a*parent1$sigma_y+(1-a)*parent2$sigma_y
  return (parent1)
}

mutation<-function(point)
{
  epsilon <- runif(1, -1, 1)
  epsilonX <- runif(1, -1, 1)
  epsilonY <- runif(1, -1, 1)
  vX <- runif(1, -1, 1)
  vY <- runif(1, -1, 1)
  rA <- 1/(1/sqrt(4))
  rB <- 1/(1/sqrt(2*sqrt(2)))
  point$sigma_x <- point$sigma_x*exp(rA*epsilon + rB*epsilonX)
  point$sigma_y <- point$sigma_y*exp(rA*epsilon + rB*epsilonY)
  point$x <- point$x + point$sigma_x*vX
  point$y <- point$y + point$sigma_y*vY
  if (point$x < -512) {
    point$x<-(-512)
  }
  if (point$x > 512) {
    point$x<-512
  }
  if (point$y < -512) {
    point$y<-(-512)
  }
  if (point$y > 512) {
    point$y<-512
  }
  return (point)
}


## particle swarm

## antlion


## the engine

aggregatedOperator<-function(history, oldModel)
{ 
  selectedPoints<-select(history, oldModel)
  newModel<-modelUpdate(selectedPoints, oldModel)
  newPoints<-variation(selectedPoints, newModel)
  return (list(newPoints=newPoints,newModel=newModel))
}

metaheuristicRun<-function(initialization, startPoints, termination, evaluation)
{
  history<-initialization(startPoints)
  history<-evaluateList(history, evaluation)
  model<-initModel(history)
  i<-1
  while (!termination(i,5))
  {
    i<-i+1
    aa<-aggregatedOperator(history, model)
    aa$newPoints<-evaluateList(aa$newPoints, evaluation)
  #  history<-aa$newModel
    history<-historyPush(history,aa$newPoints)
    model<-aa$newModel
  }
  return(history)
}

historyPush<-function(oldHistory, newPoints)
{
  newHistory <- rbind(oldHistory, newPoints)
  return (newHistory)
}

historyPop<-function(history, number)
{
  stop=nrows(history)
  start=max(stop-number+1,1)
  return(history[start:stop,])
}

evaluateList<-function(points,evaluation)
{
  for (i in 1:length(points$x)) {
    points$quality[[i]]<-evaluation(points$x[[i]], points$y[[i]])
  }
  return (points) 
}

#commonFunctions

generateStartPoints<-function(mi)
{
  startPoints<-data.frame(sigma_x=numeric(mi), sigma_y=numeric(mi), x=numeric(mi), y=numeric(mi))
  for (i in 1:mi)
  {
    startPoints$x[i]<-runif(1, -512, 512)
    startPoints$y[i]<-runif(1, -512, 512)
    startPoints$sigma_x[i]<-0.2
    startPoints$sigma_y[i]<-0.2
  }
  return (startPoints)
}

termination<-function(i, n)
{
  if (i <= n) { 
    return (F) 
  } 
  else { 
    return (T) 
  }
}

f1<-function(x, y)
{
  return(x^2 + y^2)
}

initialization<-function(points)
{
  historyPoints<-data.frame(points)
  return (historyPoints)
}

initModel<-function(points)
{
  historyPoints<-data.frame(points)
  return (historyPoints)
}


########## main function

library(ggplot2)
library(rgl)
library(akima)

startPoints<-generateStartPoints(evo_N)

objectx<-metaheuristicRun(initialization, startPoints, termination, f1)
#surface3d(x = objectx$x, y= objectx$y, z=objectx$quality )
#qplot(seq_along(objectx$y), objectx$x, data=objectx)
bla<-termination(3,2)

x <- objectx$x 
y <- objectx$y 
z <- objectx$quality 
temp <- interp(x, y, z)
persp3d(temp, col="skyblue")
plot.new() 
image(temp) 
