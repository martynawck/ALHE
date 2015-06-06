## evolutionary 
select<-function(history, model)
{
  a<-runif(1)
  
  if (a < prawdopodobienstwo) {
    usedValues <- vector(mode="numeric", length=0)
    sigma_x<-vector(mode="double", length=0)
    sigma_y<-vector(mode="double", length=0)
    x<-vector(mode="double", length=0)
    y<-vector(mode="double", length=0)
    k <- sample(2:length(history$x), 1)
    
    for (i in 1:k) {
          if (vector.is.empty(usedValues)) {
            j <- sample(1:length(history$x), 1)
            
            usedValues<-c(usedValues, j)
            usedValues<-c(usedValues, j)
            sigma_x<-c(sigma_x, history$sigma_x[j])
            sigma_y<-c(sigma_y, history$sigma_y[j])
            x<-c(x, history$x[j])
            y<-c(y, history$y[j])
            
          } else {
            
            while (is.element(j,usedValues) ) {
              j <- sample(1:length(history$x), 1)
            }
            
            usedValues<-c(usedValues, j)
            sigma_x<-c(sigma_x, history$sigma_x[j])
            sigma_y<-c(sigma_y, history$sigma_y[j])
            x<-c(x, history$x[j])
            y<-c(y, history$y[j])
            
          }
    }
    
    temp<-data.frame(sigma_x=sigma_x, sigma_y=sigma_y, x=x, y=y)
    
  } else {
    k <- sample(2:length(history$x), 1)
    temp<-data.frame(sigma_x=history$sigma_x[k], sigma_y=history$sigma_y[k], x=history$x[k], y=history$y[k], 
                     minimum=history$minimum[k], best_x=history$x[1], best_y=history$y[1], best_minimum=history$minimum[1])
  }
  return (temp)
}

vector.is.empty <- function(x) 
  return(length(x) ==0 )

modelUpdate<-function(selectedPoints, oldModel)
{
  return (oldModel)
}

variation<-function(selectedPoints, model)
{ 
  
  return (selectedPoints)
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
  print(history)
  history<-evaluateList(history, evaluation)
  model<-initModel(history)
  i<-1
  while (!termination(i,1))
  {
    i<-i+1
    aa<-aggregatedOperator(history, model)
    aa$newPoints<-evaluateList(aa$newPoints, evaluation)
    history<-historyPush(history,aa$newPoints)
    model<-aa$newModel
  }
  return(history)
}

historyPush<-function(oldHistory, newPoints)
{
  newHistory<-c(oldHistory,newPoints)
  return (newHistory)
}

historyPop<-function(history, number)
{
  stop=length(history)
  start=max(stop-number+1,1)
  return(history[start:stop])
}

evaluateList<-function(points,evaluation)
{
  for (i in 1:length(points$x)) {
    points$quality[[i]]<-evaluation(points$x[[i]], points$y[[i]])
  }
  return (points) 
}

#commonFunctions

mi<-10

generateStartPoints<-function(mi)
{
  startPoints<-data.frame(sigma_x=numeric(mi), sigma_y=numeric(mi), x=numeric(mi), y=numeric(mi), minimum=numeric(mi))
  for (i in 1:mi)
  {
    startPoints$x[i]<-runif(1, -512, 512)
    startPoints$y[i]<-runif(1, -512, 512)
    startPoints$sigma_x[i]<-0.2
    startPoints$sigma_y[i]<-0.2
    startPoints$minimum[i]<-1000.0
    startPoints$best_minimum[i]<-1000.0
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

evaluation<-function(x, y)
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

selection<-function(point, history)
{
  len<-max(i, 1)
  if(len == 1 || point$minimum<history$best_minimum[len]) 
  {
    point$best_x<-point$x
    point$best_y<-point$y
    point$best_minimum<-point$minimum
  } else {
    point$best_x<-history$best_x[len]
    point$best_y<-history$best_y[len]
    point$best_minimum<-history$best_minimum[len]
  }
  return(point) 
}

########## funtion

library(ggplot2)

startPoints<-generateStartPoints(mi)
prawdopodobienstwo<-0.5

objectx<-metaheuristicRun(initialization, startPoints, termination, evaluation)
print(qplot(objectx$x, objectx$y))
#bla<-termination(3,2)
