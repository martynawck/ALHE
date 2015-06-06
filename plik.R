## evolutionary 

## particle swarm

## antlion


## the engine

aggregatedOperator<-function(history, oldModel)
{
  
  selectedPoints<-selection(history, oldModel)
  newModel<-modelUpdate(selectedPoints, oldModel)
  newPoints<-variation(selectedPoints, newModel)
  return (list(newPoints=newPoints,newModel=newModel))
}

metaheuristicRun<-function(initialization, startPoints, termination, evaluation)
{
  history<-initialization(startPoints)
  print(history)
 # i<-1
#  n<-5
 # while (!termination(i,n)) { 
#    print(i)
#    i<-i+1
#  }
  history<-evaluateList(history, evaluation)
 # model<-initModel(history)
  #while (!termination(history,model))
  #{
  #  aa<-aggregatedOperator(history, model)
  #  aa$newPoints<-evaluateList(aa$newPoints, evaluation)
  #  history<-historyPush(history,aa$newPoints)
  #  model<-aa$newModel
  #}
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
  points<-data.frame(x = numeric(mi),y = numeric(mi))
  for (i in 1:mi)
  {
    points$x[i]<-runif(1, -10, 10)
    points$y[i]<-runif(1, -10, 10)
   # startPoints$sigma_x[i]<-0.2
  #  startPoints$sigma_y[i]<-0.2
  #  startPoints$minimum[i]<-1000.0
  #  startPoints$best_minimum[i]<-1000.0
  }
  return (points)
  
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

initialization<-function(points){
  historyPoints<-data.frame(points)
  return (historyPoints)
}

########## funtion

library(ggplot2)

startPoints<-generateStartPoints(mi)


objectx<-metaheuristicRun(initialization, startPoints, termination, evaluation)
print(qplot(startPoints$x, startPoints$y))
#bla<-termination(3,2)
