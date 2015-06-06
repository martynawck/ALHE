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

metaheuristicRun<-function(initialization)#, startPoints, termination, evaluation)
{
  history<-initialization(startPoints)
#  history<-evaluateList(history)
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
  for (i in 1:length(points))
    points[[i]]$quality<-evaluation(points[[i]]$coordinates)
  return (points) 
}

mi<-10



ga.startPoints<-function(mi)
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

ga.initialization<-function(startPoints)
{
  return (startPoints)
}

startPoints <- ga.startPoints(mi)

objectx<-metaheuristicRun(initialization)

