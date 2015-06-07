## evolutionary 

## particle swarm

psPopulation<-50
psIterations<-500
cParam<-0.73
aParam<-2.05


ps.selection<-function(history, model)
{
	selectedPoints<-historyPop(history, psPopulation)
	return (selectedPoints)
}

ps.modelUpdate<-function(selectedPoints, model)
{
  for (i in 1:psPopulation) {
  	if (selectedPoints$quality[i]<model$localBestMinimum[i]) {
  		model$localBestMinimum[i]<-selectedPoints$quality[i]
  		model$localBestX[i]<-selectedPoints$x[i]
  		model$localBestY[i]<-selectedPoints$y[i]
  	}
  	if (selectedPoints$quality[i] < model$globalBestMinimum[1]) {
  		model$globalBestMinimum[1]<-selectedPoints$quality[i]
  		model$globalBestX[1]<-selectedPoints$x[i]
  		model$globalBestY[1]<-selectedPoints$y[i]
  	}
  }
  return (model)
}

ps.variation<-function(points, model)
{
	globalBestPosition<-c(model$globalBestX[1], model$globalBestY[1])
	for (i in 1:psPopulation) {
		rg<-runif(1, 0, 1)
		rl<-runif(1, 0, 1)
		velocity<-c(points$velocityX[i], points$velocityY[i])
		position<-c(points$x[i], points$y[i])
		localBestPosition<-c(model$localBestX[i], model$localBestY[i])
		newVelocity<-aParam*(position + cParam*(rg*(globalBestPosition - position) + rl*(localBestPosition - position))) 
		points$x[i]<-points$x[i]+newVelocity[[1]]
		points$y[i]<-points$y[i]+newVelocity[[2]]
		if (points$x[i] < -512) {
			points$x[i]<-(-512)
		}
		if (points$x[i] > 512) {
			points$x[i]<-512
		}
		if (points$y[i] < -512) {
			points$y[i]<-(-512)
		}
		if (points$y[i] > 512) {
			points$y[i]<-512
		}
	}
	return (points)
}


ps.startPoints<-function(number) 
{
  points<-data.frame( x=numeric(psPopulation), y=numeric(psPopulation), velocityX=numeric(psPopulation), velocityY=numeric(psPopulation), quality=numeric(psPopulation))
  for (i in 1:psPopulation) {
	points$x[i]<-runif(1, -512, 512)
	points$y[i]<-runif(1, -512, 512)
	points$velocityX[i]<-runif(1, -512, 512)
	points$velocityY[i]<-runif(1, -512, 512)
  }
  return (points)
}

ps.initialization<-function(startPoints)
{
	return (startPoints)
}

ps.initModel<-function(history)
{
	model<-data.frame(globalBestMinimum=numeric(psPopulation), globalBestX=numeric(psPopulation), globalBestY=numeric(psPopulation), localBestMinimum=numeric(psPopulation), localBestX=numeric(psPopulation), localBestY=numeric(psPopulation))
	for (i in 1:psPopulation) {
		model$localBestMinimum[i]<-history$quality[i]
		model$localBestX[i]<-history$x[i]
		model$localBestY[i]<-history$y[i]
	}
	minIndex<-which.min(model$localBestMinimum)
	model$globalBestMinimum[1]<-model$localBestMinimum[minIndex]
	model$globalBestX[1]<-model$localBestX[minIndex]
	model$globalBestY[1]<-model$localBestY[minIndex]
  return (model)
}



## antlion


## the engine

aggregatedOperator<-function(history, oldModel)
{
  
  selectedPoints<-ps.selection(history, oldModel)
  newModel<-ps.modelUpdate(selectedPoints, oldModel)
  newPoints<-ps.variation(selectedPoints, newModel)
  return (list(newPoints=newPoints,newModel=newModel))
}

metaheuristicRun<-function(initialization, startPoints, termination, evaluation)
{
  history<-ps.initialization(startPoints)
  history<-evaluateList(history, evaluation)
  model<-ps.initModel(history)
  i<-1
  while (!termination(i, psIterations))
  {
    aa<-aggregatedOperator(history, model)
    aa$newPoints<-evaluateList(aa$newPoints, evaluation)
    history<-historyPush(history,aa$newPoints)
    model<-aa$newModel
    i<-i+1
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

  for (i in 1:psPopulation) {
    points$quality[i]<-evaluation(points$x[i], points$y[i])
  }
  return (points) 
}

#commonFunctions


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


########## funtion

library(ggplot2)

startPoints<-ps.startPoints(10)
history<-metaheuristicRun(ps.initialization, startPoints, termination, evaluation)
print(qplot(seq_along(history$x), history$quality))

bla<-termination(3,2)