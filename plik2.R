## evolutionary 

## particle swarm

minValueInIteration<-numeric(length=0)

psPopulation<-20
psIterations<-40
cParam<-0.73
aParam<-2.05


addToMinVector<-function(vector, value)
{
  vector<-c(vector,value)
  return (vector)
}


ps.selection<-function(history, model)
{
	selectedPoints<-historyPop(history, psPopulation)
	return (selectedPoints)
}

ps.modelUpdate<-function(selectedPoints, model)
{
	for (i in 1:psPopulation) {
		if (selectedPoints$quality[i] < model$localBestMinimum[i]) {
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
		newVelocity<-aParam*(velocity + cParam*(rg*(globalBestPosition - position) + rl*(localBestPosition - position))) 
		points$x[i]<-points$x[i]+newVelocity[[1]]
		points$y[i]<-points$y[i]+newVelocity[[2]]
		points$velocityX[i]<-newVelocity[[1]]
		points$velocityY[i]<-newVelocity[[2]]
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
	points$velocityX[i]<-0.0
	points$velocityY[i]<-0.0
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
  minValueInIteration<<-addToMinVector(minValueInIteration, min(newHistory$quality))
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


bla<-termination(3,2)


library(ggplot2)
library(rgl)
library(akima)

startPoints<-ps.startPoints(10)
objectx<-metaheuristicRun(ps.initialization, startPoints, termination, evaluation)
bla<-termination(3,2)


x <- objectx$x 
y <- objectx$y 
z <- objectx$quality 
temp <- interp(x, y, z)
#rzut na x-y
plot.new() 
image(temp) 
#obraz 3d
persp3d(temp, col="skyblue")
#quality(iter)
print(qplot(seq_along(minValueInIteration), minValueInIteration))
