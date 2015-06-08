## particle swarm


selection<-function(history, model) {
	selectedPoints<-historyPop(history, psPopulation)
	return (selectedPoints)
}

modelUpdate<-function(selectedPoints, model) {
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
	minValueInIteration<<-addToMinVector(minValueInIteration, min(model$globalBestMinimum[1])) 
	return (model)
}

variation<-function(points, model) {
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
		if (points$x[i] < lb) {
			points$x[i]<-lb
		}
		if (points$x[i] > ub) {
			points$x[i]<-ub
		}
		if (points$y[i] < lb) {
			points$y[i]<-lb
		}
		if (points$y[i] > ub) {
			points$y[i]<-ub
		}
	}
	return (points)
}


startPoints<-function(number) {
  points<-data.frame( x=numeric(psPopulation), y=numeric(psPopulation), velocityX=numeric(psPopulation), velocityY=numeric(psPopulation), quality=numeric(psPopulation))
  for (i in 1:psPopulation) {
	points$x[i]<-runif(1, -10, 10)
	points$y[i]<-runif(1, -10, 10)
	points$velocityX[i]<-0.0
	points$velocityY[i]<-0.0
  }
  return (points)
}


initModel<-function(history) {
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


## the engine

aggregatedOperator<-function(history, oldModel) {
	selectedPoints<-selection(history, oldModel)
	newModel<-modelUpdate(selectedPoints, oldModel)
	newPoints<-variation(selectedPoints, newModel)
	return (list(newPoints=newPoints,newModel=newModel))
}

metaheuristicRun<-function(initialization, startPoints, termination, evaluation) {
	history<-initialization(startPoints)
	history<-evaluateList(history, evaluation)
	model<-initModel(history)
	i<-1
	while (!termination(i, psIterations)) {
		aa<-aggregatedOperator(history, model)
		aa$newPoints<-evaluateList(aa$newPoints, evaluation)
		history<-historyPush(history,aa$newPoints)
		model<-aa$newModel
		i<-i+1
	}
	return(history)
}

historyPush<-function(oldHistory, newPoints) {
	newHistory<-rbind(oldHistory,newPoints)
	return (newHistory)
}

historyPop<-function(history, number) {
	stop=nrow(history)
	start=max(stop-number+1,1)
	return(history[start:stop,])
}


evaluateList<-function(points,evaluation){
	for (i in 1:psPopulation) {
		points$quality[i]<-evaluation(points$x[i], points$y[i])
	}
	return (points) 
}

termination<-function(i, n) {
	if (i <= n) { 
		return (F) 
	} 
	else { 
		return (T) 
	}
}

initialization<-function(points)
{
  return (points)
}

addToMinVector<-function(vector, value) {
	vector<-c(vector,value)
	return (vector)
}

# prosta funkcja
f1<-function(x, y)
{
  return(x^2 + y^2)
}

# funkcja Ackleya
f2<-function(x, y)
{
  return (-20*exp(-0.2*sqrt(0.5*(x^2 + y^2))) - exp(0.5*(cos(2*pi*x) + cos(2*pi*y))) + exp(1) + 20)
}

#funkcja Beale'a
f3<-function(x, y)
{
  return ( (1.5-x+x*y)^2 + (2.25 - x + x*y^2)^2 + (2.625 - x + x*y^3)^2 ) 
}

# funkcja Bukina F6
f4<-function(x, y)
{
  return ( 100*sqrt(abs(y-0.01*x^2)) + 0.01*abs(x+10) ) 
}


##main function

minValueInIteration<-numeric(length=0)

#parametry
psPopulation<- 10     # rozmiar populacji
psIterations<- 10     # liczba iteracji
cParam<- 2.05         # parametr równania
aParam<- 0.73         # parametr równania
lb<- (-10)            # ograniczenie dolne na x, y
ub<- 10               # ograniczenie górne na x, y
func <- f2            # funkcja wykorzystywana do ewaluacji: f1/f2/f3/f4

library(ggplot2)
library(rgl)
library(akima)

startPoints<-startPoints()
objectx<-metaheuristicRun(initialization, startPoints, termination, func)

x <- objectx$x 
y <- objectx$y 
z <- objectx$quality 
temp <- interp(x, y, z, duplicate="strip")
#rzut na x-y
plot.new() 
image(temp) 
#obraz 3d
persp3d(temp, col="skyblue")
print(qplot(seq_along(minValueInIteration), minValueInIteration))
 