
bestAntlion<-NULL

addToMinVector<-function(vector, value)
{
  vector<-c(vector,value)
  return (vector)
}

select<-function(history, model)
{
  return (historyPop(history, antNumber))
}

modelUpdate<-function(selectedPoints, oldModel)
{
  selectedPoints <- evaluateList(selectedPoints, f1)
  for (i in 1:antNumber)
  {
    antlionIdx <- selectedPoints$antlionIdx[i]
    if (antlionIdx != 0 && selectedPoints$quality[i] < oldModel$quality[antlionIdx])  # jeśli mrówka jest lepsza od mrówkolwa
    {
      oldModel[i,] <- selectedPoints[i,]      #mrówkolew ją zjada i przechodzi na jej miejsce
    }
  }
  return (evaluateList(oldModel, f1))
}

variation<-function(selectedPoints, model)
{ 
  for (i in 1:antNumber)  #dla każdej mrówki
  {
    # policz random walk dookoła losowego antliona i dookoła elitarnego antliona
    antlionIdx <- roulette_sel(model)
    selectedPoints$antlionIdx[i] <- antlionIdx
    ra <- random_walk(model[antlionIdx,])
    re <- random_walk(findEliteAntlion(model))
    selectedPoints$x[i] <- (ra$x + re$x)/2
    selectedPoints$y[i] <- (ra$y + re$y)/2
    
    #spr czy mrówka nie polazła za daleko
    if (selectedPoints$x[i] > ub) 
    {
      selectedPoints$x[i] <- ub  
    }
    else if (selectedPoints$x[i] < lb) 
    {
      selectedPoints$x[i] <- lb  
    }
    if (selectedPoints$y[i] > ub) 
    {
      selectedPoints$y[i] <- ub  
    }
    else if (selectedPoints$y[i] < lb) 
    {
      selectedPoints$y[i] <- lb  
    }
  }
  return (selectedPoints)
}

random_walk<-function(antlion) 
{
  I <- getCurrentRatio()
  localUb <- c(ub / I, ub / I)  # równiania 2.10, 2.11
  localLb <- c(lb / I, lb / I)
  
  localUb <- update_boundary_with_antlion(localUb, antlion) # równania 2.8, 2.9
  localLb <- update_boundary_with_antlion(localLb, antlion)
  
  rw <- calculate_random_walk(localUb, localLb)
  
  return (rw[current_iteration,])
}

calculate_random_walk<-function(localUb, localLb) 
{
  x <- calculate_random_walk_one_dimention(localUb[1], localLb[1])
  y <- calculate_random_walk_one_dimention(localUb[2], localLb[2])
  return (data.frame(x, y))
}

calculate_random_walk_one_dimention<-function(l_ub, l_lb) 
{
  X <- c(0, cumsum(2*((runif(max_iteration) > 0.5) + 0) - 1) )
  a <- min(X)
  b <- max(X)
  c <- l_lb
  d <- l_ub
  return ((X-a)*(d-c))/(b-a) + c
}

getCurrentRatio<-function() 
{
  if ( current_iteration > max_iteration*0.95 ) {
    return (1+1000000*(current_iteration/max_iteration))
  }
  if ( current_iteration > max_iteration*0.9 ) {
    return (1+100000*(current_iteration/max_iteration))
  }
  if ( current_iteration > max_iteration*(3/4) ) {
    return (1+10000*(current_iteration/max_iteration))
  }
  if ( current_iteration > max_iteration/2 ) {
    return (1+1000*(current_iteration/max_iteration))
  }
  if ( current_iteration > max_iteration/10 ) {
   return (1+100*(current_iteration/max_iteration))
  }
  return (1)
}

update_boundary_with_antlion<-function(b, antlion)
{
  a <- runif(1)
  tmp <- 1
  if (a < 0.5) 
  {
    tmp <- (-1)
  }
  b[1] = tmp*b[1] + antlion$x
  b[2] = tmp*b[2] + antlion$y
  return (b)
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
  return (i)
}

roulette_sel<-function(points) 
{
  points <- evaluateList(points, f1)
  smQuality <- softMax(points$quality)
  tmp <- runif(1)
  i <- 0
  sumQ <- 0
  while ( sumQ <= tmp )
  {
    i<-i+1
    sumQ<-sumQ + smQuality[i]
  }
  return (i)
}

softMax<-function(pointsQuality)
{
  pointsQuality <- scale((-1)*pointsQuality)
  result <- c()
  S <- 0
  for (i in 1:length(pointsQuality)) 
  {
    S <- S + exp(pointsQuality[i])
  }
  for (i in 1:length(pointsQuality)) 
  {
    x <- exp(pointsQuality[i])/S
    result <- c(result, x)
  }
  return (result)
}



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
  while (!termination(current_iteration, max_iteration))
  {
    current_iteration <- current_iteration + 1
    aa<-aggregatedOperator(history, model)
    aa$newPoints<-evaluateList(aa$newPoints, evaluation)
    history<-historyPush(history,aa$newPoints)
    model<-aa$newModel 
  }
  # porównanie mrówek i mrówkolwów po ostatniej iteracji
  model<-modelUpdate(select(history, model), model)
  bestAntlion<<-findEliteAntlion(model) # uzupełnienie zmiennej globalnej
  return(history)
}

historyPush<-function(oldHistory, newPoints)
{
  newHistory <- rbind(oldHistory, newPoints)
  minValueInIteration<<-addToMinVector(minValueInIteration, min(oldHistory$quality))
  return (newHistory)
}

historyPop<-function(history, number)
{
  h_stop=nrow(history)
  h_start=max(h_stop-number+1,1)
  return(history[h_start:h_stop,])
}

evaluateList<-function(points,evaluation)
{
  for (i in 1:length(points$x)) {
    points$quality[i]<-evaluation(points$x[i], points$y[i])
  }
  return (points) 
}

#commonFunctions

generateStartPoints<-function(mi)
{
  startPoints<-data.frame(x=numeric(mi), y=numeric(mi), antlionIdx=0)
  for (i in 1:mi)
  {
    startPoints$x[i]<-runif(1, lb, ub)
    startPoints$y[i]<-runif(1, lb, ub)
  }
  return (startPoints)
}

termination<-function(i, n)
{
  print(i)
  if (i <= n) { 
    return (F) 
  } 
  else { 
    return (T) 
  }
}

initialization<-function(points)
{
  historyPoints<-data.frame(points)
  return (historyPoints)
}

initModel<-function(history)
{
  antlionPositions<-data.frame(x=numeric(antlion_N), y=numeric(antlion_N))
  for (i in 1:antlion_N)
  {
    antlionPositions$x[i]<-runif(1, lb, ub)
    antlionPositions$y[i]<-runif(1, lb, ub)
  }
  return (evaluateList(antlionPositions, f1))
}

findEliteAntlion<-function(model)
{
  idx <- which.min(model$quality)
  return (model[idx,])
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

########## main function


minValueInIteration<-numeric(length=0)

#parametry
antlion_N <- 10            # liczba mrówkolwów
antNumber <- antlion_N     # liczba mrówek
lb <- -10                  # ograniczenie dolne na x, y
ub <- 10                   # ograniczenie górne na x, y
max_iteration <- 10        # liczba iteracji
current_iteration <- 1     # obecna iteracja
func <- f2                 # funkcja wykorzystywana do ewaluacji: f1/f2/f3/f4

library(ggplot2)
library(rgl)
library(akima)

startPoints<-generateStartPoints(antlion_N)

objectx<-metaheuristicRun(initialization, startPoints, termination, func)
#print(qplot(seq_along(objectx$x), objectx$quality))

x <- objectx$x 
y <- objectx$y 
z <- objectx$quality 
temp <- interp(x, y, z, duplicate="strip")
#rzut na x-y
plot.new() 
image(temp) 
#obraz 3d
persp3d(temp, col="skyblue")
#quality(iter)
print(qplot(seq_along(minValueInIteration), minValueInIteration))

print(bestAntlion)
