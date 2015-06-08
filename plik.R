## evolutionary 

addToMinVector<-function(vector, value)
{
  vector<-c(vector,value)
  return (vector)
}

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
  smQuality <- softMax(points$quality)
  tmp <- runif(1)
  i <- 0
  sumQ <- 0
  while ( sumQ <= tmp )
  {
    i<-i+1
    sumQ<-sumQ + smQuality[i]
  }
  return (points[i,])
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
  if (point$x < evo_lb) {
    point$x<-(evo_lb)
  }
  if (point$x > evo_ub) {
    point$x<-evo_ub
  }
  if (point$y < evo_lb) {
    point$y<-(evo_lb)
  }
  if (point$y > evo_ub) {
    point$y<-evo_ub
  }
  return (point)
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
  while (!termination(i,evo_max_iter))
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
  newHistory <- rbind(oldHistory, newPoints)
  minValueInIteration<<-addToMinVector(minValueInIteration, min(newHistory[,5]))
  return (newHistory)
}

historyPop<-function(history, number)
{
 # stop=nrow(history)
  #start=max(stop-number+1,1)
  #return(history[start:stop])
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

generateStartPoints<-function(mi)
{
  startPoints<-data.frame(sigma_x=numeric(mi), sigma_y=numeric(mi), x=numeric(mi), y=numeric(mi))
  for (i in 1:mi)
  {
    startPoints$x[i]<-runif(1, -10, 10)
    startPoints$y[i]<-runif(1, -10, 10)
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


initialization<-function(points)
{
  return(points)
}

initModel<-function(points)
{
  historyPoints<-data.frame(points)
  return (historyPoints)
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
evo_max_iter<-10      # liczba iteracji
evo_lb <- -10         # ograniczenie dolne na x, y
evo_ub <- 10          # ograniczenie górne na x, y
evo_N <- 20           # liczba osobników w jednym pokoleniu
evo_p_cross <- 0.5    # prawdopodobieństwo krzyżowania
func <- f3            # funkcja wykorzystywana do ewaluacji: f1/f2/f3/f4

library(ggplot2)
library(rgl)
library(akima)

startPoints<-generateStartPoints(evo_N)
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

