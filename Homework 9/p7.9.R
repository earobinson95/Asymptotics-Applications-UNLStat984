logLogistic <- function(theta = theta, der = 0, x = x){
  n = length(x)
  
  value = theta*n-sum(x)-2*sum(log(1+exp(theta-x)))
  if(der == 0) return(value)
  
  der1 = n-2*sum(exp(theta)/(exp(x)+exp(theta)))
  if(der == 1) return(list(value = value, der1 = der1))
  
  der2 = -2*sum(exp(theta+x)/(exp(x)+exp(theta))^2)
  return(list(value = value, der1 = der1, der2 = der2))
}

logLogistic(theta = 2, der = 2, x = x)

newtonUni = function(f, xInit, maxIt = 20, relConvCrit = 1.e-10,...){
  
  results = matrix(NA, maxIt, 5)
  colnames(results ) = c("value", "x", "Conv", "slope", "Hess")
  
  xCurrent = xInit
  for(t in 1:maxIt){
    evalF = f(xCurrent, der = 2,...)
    results[t, "value"] = evalF$value
    results[t, "x"] = xCurrent
    results[t, "slope"] = evalF$der1
    results[t, "Hess"] = evalF$der2
    xNext = xCurrent - evalF$der1/evalF$der2
    Conv = abs(xNext-xCurrent)/(abs(xCurrent)+relConvCrit)
    results[t, "Conv"] = Conv
    if(Conv < relConvCrit | t > maxIt) break
    xCurrent = xNext
  }
  return(list(theta = xNext, value = f(xNext, der = 0,...), convergence = (Conv < relConvCrit), t = t))
}

x <- c(1.0944, 6.4723, 3.1180, 3.8318, 4.1262,
       1.2853, 1.0439, 1.7472, 4.9483, 1.7001,
       1.0422, 0.1690, 3.6111, 0.9970, 2.9438)
thetaMLE <- newtonUni(logLogistic, xInit = median(x), x = x)$theta
delta <- newtonUni(logLogistic, xInit = median(x), x = x, maxIt = 1)$theta

cbind(thetaMLE, delta)

empiricalLogistic <- function(samps = 200, n = 15, theta = 2){
  median = rep(0, samps)
  delta = rep(0, samps)
  for (i in 1:samps) {
    x = rlogis(n, location = theta)
    median[i]=median(x)
    delta[i] = newtonUni(logLogistic, xInit = median[i], x = x, maxIt = 1)$theta
  }
  n*c(var(median), var(delta))
}
empiricalLogistic(samps = 200, n = 15, theta = 2)


logCauchy <- function(theta = theta, der = 0, x = x){
  n = length(x)
  
  value = -n*log(pi)-sum(log(1+(x-theta)^2))
  if(der == 0) return(value)
  
  der1 = sum((2*(x-theta))/(1+(x-theta)^2))
  if(der == 1) return(list(value = value, der1 = der1))
  
  der2 = sum((2*(x-theta)^2-2)/(1+(x-theta)^2)^2)
  return(list(value = value, der1 = der1, der2 = der2))
}

empiricalCauchy <- function(samps = 500, n = 51, theta = 2){
  median = rep(0, samps)
  delta = rep(0, samps)
  for (i in 1:samps) {
    x = rcauchy(n, location = theta)
    median[i]=median(x)
    #delta[i] = newtonUni(logCauchy, xInit = median[i], x = x, maxIt = 1)$theta
    delta[i] = median[i]+(2*logCauchy(theta = median[i], der = 1, x = x)$der1)/n
  }
  n*c(var(median), var(delta))
}
empiricalCauchy(samps = 500, n = 51, theta = 0)

