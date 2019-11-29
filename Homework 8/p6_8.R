f <- function(n) {
  x <- 2*runif(n)
  c(M=(min(x)+max(x))/2, Xbar=mean(x), Xtilde=median(x))
}
rbind(n101 = apply(replicate(500, f(101)), 1, var),
      n1001 = apply(replicate(500, f(1001)), 1, var),
      n10001 = apply(replicate(500, f(10001)), 1, var))
