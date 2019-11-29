n   <- seq(1, 50, 1)
x1  <- log(log(n))
x2  <- log(n)
x3  <- n
x4  <- log(factorial(n))
x5  <- n^2
x6  <- 2^(3*log(n))
x7  <- n^(log(n))
x8  <- 3^n
x9 <- log(n)^n
x10 <- n^(n/2)
x11 <- factorial(n)
x12 <- n^n
x13 <- 2^(2^n)

ratio <- x12/x13
plot(n, ratio, type = "l")
head(cbind(n, x11, x12))
