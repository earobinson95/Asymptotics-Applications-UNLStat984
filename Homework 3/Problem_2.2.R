x <- runif(1000, 0, 1)
t <- seq(0.001,0.99,0.001)
poo <- as.data.frame(matrix(NA, length(t), 2))

for (i in 1:length(t)){
  tloop  <- t[i]
  Gt <- sum(x <= tloop)
  poo[i,1] <- t[i]
  poo[i,2] <- Gt
}

head(poo)

plot(poo$V1, poo$V2, type = "l")
lm(poo$V2 ~ poo$V1)

y <- poo$V2
y
hist(y)


length(y)
poo2 <- runif(990, 0, 1000)
plot(poo$V1, sort(poo2), type = "l")





# PART B ------------------------------------------------------------

x <- runif(1000, 0, 1)
t <- seq(0.001,0.99,0.001)
poo <- as.data.frame(matrix(NA, length(t), 2))

for (i in 1:length(t)){
  tloop  <- t[i]
  Gt <- sum(x <= tloop)
  poo[i,1] <- t[i]
  poo[i,2] <- Gt
}

head(poo)

plot(poo$V1, poo$V2, type = "l")
lm(poo$V2 ~ poo$V1)

y <- poo$V2
y
hist(y)


length(y)
poo2 <- runif(990, 0, 1000)
plot(poo$V1, sort(poo2), type = "l")
