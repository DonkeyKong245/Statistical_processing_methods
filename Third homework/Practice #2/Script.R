## Medvedev Andrey

# Define function
# Actually, straight from h\w doc
CI<-function(x_,S,n,P,sigma.known)
{
  if(sigma.known) Z_a<-qnorm(1-(1-P)/2) else Z_a<-qt(1-(1-P)/2,n-1)
  M<-Z_a*S/sqrt(n)
  c(x_-M,x_+M)
}

# Task 1 
votersN <- 2000
votersSupp <- 0.36
p <- 0.95
S <- sqrt(votersSupp * (1 - votersSupp))
CI(votersSupp, S, votersN, p, sigma.known = TRUE)

# Task 2
samples <- c(124, 124, 145, 132, 123, 124, 122, 141, 133, 122)
sMean <- mean(samples)
sVar <- var(samples)
sSem <- sqrt(sVar) / sqrt(length(samples))

left <- sMean - 1.96 * sSem
right <- sMean + 1.96 * sSem
c(left, right)

# Task 3
studentsN <- 20
sMean <- 520
sSd <- 65
sSem <- sSd / sqrt(studentsN)
t <- (sMean - 500) / sSem; t
pValue <- 2*pt(-abs(t),df=studentsN-1); pValue
