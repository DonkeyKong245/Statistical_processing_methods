##  Medvedev Andrey
##  Geometric distribution, p = 0.677
##  1. Model distribution
print("Modeling Geom(0.677) distribution.. ")

n <- 10000
p <- 0.677
x <- rgeom(n, prob=p) + 1

library(ggplot2)
data <- data.frame(index = seq(0, n-1),
                   value = x[0:n])
ggplot(data, aes(x=value)) + 
  geom_histogram(binwidth=1, colour="black", fill="white")
ggsave("histogram.png")

print(".. distribution has been modeled")

##  2. Calculate characteristics
print("Calculating characteristics.. ")

xMean <- mean(x)
xBiasedVar <- mean((x - xMean)^2)
xUnbaisedVar <- n * xBiasedVar / (n - 1)
xSigma <- sd(x)
xSem <- xSigma / sqrt(n)
xMedian <- median(x)
xMax <- max(x)
xMin <- min(x)
xQuantiles <- quantile(x)
xIqr <- IQR(x)
xMoment3 <- mean((x - xMean)^3)
xGamma1 <- xMoment3 / xSigma^3
xM4 <- mean((x - xMean)^4)
xGamma2 <- xM4 / xSigma^4

print(paste0("Mean: ", xMean))
print(paste0("Biased variance: ", xBiasedVar))
print(paste0("Unbiased variance: ", xUnbaisedVar))
print(paste0("Standard deviation: ", xSigma))
print(paste0("Mean error: ", xSem))
print(paste0("Median: ", xMedian))
print(paste0("Max: ", xMax))
print(paste0("Min: ", xMin))
print(paste0("Interquartile range: ", xIqr))
print(paste0("Third central moment: ", xMoment3))
print(paste0("Forth central moment: ", xM4))
print(paste0("Skewness: ", xGamma1))
print(paste0("Kurtosis: ", xGamma2))
print(".. characteristics calculated")

##  3. Estimate parameters
print("Estimating parameters.. ")

pEstimatedMM <- 1 / xMean
pEstimatedML <- 1 / xMean

print(paste0(".. estimated using moments method: ", pEstimatedMM))
print(paste0(".. estimated using max likelyhood method: ", pEstimatedML))
print(".. estimated parameters")

##  4. Pearson's chi square test
print("Processing Pearson's chi square test..")

xCountPerValue <- vector()
for (i in xMin:(xMax+1))
{
  temp <- x[x == i]
  xCountPerValue <- c(xCountPerValue, length(temp))
}

xFromEstimated <- pgeom(seq((xMin-2), (xMax-1)), prob=pEstimatedMM)
xFromEstimated = c(xFromEstimated, 1)
estimated = xFromEstimated[2:(xMax+2)] - xFromEstimated[1:(xMax+1)]
estimated = n * estimated
diffSquared = (xCountPerValue - estimated) ^ 2
chiSquare = sum(diffSquared / estimated)
print(paste0(".. chi square test processed: ", chiSquare))

##  5. Shapiro-Wilk's test
print("Processing Shapiro-Wilk's test..")
print(".. normal distribution")

n <- 1000
mean <- 0
standardDeviation <- 1
print(paste0(".. mean: ", mean))
print(paste0(".. standard deviation: ", standardDeviation))

x <- rnorm(n, mean, standardDeviation)
shapiroTestResult <- shapiro.test(x)
print(shapiroTestResult)

print(".. Shapiro-Wilk's test processed")