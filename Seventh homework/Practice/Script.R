### Medvedev Andrey
library("cluster")

data_big <- read.csv("data_big.csv")

kable <- function(x, digits=3, label="")
{
  print(label)
  if (digits > 0)
  {
    x <- round (x, digits=digits)
  }
  print(x)
}

names <- c("HR.1", 
           "SBP.1", 
           "DBP.1",
           "MBP.1", 
           "SV.1",
           "CO.1",
           "SI.1",
           "CI.1",
           "TPR.1")
data <- data_big[names]
data[is.na(data)] <- 0

# Principal component method
data.0<-apply(data,2,function(x)(x-mean(x))/sd(x))
Sigma<-cov(data.0)
kable(Sigma, 
      digits=3, 
      label="Correlation matrix")

ei<-eigen(Sigma)
df<-data.frame('Eigen values'=ei$values,
               'Percent'=cumsum(ei$values)/sum(ei$values)*100,
               row.names = as.character(seq(dim(Sigma)[1])))
kable(df,
      label="Eigen values and total contribution")
      
kable(data.frame(ei$vectors),
      digits=3,
      label="Eigen vectors")

c(sum(ei$values),sum(diag(cov(data.0))))

Scores<-data.0 %*% ei$vectors

apply(Scores,2,var)
ei$values
c(apply(Scores,2,var),ei$values)

factors<-apply(Scores,2, function(x) x/sd(x))
kable(data.frame(factors), 
      digits=3,
      label="Factors")

Matr<-apply(Scores,2,function(y)apply(data.0,2,function(x)cor(x,y)))
kable(data.frame(Matr), 
      label="Factor matrix")

data.0<-scale(data)
pc<-prcomp(data.0)
kable(data.frame(lambda=pc$sdev^2,EI=ei$values),
      digits=3,
      label="Dispersions of the main components calculated by different methods")

kable(pc$rotation[,seq(2)],
      digits=3,
      label="The matrix of eigen vectors from prcomp.")

kable(data.frame(ei$vectors[,seq(2)]),
      digits=3,
      label="The matrix of eigen vectors of the covariance matrix.")

kable(pc$x[,seq(2)],
      digits=3,
      label="Values of the main components from prcomp.")

kable(data.frame(Scores[,seq(2)]),
      digits=3,
      label="The values of the main components from the transformation using rv")

MatrPC<-apply(rbind(pc$rotation[,seq(2)],pc$sdev[seq(2)]),2,function(x)x[-length(x)]*x[length(x)])
kable(MatrPC,label="Factor Load Calculation")
pc$x[,seq(2)]
plot(Scores[,seq(2)],type="n",xlab="PC1",ylab="PC2")
lines((-1)*pc$x[,seq(2)],col=2,type="p")
text(Scores[,seq(2)],as.character(1:34),cex=0.75)

# Recovery of variables from the first two factors

sigma2<-apply(data,2,var)
Rest<-function(k,sigma2,Matr,factors,data)
  {
  XX<-Matr[,seq(k)]%*%t(factors[,seq(k)])
  XX.1<-apply(cbind(XX,sqrt(sigma2)),1,function(x)x[-length(x)]*x[length(x)])
  XX.2<-apply(rbind(XX.1,colMeans(data)),2,function(x)x[-length(x)]+x[length(x)])
  return(XX.2)
  }
plot(data[,2],type="b")
lines(Rest(2,sigma2,Matr,factors,data)[,2],lty=2,col=2)

errors <- seq(9)
for (i in 1:length(errors))
{
  errors[i] <- sum(abs(data[,i] - Rest(2,sigma2,Matr,factors,data)[,i]))
}

cbind(names, errors)

# Factors interpretation
table <- data_big[,c("craving.to.alcohol.1", 
                     "tremor.1",
                     "sweating.1")]

kable(cor(cbind(table,Scores[,seq(2)])),
      label="Correlation matrix")

# Cluster analysis
dist <- daisy(data.0, metric="manhattan")
h <- hclust(dist, method="average")
plot(h)

dist.T <- daisy(t(data.0), metric="euclidean")
h <- hclust(dist, method="ward.D")
plot(h)

# K-means
total <- c()
for (i in 1:18)
{
  km <- kmeans(data.0, i)
  total <- c(total, km$tot.withinss)
}

plot(total, type="b")

km <- kmeans(data.0, 6); km

centroids <- km$centers %*% ei$vectors[,seq(2)]
centroids[, 2] <- -centroids[, 2]
colors <- 5 * km$cluster

plot(Scores[,seq(2)], type="n", xlab="PC1", ylab="PC2")
lines((-1)*pc$x[,seq(2)],type="p", col=colors)
text(centroids, as.character(1:6),cex=2,col(1:6)*5)
