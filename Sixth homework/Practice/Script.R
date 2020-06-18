### Medvedev Andrey
library("knitr")
library(klaR)

kable <- function(x, digits = 3, caption = "")
{
  print(caption)
  if (digits > 0)
  {
    x <- round(x, digits=digits)
  }
  print(x)
}

data_big <- read.csv("data_big.csv")
df<-na.omit(data.frame(tremor=as.factor(ifelse(data_big$tremor.1<=1,0,1)),
                       scale(data_big[,c(23:30)])))

# Discriminant analysis
ldaSummary <- lda(tremor~.,df)
kable(data.frame(ldaSummary$scaling),
      digits=3,
      caption = "Weights")
table(tremor=df$tremor,Predict=predict(ldaSummary, df)$class)

characteristcs.mm<-tapply(predict(ldaSummary,df)$x,
           df$tremor,
           function(x) mean(x,na.rm=TRUE))
characteristcs.sd<-sd(predict(ldaSummary,df)$x)

f1<-function(x) dnorm(x,mean = characteristcs.mm[[1]],sd=characteristcs.sd)
f2<-function(x) dnorm(x,mean = characteristcs.mm[[2]],sd=characteristcs.sd)
curve(f1,-3,3)
curve(f2,-3,3,col=2,add=TRUE)

rt <- uniroot(function(x)  f1(x) - f2(x)  , c(.01,10), tol=1e-8)     
print(rt$root)

# Decision tree
library("tree")

treeResult <- tree(tremor~., df, split = "gini")
prediction <- predict(treeResult,df)
classification <- apply(prediction,1,function(x)ifelse(x[1]>x[2],0,1))
table(df$tremor, classification)

plot(treeResult)
text(treeResult)

# Random forest
library('randomForest')

randomForestResult<-randomForest(tremor~.,df)
randomForestPrediction<-predict(randomForestResult,df)
table(tremor=df$tremor,Predict=randomForestPrediction)

commonRandomForest <-randomForest(df)
MDSplot(commonRandomForest,df$tremor,palette=c(1,2))

library(e1071)
svmResult<-svm(tremor~.,df)
svmPrediction<-predict(svmResult,df)
table(tremor=df$tremor,Predict=svmPrediction)
