## Medvedev Andrey
## f(x,a,b) = (x+a)^2 + b*x^3

## 1
N<-100

## Define functions
f<-function(x,ab) (x+ab[1])^2+ab[2]*(x^3)
L<-function(X,Y,ab) sum((Y-f(X,ab))^2)
f0<-function(x,AB) AB[1]+AB[2]*x
L0<-function(X,Y,AB) sum((Y-f0(X,AB))^2)

## Define parameters
ab<-c(1,2)
eps<-4

## Define non-linear model function
X<-rnorm(N)
Y<-f(X,ab)+rnorm(N,0,eps)
SLM<-summary(lm(Y~X))
AB<-SLM$coefficients[,1]
Y.<-f0(X,AB)

#2
## Estimate linear model parameters
EstLM<-function(X,Y)
  {
  b.<-(sum(X*Y)-N*mean(X)*mean(Y))/(sum(X^2)-N*mean(X)^2);
  a.<-mean(Y)-AB[2]*mean(X)
  c(a.,b.)
  }
AB<-EstLM(X,Y);AB

## Estimate non-linear model parameters
NLM<-nlm(function(ab)L(X,Y,ab),c(1,1))
ab.<-NLM$estimate
cbind(ab.=ab.,ab=ab)

plot(X,Y)
f_<-function(x)f(x,ab); curve(f_,-2,3,add=TRUE,col=2)
f_<-function(x)f(x,ab.); curve(f_,-2,3,add=TRUE,col=3)
f_<-function(x)f0(x,AB); curve(f_,-2,3,add=TRUE,col=4,lty=2)
legend('bottomright',c('hyp','mnk','linear'),pch=20,col=c(2,3,4))

## Output errors
c(Q.linear=L0(X,Y,AB),Q.model=L(X,Y,ab),Q.model.hat=L(X,Y,ab.))

## 3
## Analysis of variance
QT<-sum(( Y-mean(Y) )^2);QT
QR<-sum((Y.-mean(Y))^2);QR
QE<-sum((Y-Y.)^2);QE
R2<-QR/QT;R2

xx<-sum((X-mean(X))^2);xx
S2<-QE/(N-2)
S2a<-S2*sum(X^2)/N/xx
S2b<-S2/xx

F.<-QR/QT*(N-2);F.
Pf<-1-pf(F.,2,N-2);Pf
Ta<-AB[1]/sqrt(S2a);Ta
Tb<-AB[2]/sqrt(S2b);Tb
Pa<-2*(1-pt(abs(Ta),N-2));Pa
Pb<-2*(1-pt(abs(Tb),N-2));Pb

## Analyse with default functions
LM<-lm(Y~X)
SLM<-summary(LM);
cbind(AB,SLM$coefficients[,1])
c(R2=R2,SLM$r.squared)
df<-SLM$df[seq(2)];df
Pf.lm<-1-pf(SLM$fstatistic[1],df[1],df[2])
cbind(c(Pf=Pf,Pa=Pa,Pb=Pb),c(Pf=Pf.lm,SLM$coefficients[,4]))

## 4
## Multiple regression
a<-c(1,-3,10)
eps<-2
X1<-rnorm(N,-1,1)
X2<-rnorm(N,2,0.5)
Y<-a[1]*X1+a[2]*X2+a[3]+rnorm(N,0,eps)

LM<-lm(Y~X1+X2)
SLM<-summary(LM)
SLM

op <- par(mfrow = c(2, 2))
plot(X1,SLM$residuals)
title(sub=paste("r",round(cor(SLM$residuals,X1),3), sep="="))
plot(X2,SLM$residuals)
title(sub=paste("r",round(cor(SLM$residuals,X2),3), sep="="))

plot(X1,Y)
title(sub=paste("r",round(cor(X1,Y),3), sep="="))
plot(X2,Y)
title(sub=paste("r",round(cor(X2,Y),3), sep="="))

par(op)
cor(X1,SLM$residuals)