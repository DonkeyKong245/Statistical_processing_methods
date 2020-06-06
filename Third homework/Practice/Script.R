##  Medvedev Andrey

library("knitr")
library("agricolae")
library("multcomp")
print("Reading data source..")

data_big <- read.csv("data_big.csv")
print(".. data source read")

df<-data.frame(group=as.factor(data_big$anoreksia.1)
               , X=as.numeric(data_big$DBP.1))
table(df$group)

name.gr<-"anoreksia.1"
name.x<-"DBP.1"
boxplot(X~group,xlab=name.gr,ylab=name.x,data=df)

#1
bartlett.test(X~group,df)

ao<-aov(X~group,df)
plot(TukeyHSD(ao, "group"))

df.<-subset(df,df$group!=0)
S2<-(with(df.,tapply(X,group,'sd')))^2
S2

nn<-with(df.,tapply(X,group,'length'));nn
F.<-S2[3]/S2[2]
pf(F.,nn[3]-1,nn[2]-1)
p.F<-1-pf(F.,nn[3]-1,nn[2]-1)
p.F
t.test(X~group,subset(df,df$group!=0),var.equal=(p.F>0.05))

#2

ao<-aov(X~group,df)
summary(ao)

out <- LSD.test(ao,"group", p.adj="none",group=FALSE)
out

out <- LSD.test(ao,"group", p.adj="bonferroni",group=FALSE)
out

TukeyHSD(ao, "group", ordered = TRUE)

#3

t.test(data_big$DBP.1,data_big$DBP.2, paired = TRUE)
t.test(data_big$DBP.2,data_big$DBP.3, paired = TRUE)

dat.AR<-na.omit(subset(data_big,select=c(anoreksia.1,DBP.1,DBP.2,DBP.3)))
m<-ncol(dat.AR)-1;m 
Names<-names(table(dat.AR[,1])); Names; K<-length(Names)

dat.AR.T<-data.frame( stack(dat.AR[,-1])
                      ,sub=as.factor(rep(seq(nrow(dat.AR)),m))
                      ,gr=as.factor(rep(dat.AR[,1],m)))
dat.AR.T$values<-as.numeric(dat.AR.T$values)

formula<-values~gr*ind+Error(sub/ind)
aov.out <- aov(formula, data=dat.AR.T)
model.tables(aov.out,'mean')
aov.S<-summary(aov.out);
aov.S

interaction.plot(x.factor=dat.AR.T$ind
                 ,trace.factor=dat.AR.T$gr
                 ,response=dat.AR.T$values
                 ,type = "b"
                 ,legend = FALSE
                 ,trace.label ="group"
                 ,xlab = ""
                 ,ylab ='DBP'
                 ,lty = seq(K)
                 ,col = seq(K)
                 ,pch = 20
                 ,lwd=2)
legend('topright',Names,lty = seq(K), col =seq(K), cex=0.7,pch=20)

#4

tab<-table(sign(data_big$DBP.2-data_big$DBP.1));tab

binom.test(min(tab), sum(tab), p = 0.5,
           alternative = "less",
           conf.level = 0.95)

wilcox.test(data_big$DBP.1,data_big$DBP.2,paired=TRUE,exact = FALSE)

dat<-format.data.frame(data_big, Name="DBP")
friedman.test(as.matrix(dat[,c("DBP.1","DBP.2")]))
friedman.test(as.matrix(dat[,-1]))

#5

df<-data.frame(group1=as.factor(data_big$anoreksia.1)
               ,group2=as.factor(data_big$depressed.mood.1 )
               ,X=as.numeric(data_big$DBP.1))
ao<-aov(X~group1*group2,df)
SLM<-summary(ao)
SLM

df_<-SLM[[1]][,1]
y<-SLM[[1]][,3]
F1<-y[1]/y[3]; p1<-1-pf(F1,df_[1],df_[3])
F2<-y[2]/y[3]; p2<-1-pf(F2,df_[2],df_[3])

c(p1,p2)



