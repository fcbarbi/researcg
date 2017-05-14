
# -------------------------------------------
# statR.R
# fcbarbi@gmail.com
# Start: 19 Set 2016  
# Update: May 2017 
# -------------------------------------------

# update the working directory and check that it changed
# Mac/Linux 
setwd("/Users/fcbarbi/Documents/statR") 
# Windows 
#setwd("C:\\Documents\\statR")
getwd()

# -------------------------------------------
# install only once 
# install.packages("ISwR")
require(ISwR)

# load R dataset and summarize it 
data(bp.obese)
summary(bp.obese)
head(bp.obese)

# load all datasets: check options with ?data 
data(package="ISwR")

# save it in CSV and open in Eviews or Excel 
write.csv(bp.obese,"bp_obese.csv")

#install.packages("psych")
#require(psych)
psych::describe(bp.obese)

# import CSV data 

students <- read.csv("http://dss.princeton.edu/training/students.csv", header=TRUE, stringsAsFactors = FALSE)
summary(students)

# import EXCEL data 
# xls_file <- "students.xls"
# download.file("http://dss.princeton.edu/training/students.xls", xls_file, method="auto", quiet=FALSE, mode = "wb", cacheOK= TRUE)
# install.packages("gdata")
# require(gdata)  # require perl installed...
# xlsfile <- file.path( getwd(), xls_file )
# students2 <- read.xls( xlsfile, sheet = 1, header = TRUE)
  
# install.packages("xlsx")
# require(xlsx)
# xlsxfile <- "students.xlsx"
# xlsxfile <- file.path( getwd(), xlsxfile )
# read.xlsx( xlsxfile, sheetName = "Sheet1")

# import TXT data
data(malaria)
str(malaria)
write.table(malaria,"malaria.txt")
malaria2 <- read.table("malaria.txt",header=T)
str(malaria2)
all.equal(malaria,malaria2)  # datasets are they different ? 

# MOMENTS

# consistent variance estimator 1/(N-1)
N <- 100
x <- rnorm(N) 
mx <- mean(x); mx
se <- sqrt(sum((x-mx)^2)/(N-1));se # standard error 
sd(x) # compare with se
#install.packages("timeDate")
skew <- sum((x-mx)^3)/(N*se^3); skew
timeDate::skewness(x)
kurt <- sum((x-mx)^4)/(N*se^4); kurt
timeDate::kurtosis(x,method="moment")

# median robust to outliers 
set.seed(222)
x1 <- rnorm(100)
x2 <- x1
x2[10] <- x2[60] <- 100
x2[40] <- x2[90] <- -200
plot( ts(x2) )
cat("mean varied ",abs(mean(x2)-mean(x1)))
cat("median varied ",abs(median(x2)-median(x1)))

#png("figs/multimodal.png") 
curve(dnorm(x,1,1)+dnorm(x,6,2),-4,10)
#dev.off()

# DISTRIBUTIONS 

# Normal Distribution

# plot discrete distribution 
x = seq(-4,4,0.1) # change step to 0.001 for a continuous plot or use curve()
plot(x,dnorm(x),ylab="",col="blue",main="Discrete Normal")

# continuous distribution 
curve(dnorm(x),-4,4,type="l",ylab="",main="Continuous Normal",col="red")

# plots the cumulative probability curve between -4 and 4
#png("figs/gaussian_cumulative.png")
curve(pnorm(x),-4,4,main="Cumulative Gaussian distribution")
#dev.off()


# mirror functions: pnorm() and qnorm()
qnorm(0.05/2) # -1.959964
qnorm(1-0.05/2) # 1.959964
pnorm(1.96)-pnorm(-1.96) # 0.9500042

qnorm(1-0.10/2) # 1.644
pnorm(1.644)-pnorm(-1.644) # 0.8998238

qnorm(1-0.01/2) # 2.575829
pnorm(2.58)-pnorm(-2.58) # 0.99012

# P( -2 < (x-mu)/(sigma/N^.5) < 2 ) = ?
(pnorm(2)-pnorm(-2)) # 0.9544997


# t-student distribution
#png("figs/student_distribution.png") 
qt(0.025,df=100)   # -1.983972
qt(1-0.025,df=100) # 1.983972
pt(1.98,df=100)-pt(-1.98,df=100) # 0.9495484
curve(dt(x,df=100),-4,4,add=F,col="red",main="Student-t distribution")
curve(dt(x,df=1),-4,4,add=T,col="black",ylab="")
legend("topright",legend=c("df=100","df=1"),lty=c(1,1),col=c("red","black"))
#dev.off()

# Chi-squared distribution

qchisq(0.975,df=10) # 20.48318
qchisq(1-0.975,df=10) # 3.246973
pchisq(20.5,df=10)-pchisq(3.25,df=10) # 0.95005

#png("figs/chisq_distribution.png") 
curve(dchisq(x,df=2),0,8,add=F,,col="red",main="Chi-square distribution")
curve(dchisq(x,df=4),0,8,add=T,col="black")
curve(dchisq(x,df=8),0,8,add=T,col="blue")
legend("topright",legend=c("df=2","df=4","df=8"),lty=c(1,1),col=c("red","black","blue"))
#dev.off()

# F distribution

qf(0.975,  df1=10,df2=10) # 3.716792
qf(1-0.975,df1=10,df2=10) # 0.2690492
pf(3.71,df1=10,df2=10)-pf(0.26,df1=10,df2=10) # 0.9525199

#png("figs/F_distribution.png") 
curve(df(x,df1=1,df2=1),add=F,col="red",main="F distribution",ylab="")
curve(df(x,df1=1,df2=10),add=T,col="black",ylab="")
curve(df(x,df1=10,df2=10),add=T,col="blue",ylab="")
legend("topright",legend=c("df=1,1","df=1,10","df=10,10"),lty=c(1,1),col=c("red","black","blue"))
#dev.off()


# WLLN
N <- 100

p <- 0.4
#dataframe
df <- data.frame(bi = rbinom(n, 1, p)  ,count = 0, mean = 0)
ifelse(df$bi[1] == 1, df[1, 2:3] <- 1, 0)
for (i in 2 : n){
  df$count[i] <- ifelse(df$bi[i] == 1, df$count[i]<-df$count[i - 1]+1, df$count[i - 1])
  df$mean[i] <- df$count[i] / i
}

N <- 500
mu <- 0.5
means <- rep(NA,N)
set.seed(222)
for (i in 1:N) 
  means[i] <- mean(rnorm(i,mean=mu))
#png("figs/wlln.png")
plot(means, type='l',
     main = "Low of Large Numbers",
     xlab="Sample size", ylab="Sample mean")
abline(h = mu, col="red")
#dev.off()


N <- 1e2
ret  <- rnorm(N,0.5,0.8) # ativo com retorno de 0.5% 
plot(ts(ret))

index <- function(x) { 
  iret <- rep(NA,length(x))
  iret[1] <- ret[1]
  for (i in 2:length(ret)) 
    iret[i] <- ((1+iret[i-1]/100)*(1+ret[i]/100)-1)*100
  iret
}
iret <- index(ret)

#var(iret)
head(cbind(ret,iret))

N <- 1e2
ret  <- rnorm(N,0.5,0.8) # ativo com retorno de 0.5% 
index <- function(ret) { 
  iret <- (cumprod(1+ret/100)-1)*100
  iret+100
}
iret <- index(ret)
dlret <- c(0,diff(log(iret)))*100
head(cbind(ret,iret,dlret))

plot(ts(iret))

curve(log(x),0,1e3)
abline(h=5)
abline(h=4)
abline(v=exp(5))
abline(v=exp(4),lty=2)


plot(ts(ret))
var(ret)

xret <- scale(ret)+100
plot(ts(xret))  
var(xret)

# Ax = b
b=c(7,4)
A <- matrix(c(2,0,1,3), ncol=2) 
B <- matrix(c(5,2,4,-1), ncol=2)
x <- b*solve(A)
# A*x == B ?



# Confidence Intervals CI

# Calculate the 95% CI for X ~ N(83,144) for a sample with 50 observations 
xbar <- 83
sigma <- sqrt(144)
n <- 50
sem <- sigma/sqrt(n)
cat("CI upper boundray is", xbar + sem * qnorm(0.025))
cat("CI lower boundray is", xbar + sem * qnorm(0.975))

# simulate 1000 obs X ~ N(0,1)
N <- 1000
x1 <- rnorm(N,mean=0,sd=1)
sprintf("x ~ N(0,1) has mean %4.4f and sd %4.4f ",mean(x1),sd(x1)) 

# simulate 1000 obs X ~ t(df=N)
N <- 1000
x2 <- rt(N,df=N,ncp=0)
sprintf("x ~ t(df=%d) has mean %4.4f and sd %4.4f ",N,mean(x2),sd(x2)) 

# can we distinguish between these two distributions ?
t.test(x1,x2)
var.test(x1,x2)

# encapsulate it in a function with default values 
simula <- function(p_N=1000,p_mean=0,p_sd=0,p_dist="g",p_df){
  set.seed(222)
  if (p_dist=="g") x <- rnorm( p_N, mean=p_mean, sd=p_sd )
  if (missing(p_df)) p_df <- p_N
  if (p_dist=="t") x <- rt( p_N, df=p_df )
  sprintf("x has mean %4.4f and sd %4.4f ",mean(x),sd(x))
  x
}
g1 <- simula(1000,83,12)
t1 <- simula(50,p_dist="t")



# Tests

# H0: beta1=0 and Ha: bhat1<>0
set.seed(222)
bhat1 <- rnorm(100,mean=5,sd=1) 
mean(bhat1) 
t.test(bhat1) 

# H0: bhat1=bhat2 and Ha: bhat1<>bhat2
set.seed(222)
bhat2 <- rnorm(100,mean=5,sd=3) 
mean(bhat2) 

# mean/sd test 
t.test(bhat1,bhat2) 
# variance test 
var.test(bhat1,bhat2)

library(tseries)
set.seed(222)
jarque.bera.test( rnorm(100,mean=0,sd=1) ) 
jarque.bera.test( rnorm(100,mean=0,sd=5) )

set.seed(222)
shapiro.test(rnorm(100, mean=0, sd=5))
shapiro.test(rnorm(10 , mean=0, sd=5))

# check if the variance in the two groups is equal
#library(car)
#leveneTest(mpg~am, center = mean)
  
# Aggregation Tables 

data(mtcars)
head(mtcars)

aggregate(mpg~am,data=mtcars,FUN = mean)
aggregate(mpg~am,data=mtcars,FUN = range)

aggregate(mpg~am,by=list(am),data=mtcars,FUN = mean)

# normality test for each category
aggregate(mpg~vs,data=mtcars,FUN = function(x) shapiro.test(x))
# warning because shapiro.test returns four values so extract p values
aggregate(mpg~vs,data=mtcars,FUN = function(x) shapiro.test(x)$p.value)


# Hisogram and QQPlot 

# Histogram with normal density
#png("figs/histogram.png",width=600,height = 480)
op<-par(mfrow=c(1,2))
set.seed(222)
x <- rnorm(1e3,mean=0,sd=1)
hist(x, freq=FALSE, breaks=10,main="Histogram by density",xlab="N(0,1)")
lines(density(x),col="black",lwd=2)
hist(x, freq=TRUE,main="Histogram by frequency",xlab="N(0,1)")
par(op)
#dev.off()

install.packages("ggplot2")
require(ggplot2)
ggplot()





#png("figs/qqplot_gaussian.png")
qqnorm(x)  
qqline(x)    
#dev.off()

#png("figs/qqplot_student.png")
x <- rt(1000,df=5)
qqnorm(x,main="Student t QQ-Plot")  
qqline(x)
#dev.off()

data(mtcars)
summary(mtcars)
psych::describe(Wages)

Wages_stats <- psych::describe(Wages,range=FALSE) 
write.csv(Wages_stats,"Wages.csv")
print(xtable::xtable(Wages_stats),file="Wages.tex",type="latex")


data(cars)
plot(cars, lab=c(20,10,6), cex.axis=.6, las=1)
points(x=c(23,26), y=c(60,61), col="red")

N <- 1e3
Z <- rnorm(N)
X <- 2*Z + rnorm(N,sd=0.1)
Y <- 0.5*Z + rnorm(N)
cor(X,Z) # 0.9987634
cor(X,Y) # 0.4582502
a <- cor(Y,Z); a # 0.4593239 
cor(Y-a*Z,X) # 0.05500443
m1 <- lm(X~Y)
m2 <- lm(X~Y+Z) 
stargazer::stargazer(m1,m2,out="omit.tex")

require(grDevices)
matplot((-4:5)^2, main = "Quadratic") # almost identical to plot(*)


attach(mtcars)
png("figs/scatter_plot.png")
plot(wt, mpg, main="Does weight affects consumption?", 
     xlab="Car Weight ", ylab="Miles Per Gallon ", pch=19)
abline(lm(mpg~wt), col="red") # regression line (y~x) 
#lines(lowess(wt,mpg), col="blue") # lowess line (x,y)
dev.off()

# Basic Scatterplot Matrix
png("figs/scatter_matrix.png")
pairs(~mpg+disp+drat+wt,data=mtcars, main="Factors to impact consumption")
dev.off()

data(mtcars)
# name the groups 
mtcars$am2 <- factor(mtcars$am, levels = c(0,1), labels = c("Automatic", "Manual"))
attach(mtcars)
#png("figs/boxplot_mtcars.png")
boxplot(mpg~am2,main = "Mileage of Automatic and Manual Cars", 
        xlab = "Type of car", ylab = "Miles per gallon")
#dev.off()

# Calculate the 99% CI for X ~ t(df=5) for a sample with 100 observations
# 

# transform to standard distribution 

N <- 100
x <- rnorm(N,mean=5,sd=50)
standard <- function(x) { (x-mean(x))/sd(x) }
z1 <- standard(x)
mean(z1) # 0 
sd(z1) # 1

reference <- rnorm(N)
dev.off()
qqplot(x,reference,ylab=("N(0,1)"),main="QQ-Plot of Gaussian distribution")
qqline(x,reference)

z2 <- scale(x, center = TRUE, scale = TRUE)
head(cbind(z1,z2))

y <- rnorm(N,mean=5,sd=500)
zy <- scale(y)
dev.off()
qqplot(zy,reference,ylab=("N(0,1)"),main="QQ-Plot of Gaussian distribution")
qqline(zy,reference)

z_x <- scale(x)
op <- par(mfcol=c(2,1))
plot( ts(z_x) )
plot( ts(z_y) )
par(op)

# sampling 
set.seed(222)
dgp <- rnorm(1000,mean=1,sd=10)

dgp1 <- sample( dgp, 100 ) # random sampling 
dgp2 <- sample( dgp, 100 ) # random sampling 
mean(dgp1)
mean(dgp2)
t.test(dgp1,dgp2)

dgp3 <- dgp[order(dgp)][900:1000] # biggest values
t.test(dgp1,dgp3)

# DATASETS

require(Ecdat)
data(Wages)
Wages_stats <- psych::describe(Wages,range=FALSE) 
openxlsx::write.xlsx(Wages_stats, file = "Wages_stats.xlsx", row.names=TRUE)

require(ISwR)

# blood pressure 
data(bp.obese)
summary(bp.obese)
head(bp.obese)

# occupation related diseases 
data(nickel)
psych::describe(nickel)
head(nickel)

# maternal milk 
data(kfm)
psych::describe(kfm)
head(kfm)
#edit(kfm)

# malaria dataset 
data(malaria)
psych::describe(malaria)

# strokes in Estonia 
data(stroke)
psych::describe(stroke)
head(stroke)


# eof
