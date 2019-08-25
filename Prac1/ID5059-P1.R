##Reference：P01-code-stats-functions
#Load functions!!
#-------------------------------------
source('P01-code-stats-functions.R')
library(knitr)
library(dplyr)
library(corrplot)
library(visreg)
library(ggplot2)
library(splines)
library(psych)
library(caret)
library(car)
library(mgcv)
library(VIM)
data <- read.csv("auto-data.csv",header = T, col.names = c("mpg","cylinders","displacement","horsepower","weight","acceleration","model_year", "origin","car_name"))
attach(data)


##Processing Data
anyNA(data)
is.na(data$horsepower)

data[!complete.cases(data),]

sum(is.na(data))
##Show missing value by plot
aggr(data,prop=F,numbers=T)
matrixplot(data)

##Using mice to address missing value
library(mice)
newdata<-data
data<-mice(newdata,m=6,method = "pmm",maxit = 100,seed=1)
sum(is.na(data))
##Check interpolation data
data$imp
data$imp$horsepower

##Generate a new data set
completedata<-complete(data)
head(completedata)

anyNA(completedata)
##The data density curve of the interpolated dataset
densityplot(data)
##The red part represents the interpolation data
stripplot(data,pch=12)

displacement = completedata$displacement
mpg = completedata$mpg
horsepower = completedata$horsepower
weight = completedata$weight
acceleration = completedata$acceleration

#-------------------------------------#
####     Descriptive Statistics     ####
#-------------------------------------#
pairs(mpg~displacement + horsepower + weight + acceleration, col="darkblue", pch=20, main="Scatterplot Matrices")
cor(completedata[,-c(2,7,8,9)])
describe(completedata[,-c(2,7,8,9)])
#histograms
hist(mpg, col="red", main="Histogram of mpg")
qqPlot(mpg, col="darkblue", main="QQ-Plot of mpg", pch=20)
hist(log(mpg))
qqPlot(log(mpg))

##Using caTools devides data set
require(caTools)
set.seed(100) 
index <-  sample(nrow(completedata), nrow(completedata)*.75)
train <- completedata[index,]
test <-  completedata[-index,]

trdisplacement = train$displacement
trmpg = train$mpg
trhorsepower = train$horsepower
trweight = train$weight
tracceleration = train$acceleration
tedisplacement = test$displacement
tempg = test$mpg
tehorsepower = test$horsepower
teweight = test$weight
teacceleration = test$acceleration

scale <- function(x){
  x<-x-min(x)
  x<-x/max(x)
}


#Displacement Scale
trdisplacement<-scale(trdisplacement)
tedisplacement<-scale(tedisplacement)
plot(trdisplacement,trmpg, col="darkblue", pch=20)
#Horsepower ScaleScale
trhorsepower<-scale(trhorsepower)
tehorsepower<-scale(tehorsepower)
plot(trhorsepower,trmpg, col="darkblue", pch=20)
#Weight ScaleScale
trweight<-scale(trweight)
teweight<-scale(teweight)
plot(trweight,trmpg, col="darkblue", pch=20)
#Acceleration ScaleScale
tracceleration<-scale(tracceleration)
teacceleration<-scale(teacceleration)
plot(tracceleration,trmpg, col="darkblue", pch=20)


#-------------------------------------
#####  Linear Regression  #####
#-------------------------------------
####-----OneDegree: Train Sets 
linreg(trdisplacement, trmpg, ploto=1)
linreg(trhorsepower, trmpg, ploto=1)
linreg(trweight, trmpg, ploto=1)
linreg(tracceleration, trmpg, ploto=1)

####-----OneDegree: Test Set  
#Displacement
lnmodel1 = linreg(trdisplacement, trmpg, ploto=1,opt=1)
funclm <- function(x,p){
  n <- length(x)
  q <- p + 1
  DM = matrix(1,n,q)
  DM[,2] <- x
  if(q>2) for(i in 3:q) { DM[,i] <- x**(i-1)}
  return(DM)
}
y_hat1 = funclm(tedisplacement,1) %*% coefficients(lnmodel1)
cat("testRSS: ", sum(sapply(tempg-y_hat1, function(x) { x^2 })), "\n")

#k-fold
kfold <-function(x)
{
  set.seed(50)
  vm <- matrix(nrow=10,ncol=2) 
  folds<-createFolds(y=train$mpg,k=10)
  for(i in 1:10){
    vtraindata<-train[-folds[[i]],]
    vtestdata<-train[folds[[i]],]
    vtraindata[,x] = scale(vtraindata[,x])
    vtestdata[,x] = scale(vtestdata[,x])
    vlnmodel = linreg(vtraindata[,x], vtraindata$mpg, ploto=1,opt=1)
    ##y_hat = funclm(vtestdata$displacement,1) %*% coefficients(vlnmodel)
    vm[i,]= coefficients(vlnmodel)
    print(vm)
  }
  return(vm)
}
vm = kfold(3)
colMeans(vm)
y_hat1_1 = funclm(tedisplacement,1) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(tempg-y_hat1_1, function(x) { x^2 })), "\n")

#Horsepower
lnmodel2 = linreg(trhorsepower, trmpg, ploto=1,opt=1)
y_hat2 = funclm(tehorsepower,1) %*% coefficients(lnmodel2)
cat("testRSS: ", sum(sapply(tempg-y_hat2, function(x) { x^2 })), "\n")
#k-fold
vm = kfold(3)
colMeans(vm)
y_hat2_1 = funclm(tehorsepower,1) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(tempg-y_hat2_1, function(x) { x^2 })), "\n")

#Weight
lnmodel3 = linreg(trweight, trmpg, ploto=1,opt=1)
y_hat3 = funclm(teweight,1) %*% coefficients(lnmodel3)
cat("testRSS: ", sum(sapply(tempg-y_hat3, function(x) { x^2 })), "\n")
#k-fold
vm = kfold(5)
colMeans(vm)
y_hat3_1 = funclm(teweight,1) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(tempg-y_hat3_1, function(x) { x^2 })), "\n")

#Acceleration
lnmodel4 = linreg(tracceleration, trmpg, ploto=1,opt=1)
y_hat4 = funclm(teacceleration,1) %*% coefficients(lnmodel4)
cat("testRSS: ", sum(sapply(tempg-y_hat4, function(x) { x^2 })), "\n")
#k-fold
vm = kfold(6)
colMeans(vm)
y_hat4_1 = funclm(teacceleration,1) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(tempg-y_hat4_1, function(x) { x^2 })), "\n")


####-----TwoDegree: Train Sets 

polreg(trdisplacement,trmpg,2, ploto=1)
polreg(trhorsepower,trmpg,2, ploto=1)
polreg(trweight,trmpg,2, ploto=1)
polreg(tracceleration,trmpg,2, ploto=1)

####-----TwoDegree: Test Set  
#Displacement
lnmodel5 = polreg(trdisplacement, trmpg,p=2, output=1, ploto=1, opt=1)
y_hat5 = funclm(tedisplacement,2) %*% coefficients(lnmodel5)
cat("testRSS: ", sum(sapply(tempg-y_hat5, function(x) { x^2 })), "\n")
#k-fold
kfold2 <-function(x)
{
  set.seed(50)
  vm <- matrix(nrow=10,ncol=3) 
  folds<-createFolds(y=train$mpg,k=10)
  for(i in 1:10){
    vtraindata<-train[-folds[[i]],]
    vtestdata<-train[folds[[i]],]
    vtraindata[,x] = scale(vtraindata[,x])
    vtestdata[,x] = scale(vtestdata[,x])
    vlnmodel = polreg(vtraindata[,x], vtraindata$mpg,2, output=1, ploto=1, opt=1)
    ##y_hat = funclm(vtestdata$displacement,1) %*% coefficients(vlnmodel)
    vm[i,]= coefficients(vlnmodel)
    print(vm)
  }
  return(vm)
}
vm = kfold2(3)
colMeans(vm)
y_hat5_1 = funclm(tedisplacement,2) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(tempg-y_hat5_1, function(x) { x^2 })), "\n")

#Horsepower
lnmodel6 = polreg(trhorsepower, trmpg,p=2, output=1, ploto=1, opt=1)
y_hat6 = funclm(tehorsepower,2) %*% coefficients(lnmodel6)
cat("testRSS: ", sum(sapply(tempg-y_hat6, function(x) { x^2 })), "\n")
#k-fold
vm = kfold2(4)
colMeans(vm)
y_hat6_1 = funclm(tehorsepower,2) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(tempg-y_hat6_1, function(x) { x^2 })), "\n")

#Weight
lnmodel7 = polreg(trweight, trmpg,p=2, output=1, ploto=1, opt=1)
y_hat7 = funclm(teweight,2) %*% coefficients(lnmodel7)
cat("testRSS: ", sum(sapply(tempg-y_hat7, function(x) { x^2 })), "\n")
#k-fold
vm = kfold2(5)
colMeans(vm)
y_hat7_1 = funclm(teweight,2) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(tempg-y_hat7_1, function(x) { x^2 })), "\n")

#Acceleration
lnmodel8 = polreg(tracceleration, trmpg,p=2, output=1, ploto=1, opt=1)
y_hat8 = funclm(teacceleration,2) %*% coefficients(lnmodel8)
cat("testRSS: ", sum(sapply(tempg-y_hat8, function(x) { x^2 })), "\n")
#k-fold
vm = kfold2(6)
colMeans(vm)
y_hat8_1 = funclm(teacceleration,2) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(tempg-y_hat8_1, function(x) { x^2 })), "\n")


####-----ThreeDegree: Train Sets 

polreg(trdisplacement,trmpg,3, ploto=1)
polreg(trhorsepower,trmpg,3, ploto=1)
polreg(trweight,trmpg,3, ploto=1)
polreg(tracceleration,trmpg,3, ploto=1)

####-----ThreeDegree: Test Set  
#Displacement
lnmodel9 = polreg(trdisplacement, trmpg,p=3, output=1, ploto=1, opt=1)
y_hat9 = funclm(tedisplacement,3) %*% coefficients(lnmodel9)
cat("testRSS: ", sum(sapply(tempg-y_hat9, function(x) { x^2 })), "\n")
#k-fold
kfold3 <-function(x)
{
  set.seed(50)
  folds<-createFolds(y=train$mpg,k=10)
  vm <- matrix(nrow=10,ncol=4) 
  for(i in 1:10){
    vtraindata<-train[-folds[[i]],]
    vtestdata<-train[folds[[i]],]
    vtraindata[,x] = scale(vtraindata[,x])
    vtestdata[,x] = scale(vtestdata[,x])
    vlnmodel = polreg(vtraindata[,x], vtraindata$mpg,3, output=1, ploto=1, opt=1)
    ##y_hat = funclm(vtestdata$displacement,1) %*% coefficients(vlnmodel)
    vm[i,]= coefficients(vlnmodel)
    print(vm)
  }
  return(vm)
}
vm = kfold3(3)
colMeans(vm)
y_hat9_1 = funclm(tedisplacement,3) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(tempg-y_hat9_1, function(x) { x^2 })), "\n")

#Horsepower
lnmodel10 = polreg(trhorsepower, trmpg,p=3, output=1, ploto=1, opt=1)
y_hat10 = funclm(tehorsepower,3) %*% coefficients(lnmodel10)
cat("testRSS: ", sum(sapply(tempg-y_hat10, function(x) { x^2 })), "\n")
#k-fold
vm = kfold3(4)
colMeans(vm)
y_hat10_1 = funclm(tehorsepower,3) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(tempg-y_hat10_1, function(x) { x^2 })), "\n")

#Weight
lnmodel11 = polreg(trweight, trmpg,p=3, output=1, ploto=1, opt=1)
y_hat11 = funclm(teweight,3) %*% coefficients(lnmodel11)
cat("testRSS: ", sum(sapply(tempg-y_hat11, function(x) { x^2 })), "\n")
#k-fold
vm = kfold3(5)
colMeans(vm)
y_hat11_1 = funclm(teweight,3) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(tempg-y_hat11_1, function(x) { x^2 })), "\n")

#Acceleration
lnmodel12 = polreg(tracceleration, trmpg,p=3, output=1, ploto=1, opt=1)
y_hat12 = funclm(teacceleration,3) %*% coefficients(lnmodel12)
cat("testRSS: ", sum(sapply(tempg-y_hat12, function(x) { x^2 })), "\n")
#k-fold
vm = kfold3(6)
colMeans(vm)
y_hat12_1 = funclm(teacceleration,3) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(tempg-y_hat12_1, function(x) { x^2 })), "\n")


#----------------------------#
#####     Bin Smooth     #####  
#----------------------------#
##Train Set 
funbinsmoothREG <- function(x,y)
{
  binsmoothREG (x, trmpg, binlength=y, knotsdef=NULL, output=1, ploto=1, opt=1)
  binlength = y
  knotsdef=NULL
  modelbs = binsmoothREG(x, trmpg, binlength, knotsdef, opt=1)
}
#binlength
par(mfrow=c(1,2))
layout(matrix(1:4,2,2))
funbinsmoothREG(trdisplacement,10)
funbinsmoothREG(trhorsepower,10)
funbinsmoothREG(trweight,10)
funbinsmoothREG(tracceleration,10)

funbinsmoothREG(trdisplacement,30)
funbinsmoothREG(trhorsepower,30)
funbinsmoothREG(trweight,30)
funbinsmoothREG(tracceleration,30)

funbinsmoothREG(trdisplacement,60)
funbinsmoothREG(trhorsepower,60)
funbinsmoothREG(trweight,60)
funbinsmoothREG(tracceleration,60)
par(mfrow=c(1,1))


##### -----Test Set-----####
#binlength=60
binlength = 60
knotsdef=NULL
modelbs = binsmoothREG(trdisplacement, trmpg, binlength, knotsdef, opt=1)
bintest <- function(x,m){
  binlength = 60
  om <- sort(m)
  if(is.vector(knotsdef)) bins = knotsdef else bins = ceiling(length(x) / binlength)
  binlength = length(m)/bins
  #Create Design Matrix without intercept
  DM <- matrix(1,length(om),bins)
  #Set all elements not corresponding to region j equal 0
  for(i in 1:bins)
  {
    if(i==1) { xstart = 1 }
    if(i>1) { xstart = (i-1)*binlength+1 }
    xend = min(xstart + binlength-1, length(x))
    binelements <- xstart:xend
    elements <- 1:length(m)
    elements[binelements] <- 0
    DM[elements,i] <- 0
  }
  return(DM)
}
DM = bintest(trdisplacement,tedisplacement)
otempg <- tempg[order(tedisplacement)]
y_hat = DM %*% coefficients(modelbs)
cat("testRSS: ", sum(sapply(otempg-y_hat, function(x) { x^2 })), "\n")
#k-fold
kfold_bin <-function(x)
{
  set.seed(50)
  folds<-createFolds(y=train$mpg,k=10)
  vm <- matrix(nrow=10,ncol=5) 
  for(i in 1:10){
    vtraindata<-train[-folds[[i]],]
    vtestdata<-train[folds[[i]],]
    vtraindata[,x] = scale(vtraindata[,x])
    vtestdata[,x] = scale(vtestdata[,x])
    vmodelbs = binsmoothREG(vtraindata[,x], vtraindata$mpg, binlength=54, knotsdef=5, opt=1)
    vm[i,]= coefficients(vmodelbs)
    print(vm)
  }
  return(vm)
}
vm = kfold_bin(3)
colMeans(vm)
y_hat = bintest(trdisplacement,tedisplacement) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(otempg-y_hat, function(x) { x^2 })), "\n")

##Test-Horsepower
binlength = 60
knotsdef=NULL
modelbs2 = binsmoothREG(trhorsepower, trmpg, binlength, knotsdef, opt=1)
DM = bintest(trhorsepower,tehorsepower)
otempg <- tempg[order(tehorsepower)]
y_hat = DM %*% coefficients(modelbs2)
cat("testRSS: ", sum(sapply(otempg-y_hat, function(x) { x^2 })), "\n")
#k-fold
vm = kfold_bin(4)
colMeans(vm)
y_hat = bintest(trhorsepower,tehorsepower) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(otempg-y_hat, function(x) { x^2 })), "\n")

##Test-Weight
binlength = 60
knotsdef=NULL
modelbs3 = binsmoothREG(trweight, trmpg, binlength, knotsdef, opt=1)
otempg <- tempg[order(teweight)]
DM = bintest(trweight,teweight)
y_hat = DM %*% coefficients(modelbs3)
cat("testRSS: ", sum(sapply(otempg-y_hat, function(x) { x^2 })), "\n")
#k-fold
vm = kfold_bin(5)
colMeans(vm)
y_hat = bintest(trweight,teweight) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(otempg-y_hat, function(x) { x^2 })), "\n")

##Test-Acceleration
binlength = 60
knotsdef=NULL
modelbs4 = binsmoothREG(tracceleration, trmpg, binlength, knotsdef, opt=1)
otempg <- tempg[order(teacceleration)]
DM = bintest(tracceleration,teacceleration)
y_hat = DM %*% coefficients(modelbs4)
cat("testRSS: ", sum(sapply(otempg-y_hat, function(x) { x^2 })), "\n")
#k-fold
vm = kfold_bin(6)
colMeans(vm)
y_hat = bintest(tracceleration,teacceleration) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(otempg-y_hat, function(x) { x^2 })), "\n")


#-------------------------------#
#####  B-Spline Regression  #####
#-------------------------------#
##Train-Set
bsplinereg(trdisplacement, trmpg, knots=7, knotsdef=NULL,  degree=3, output=1, ploto=1, opt=0)
bsplinereg(trhorsepower, trmpg, knots=7, knotsdef=NULL,  degree=3, output=1, ploto=1, opt=0)
bsplinereg(trweight, trmpg, knots=7, knotsdef=NULL,  degree=3, output=1, ploto=1, opt=0)
bsplinereg(tracceleration, trmpg, knots=7, knotsdef=NULL,  degree=3, output=1, ploto=1, opt=0)

####Test 
#Displacement
tespline <- function(x,y){
  knots=7
  degree=3
  modelsp = bsplinereg(x, trmpg, knots, knotsdef=c(), degree, ploto=1, opt=1)
  n <- length(y)
  #Calculate knot postions
  if(knots == 0) knotpos <- NULL
  if(knots != 0) knotpos <- 1:knots / (knots+1)
  if(length(knotsdef)>0) knotpos <- knotsdef
  #Create Design Matrix 
  DM <- bspline(y, degree, knotpos) 
  return(DM)
}
DM = tespline(trdisplacement,tedisplacement)
modelsp = bsplinereg(trdisplacement, trmpg, knots=7, knotsdef=c(), degree=3, ploto=1, opt=1)
y_hat = DM %*% coefficients(modelsp)
cat("testRSS: ", sum(sapply(tempg-y_hat, function(x) { x^2 })), "\n")
#k-fold
kfold_sp <-function(x)
{
  set.seed(50)
  folds<-createFolds(y=train$mpg,k=10)
  vm <- matrix(nrow=10,ncol=11) 
  folds<-createFolds(y=train$mpg,k=10)
  for(i in 1:10){
    vtraindata<-train[-folds[[i]],]
    vtestdata<-train[folds[[i]],]
    vtraindata[,x] = scale(vtraindata[,x])
    vtestdata[,x] = scale(vtestdata[,x])
    modelsp = bsplinereg(vtraindata[,x], vtraindata$mpg, knots=7, knotsdef=c(), degree=3, ploto=1, opt=1)
    vm[i,]= coefficients(modelsp)
    print(vm)
  }
  return(vm)
}
vm = kfold_sp(3)
colMeans(vm)
y_hat = tespline(trdisplacement,tedisplacement) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(tempg-y_hat, function(x) { x^2 })), "\n")

#Horsepower
DM = tespline(trhorsepower,tehorsepower)
modelsp = bsplinereg(trhorsepower, trmpg, knots=7, knotsdef=c(), degree=3, ploto=1, opt=1)
y_hat = DM %*% coefficients(modelsp)
cat("testRSS: ", sum(sapply(tempg-y_hat, function(x) { x^2 })), "\n")
#k-fold
vm = kfold_sp(4)
colMeans(vm)
y_hat = tespline(trhorsepower,tehorsepower) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(tempg-y_hat, function(x) { x^2 })), "\n")

#Weight
DM = tespline(trweight,teweight)
modelsp = bsplinereg(trweight, trmpg, knots=7, knotsdef=c(), degree=3, ploto=1, opt=1)
y_hat = DM %*% coefficients(modelsp)
cat("testRSS: ", sum(sapply(tempg-y_hat, function(x) { x^2 })), "\n")
#k-fold
vm = kfold_sp(5)
colMeans(vm)
y_hat = tespline(trweight,teweight) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(tempg-y_hat, function(x) { x^2 })), "\n")

#WAcceleration
DM = tespline(tracceleration,teacceleration)
modelsp = bsplinereg(tracceleration, trmpg, knots=7, knotsdef=c(), degree=3, ploto=1, opt=1)
y_hat = DM %*% coefficients(modelsp)
cat("testRSS: ", sum(sapply(tempg-y_hat, function(x) { x^2 })), "\n")
#k-fold
vm = kfold_sp(6)
colMeans(vm)
y_hat = tespline(tracceleration,teacceleration) %*% colMeans(vm)
cat("testRSS: ", sum(sapply(tempg-y_hat, function(x) { x^2 })), "\n")





