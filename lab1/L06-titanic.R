########  Titanic Predictions ###########
##### Data from Kaggle.com
##### Tom Kelsey, Jan 2019

## set workspace
setwd("/Users/tom/Dropbox/Teaching/ID5059-KDDM/2019/Lectures/L06")

## load train data
sink <- read.csv("L06-train.csv", header=TRUE)
attach(sink)
 

## load test data
check <- read.csv("L06-test.csv", header=TRUE)
attach(check)


##### Classification tree
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(rpart)

## women and children first!
wacf.train <- rpart(Survived ~ Sex + Age, method="class", data=sink)

## plot the tree
plot(wacf.train, uniform=TRUE,
   main="Women and Children First")
text(wacf.train, use.n=TRUE, all=TRUE, cex=.8)

## the rpart plot is terrible, so get something better
fancyRpartPlot(wacf.train)

## learn a tree for a larger subset of covariates
CART.train <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare, method="class", data=sink)

## view details
summary(CART.train)

## plot the tree
fancyRpartPlot(CART.train)

# prune the tree
pfit<- prune(CART.train, 
	cp= CART.train$cptable[which.min(CART.train$cptable[,"xerror"]),"CP"])

# plot the pruned tree
fancyRpartPlot(pfit)


# missing step - impute missing covariate values in the test data
check3 <- read.csv("test-imputed.csv", header=TRUE)
attach(check3)
preds <- predict(pfit,check3)
pid <- subset(check3, select=PassengerId)
out <- cbind(pid,round(preds[,2],digits=0))
write.csv(out,"ToKaggleCART.csv", row.names=FALSE)
detach(check3)

#### 79% accurate ##########




