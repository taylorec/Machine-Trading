library("caret")
library("corrplot")
library("forecast")
library("kernlab")
library("neuralnet")
library(dplyr)
library("PerformanceAnalytics")
library("quantmod")
library("tseries")
library("xgboost")
library(randomForest)

getSymbols('DIS')
sym <- DIS
sym.H <- sym$DIS.High
sym.L <- sym$DIS.Low
sym.V <- sym$DIS.Volume
tail(sym.V) #Check previous week volume
date <- Sys.Date()
H <- xts(mean(tail(sym.H)), date)
L <- xts(mean(tail(sym.L)), date)
sym.H <- rbind(sym.H, H)
sym.L <- rbind(sym.L, L)

sym.H1 <- Lag(sym.H,k=1)
sym.H2 <- Lag(sym.H,k=2)
sym.H3 <- Lag(sym.H,k=3)
sym.H4 <- Lag(sym.H,k=4)
sym.H5 <- Lag(sym.H,k=5)
sym.H6 <- Lag(sym.H,k=6)
sym.H7 <- Lag(sym.H,k=7)
sym.H8 <- Lag(sym.H,k=8)
sym.H9 <- Lag(sym.H,k=9)

sym.L1 <- Lag(sym.L,k=1)
sym.L2 <- Lag(sym.L,k=2)
sym.L3 <- Lag(sym.L,k=3)
sym.L4 <- Lag(sym.L,k=4)
sym.L5 <- Lag(sym.L,k=5)
sym.L6 <- Lag(sym.L,k=6)
sym.L7 <- Lag(sym.L,k=7)
sym.L8 <- Lag(sym.L,k=8)
sym.L9 <- Lag(sym.L,k=9)

sym.H.all <- cbind(sym.H, sym.H1, sym.H2, sym.H3, sym.H4, sym.H5, sym.H6, sym.H7, sym.H8, sym.H9)
colnames(sym.H.all) <- cbind('sym.H', 'sym.H1', 'sym.H2', 'sym.H3', 'sym.H4', 'sym.H5', 'sym.H6', 'sym.H7', 'sym.H8', 'sym.H9')
sym.H.all <- sym.H.all[complete.cases(sym.H.all),]

sym.L.all <- cbind(sym.L, sym.L1, sym.L2, sym.L3, sym.L4, sym.L5, sym.L6, sym.L7, sym.L8, sym.L9)
colnames(sym.L.all) <- cbind('sym.L', 'sym.L1', 'sym.L2', 'sym.L3', 'sym.L4', 'sym.L5', 'sym.L6', 'sym.L7', 'sym.L8', 'sym.L9')
sym.L.all <- sym.L.all[complete.cases(sym.L.all),]

# Training Range
sym.H.t <- window(sym.H.all, start='2017-01-01', end=date-1)
sym.L.t <- window(sym.L.all, start='2017-01-01', end=date-1)
# Machine Trading Models
set.seed(101)

# Random Forest Model
#MT.H.sym <- randomForest(sym.H~sym.H1+sym.H2+sym.H3+sym.H4+sym.H5+sym.H6+sym.H7+sym.H8+sym.H9,data=sym.H.t,importance=TRUE)
#MT.L.sym <- randomForest(sym.L~sym.L1+sym.L2+sym.L3+sym.L4+sym.L5+sym.L6+sym.L7+sym.L8+sym.L9,data=sym.L.t,importance=TRUE)

# XGB Model
MT.H.sym <- train(sym.H~sym.H1+sym.H2+sym.H3+sym.H4+sym.H5+sym.H6+sym.H7+sym.H8+sym.H9,data=sym.H.t,method="xgbTree",preProcess="pca")
MT.L.sym <- train(sym.L~sym.L1+sym.L2+sym.L3+sym.L4+sym.L5+sym.L6+sym.L7+sym.L8+sym.L9,data=sym.L.t,method="xgbTree",preProcess="pca")

# Neural Net Model
#tsctrlt <- trainControl(method="timeslice",initialWindow=168,horizon=82,fixedWindow=TRUE)
#MT.H.sym <- train(sym.H~sym.H1+sym.H2+sym.H3+sym.H4+sym.H5+sym.H6+sym.H7+sym.H8+sym.H9,data=sym.H.t,method="neuralnet",preProcess="pca", trControl=tsctrlt)
#MT.L.sym <- train(sym.L~sym.L1+sym.L2+sym.L3+sym.L4+sym.L5+sym.L6+sym.L7+sym.L8+sym.L9,data=sym.L.t,method="neuralnet",preProcess="pca", trControl=tsctrlt)

# Support Vector Machine Model
#MT.H.sym <- train(sym.H~sym.H1+sym.H2+sym.H3+sym.H4+sym.H5+sym.H6+sym.H7+sym.H8+sym.H9,data=sym.H.t,method="svmRadial",preProcess="pca")
#MT.L.sym <- train(sym.L~sym.L1+sym.L2+sym.L3+sym.L4+sym.L5+sym.L6+sym.L7+sym.L8+sym.L9,data=sym.L.t,method="svmRadial",preProcess="pca")

# Trading Range
sym.H.i <- window(sym.H.all, start='2019-01-01')
sym.L.i <- window(sym.L.all, start='2019-01-01')

#Trading Strategy
MT.H.sym.mi <- predict(MT.H.sym,newdata=sym.H.i)
MT.L.sym.mi <- predict(MT.L.sym,newdata=sym.L.i)
MT.HL.sym.mdfi <- cbind(index(sym.H.i),as.data.frame(MT.H.sym.mi),as.data.frame(MT.L.sym.mi))
MT.HL.sym.mdfi$BuyPr <- MT.L.sym.mi+((MT.H.sym.mi-MT.L.sym.mi)/6)
MT.HL.sym.mdfi$SellPr <- MT.H.sym.mi-((MT.H.sym.mi-MT.L.sym.mi)/6)
MT.HL.sym.mli <- xts(MT.HL.sym.mdfi[,-1],order.by=as.Date(MT.HL.sym.mdfi[,1]))
MT.HL.sym.ms <- window(MT.HL.sym.mli,start="2019-01-01")
colnames(MT.HL.sym.ms) <- c('Pred High', 'Pred Low', 'Buy Price', 'Sell Price')
View(tail(MT.HL.sym.ms,10))
