library(glmnet)
library(readxl)
library(MASS)
Hald_Data <- read_excel("Documents/Reg/Hlad_Data.xlsx")
Hald_Data<-as.data.frame(Hald_Data)
colnames(Hald_Data) <- c("y","x1","x2","x3","x4")

library(leaps)
leaps( x=Hald_Data[,2:5], y=Hald_Data[,1], method="Cp")
leaps( x=Hald_Data[,2:5], y=Hald_Data[,1], method="adjr2")
leaps( x=Hald_Data[,2:5], y=Hald_Data[,1], method="r2")
#plots


regsubsets.out <- regsubsets(y~., data = Hald_Data,nvmax = 4)
summary.out<-summary(regsubsets.out)
summary.out$adjr2
summary.out$cp

names(reg.summary)
reg.summary$cp
reg.summary$rss
library(car)
subsets(leaps,statistics = "bic")