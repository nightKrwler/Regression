library(readxl)
library(csv)
library(csvread)

House_Data <- read_excel(file = "Documents/Reg/houses.csv", header = TRUE)
X<-as.matrix(House_Data[1:21])
X<-X[c(1:1999),]
X[is.na(X)]<-0
y <- X[,3]
X <- X[,-c(3,1,2)]
x3<-X[,3]
x2<-X[,2]

X<- as.data.frame(X)
colnames(X) <- c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","x16","x17","x18")
lm_fit <- lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9+x10+x11+x12+x13+x14+x15+x16+x17+x18,data=X)
summary(lm_fit)
