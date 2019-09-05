library(readxl)
library(MASS)
library(ridge)
Acetylene_Data <- read_excel("Desktop/Regression Analysis/Acetylene_Data.xlsx")
Ace_Data<-as.data.frame(Acetylene_Data)
Ace_Data<-as.matrix(Ace_Data)
#Ace_Data_N<-cbind(Ace_Data)
y<-Ace_Data[,1]
x1<-(Ace_Data[,2]-mean(Ace_Data[,2]))/sqrt(var(Ace_Data[,2]))
x2<-(Ace_Data[,3]-mean(Ace_Data[,3]))/sqrt(var(Ace_Data[,3]))
x3<-(Ace_Data[,4]-mean(Ace_Data[,4]))/sqrt(var(Ace_Data[,4]))
x1sq<-x1^2
x2sq<-x2^2
x3sq<-x3^2
x1x2<-x1*x2
x1x3<-x1*x3
x2x3<-x2*x3

lm_fit<-lm(y~x1+x2+x3+x1x2+x1x3+x2x3+x1sq+x2sq+x3sq)
summary(lm_fit)
Ace_Data_N<-cbind(y,x1,x2,x3,x1x2,x1x3,x2x3,x1sq,x2sq,x3sq)
colnames(Ace_Data_N) <- c("y","x1","x2","x3","x1x2","x1x3","x2x3","x1sq","x2sq","x3sq")
Ace_Data_N<-data.frame(Ace_Data_N)
library(lmridge)
mod <- lmridge(y~., data = Ace_Data_N, K = c(0, 0.0132, 0.1))
plot(mod, type = "ridge")
mod0 <- lmridge(y~., data = Ace_Data_N, K = 0)
## coefficients for first biasing parameter
summary(mod0)$summaries[[1]]$coefficients
summary(mod0)$summaries[[1]]$stats
mod1 <- lmridge(y~., data = Ace_Data_N, K = 0.01)
## coefficients for first biasing parameter
summary(mod1)$summaries[[1]]$coefficients
summary(mod1)$summaries[[1]]$stats
