library(readxl)
library(ggplot2)
library(corrplot)
Data <- read_excel("house.xlsx")

#cleaning Data

for(i in 1:ncol(Data)){
  Data[is.na(Data[,i]), i] <- mean(Data[,i], na.rm = TRUE)
}
X<-as.matrix(Data[1:19])
X<-as.data.frame(X)
X[is.na(X),]<-0
y<-X[,1]
X1 <- X[,2]
X2 <- X[,3]
X3 <- X[,4]
X4 <- X[,5]
X5 <- X[,6]
X6 <- X[,7]
X7 <- X[,8]
X8 <- X[,9]
X9 <- X[,10]
X10 <- X[,11]
X11 <- X[,12]
X12 <- X[,13]
X13 <- X[,14]
X14 <- X[,15]
X15 <- X[,16]
X16 <- X[,17]
X17 <- X[,18]
X18 <- X[,19]
fit <- lm(y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13+X14+X15+X16+X17+X18,data = X)
summary(fit)
coef(fit) #r^2 = 0.6945

#coef of X11 is NA this implis that X11 is linearly independent with some attributes and thus is eliminated
fit1 <- lm(y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X12+X13+X14+X15+X16+X17+X18, data = X)
summary(fit1) #r^2 = 0.6945

M <- cor(X) #correlation
corrplot(M, method="number")

library(olsrr) 
#MODEL SELCTION
forwardp <- ols_step_forward_p(fit1,prem = 0.05)

backwardp <- ols_step_backward_p(fit1,prem = 0.05)

step <- ols_step_both_p(fit1, details = TRUE)


backwardaic <- ols_step_backward_aic(fit1,details = TRUE)

forwardaic <- ols_step_forward_aic(fit1,details = TRUE)

#checking normality of residuals
res<-residuals(fit1)
py <- predict(fit1)
qqnorm(res)
plot(fit1)

#transformation for response variable
library(car)
lambda<-boxCox(y ~ X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X12+X13+X14+X15+X16+X17+X18, family="yjPower", plotit = TRUE)
ind<-which(lambda$y == max(lambda$y))
lambda.max<-lambda$x[ind]
y.tr<-bcPower(y, lambda = lambda.max)
transform1<-lm(y.tr~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X12+X13+X14+X15+X16+X17+X18,data=X)
summary(transform1)#r^2 = 0.7753
plot(transform1)

Tstep <- ols_step_both_p(transform1, details = TRUE)#r^2 = 0.775 adj r2 =0.773


Tbackwardaic <- ols_step_backward_aic(transform1,details = TRUE)#r^2 = 0.77487 r2 = 0.77328 final model

Tforwardaic <- ols_step_forward_aic(transform1,details = TRUE)#r^2= 0.77487 adj r2 = 0.77328

#xlambda<-boxTidwell(y~ X1+X2+X3+X4+X5+X8+X9+X10+X12+X13+X14+X17+X18)

#wx.tr<-yjPower(wx, -0.8333)

final <- lm(y.tr~X2+X3+X4+X5+X6+X7+X8+X9+X12+X13+X14+X15+X16+X17)#r^2 = 0.7749 r2 = 0.7733 final model

Rres<-residuals(final)
Py<-predict(final)
Fy<-fitted(final)
plot(Py,abs(Rres))

wts <- 1/fitted(lm(abs(residuals(final)) ~ fitted(final)))^2

finalw <- lm(y.tr~X2+X3+X4+X5+X6+X7+X8+X9+X12+X13+X14+X15+X16+X17, weights=wts)
summary(finalw)
plot(finalw)#Multiple R-squared:  0.7627,	Adjusted R-squared:  0.7611 

#Outlier Analysis
res<- residuals(final)
py<-predict(final)
plot(py,res)
rstud<- rstudent(final)
pr<- residuals(final)/(1-lm.influence(final)$hat)

rst<-rstandard(final)
# combining dataset
resadd <- cbind(res,rstudent,rst)
#q-q plot
qqnorm(res)
#for all plots
plot(final)
outlierTest(final) # 327,412,1221,466



