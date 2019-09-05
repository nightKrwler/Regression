library(MASS)
library(readxl)
library(car)
ID_Data <- read_excel("Documents/Reg/ID.xlsx")
IX<-as.matrix(ID_Data)
Iy<-IX[,2]
Ix<-IX[,1]
plot(Ix,Iy)
mod<-lm(Iy~Ix)
summary(mod)
res<-residuals(mod)
py<-predict(mod)
plot(py,res)
plot(Ix,res)

# weighted 
xw<-1/Ix
modwet<-lm(Iy~Ix, weights = xw)
summary(modwet)
res<-residuals(modwet)
py<-predict(modwet)
plot(py,res)
plot(Ix,res)

#Box_cox Transformation
lambda<-boxCox(Iy~Ix, family="yjPower", plotit = TRUE)
ind<-which(lambda$y == max(lambda$y))
lambda.max<-lambda$x[ind]
Iy.tr<-bcPower(Iy, lambda = lambda.max)
modt<-lm(Iy.tr~Ix)
summary(modt)
rest<-residuals(modt)
pyt<-predict(modt)
plot(pyt,rest)


library(readxl)
ELCT_Data <- read_excel("Desktop/Regression Analysis/Electricity Data.xlsx")

X<-as.matrix(ELCT_Data)
y<-X[,2]
x<-X[,1]

mod1<-lm(y~x)
summary(mod1)
plot(mod1)
#required package car
# Box-Cox transformation
lambda<-boxCox(y~x, family="yjPower", plotit = TRUE)
ind<-which(lambda$y == max(lambda$y))
lambda.max<-lambda$x[ind]
y.tr<-bcPower(y, lambda = lambda.max)
mod2<-lm(y.tr~x)
summary(mod2)
plot(mod2)
WM_Data <- read_excel("Desktop/Reg/Wind_Mill_Data.xlsx")
WX<-as.matrix(WM_Data)
wy<-WX[,2]
wx<-WX[,1]

wmod<-lm(wy~wx)
summary(wmod)
plot(wmod)
wres<-residuals(wmod)
plot(wx,wres)

xlambda<-boxTidwell(wy~wx)

wx.tr<-yjPower(wx, -0.8333)
wmodn<-lm(wy~wx.tr)
summary(wmodn)
wnres<-residuals(wmodn)
plot(wx.tr,wnres)
plot(wmodn)

# Restaurant Data

library(readxl)
Rest_Data <- read_excel("Desktop/Regression Analysis/Restaurant Data.xlsx")

RX<-as.matrix(Rest_Data)
Ry<-RX[,1]
Rx<-RX[,2]
Rmod.lm<-lm(Ry~Rx)
summary(Rmod.lm)

Rres<-residuals(Rmod.lm)
Py<-predict(Rmod.lm)
Fy<-fitted(Rmod.lm)
plot(Py,abs(Rres))

wts <- 1/fitted(lm(abs(residuals(Rmod.lm)) ~ fitted(Rmod.lm)))^2

WRmod.lm<-lm(Ry~Rx, weights = wts)
summary(WRmod.lm)
plot(WRmod.lm)