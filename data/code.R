library(readxl)
Rocket_Data <- read_excel("Documents/Reg/rocket.xlsx")

X<-as.matrix(Rocket_Data[1:2])
y<-X[,1]
x<-X[,2]
plot(t(x),t(y),ylab="Shear strength",xlab="Age of Propellant",main="Rocket Data")
X0<-rep(1,nrow(X)) 

mod1 = lm(y~x)

#fitted regression line
abline(2627.82, -37.15)
#alternate method to fit the regression line
abline(lm(y ~ x))
# summary statistics
summary(mod1)
# ANOVA test (already a part of above summary statistics)
anova(mod1)
# 95% confidence interval (default) of the mean response
predict(mod1, data.frame(x=13.3625), interval = "confidence")
# 95% confidence interval (default) prediction interval
predict(mod1, data.frame(x=10), interval = "prediction")