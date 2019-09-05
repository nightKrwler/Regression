library(readxl)
Rocket_Data <- read_excel("Documents/Reg/data.xlsx")

X<-as.matrix(Rocket_Data)
y<-X[,1]
x1<-X[,2]
x2<-X[,3]

mod1 = lm(y~x1+x2)

# summary statistics
summary(mod1)

res<- residuals(mod1)
py<-predict(mod1)
plot(py,res)
rstud<- rstudent(mod1)
pr<- residuals(mod1)/(1-lm.influence(mod1)$hat)

rst<-rstandard(mod1)
# combining dataset
resadd <- cbind(res,rstud,rst)
#q-q plot
qqnorm(res)
#for all plots
plot(mod1)