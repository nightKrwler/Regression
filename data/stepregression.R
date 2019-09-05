library(readxl)
library(MASS)

Hald_Data <- read_excel("Desktop/Regression Analysis/Hald_Cement_Data.xlsx")
Hald_Data<-as.data.frame(Hald_Data)
colnames(Hald_Data) <- c("y","x1","x2","x3","x4")
model<-lm(y~x1+x2+x3+x4,data = Hald_Data)
# install olsrr
library(olsrr) 
ols_step_all_possible(model)

ols_step_forward_p(model,prem = 0.05)

ols_step_backward_p(model,prem = 0.05)

ols_step_both_p(model, details = TRUE)


ols_step_backward_aic(model,details = TRUE)

ols_step_forward_aic(model,details = TRUE)

