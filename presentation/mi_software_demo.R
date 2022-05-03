## data inlezen 
boys <- read.delim2("Data demo/boys.dat")
View(boys)

install.packages("foreign")

library(foreign)

boys <-read.spss("Data demo/boys.sav", to.data.frame=TRUE)


## summary of data
summary(boys)



## multiple imputation
install.packages("mice")
library(mice)

imp <-mice(boys, m=5, maxit=10, seed = 800)

## predictor matrix
imp$pred

imp$pred["bmi",] <- c(0,0,0,0,0)
imp$pred
imp$pred[,"bmi"] <- c(0,0,0,0,0)
imp$pred


pred_update <- imp$pred

## methode per variable
imp$meth

imp$meth["bmi"] <- "~I(wgt/(hgt/100)^2)"
imp$meth

meth_update <- imp$meth

imp_update <- mice(boys, m=5, maxit=10, pred=pred_update, meth=meth_update, seed = 90)


plot(imp_update)


fit <- with(imp_update, lm(hc ~ age))

pool(fit)



