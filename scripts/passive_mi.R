
#generate questionnaire data
x <- gen_qdata(n=100, nq=5, k= c(10,10,10,10,10))

#generate covariate data
cov <- MASS::mvrnorm(n=100, mu=c(5,5,5), Sigma=matrix(c(10,1,1,1,10,1,1,1,10),3,3))
colnames(cov) <- c("cov1", "cov2", "cov3")

x <- data.frame(x,cov)
#generate missings
alpha <- 0.25
pattern <- matrix(c(sample(c(0,1), size=2*sum(k),replace = TRUE)),nrow=2)
pattern <- cbind(pattern, matrix(c(1,1,1,1,1,1),nrow=2))
f <- c(0.5,0.5)
g <- c(4,4)
x <- MAR(x,alpha,pattern,f,g)
colnames(x) <- c(paste0("I",1:sum(k)),c("cov1", "cov2", "cov3") )


library(mice)
library (mitools)
library(Hmisc)

#calculate total scores for questionnaires
ts <- calculate_ts(x, nq=5,k= c(10,10,10,10,10))
data <- data.frame(x,ts)

#set up imputation model
library(mice)
ini <- mice(data, max=0, print=FALSE)
meth <- ini$meth

#for each questionnaire adapt imputation method (meth): make function to calculate TS between iterations. Example TS1:
#meth["TS1"] <- "~I(I1+I2+I3+I4+I5+I6+I7+I8+I9+I10)" - loop below automates for simulation data
nq <- 5
k <- c(10,10,10,10,10)
for(q in seq_along(1:nq)){
  meth[paste0("TS",q)] <- paste0("~I(", paste(paste0("I",1:k[q]), collapse="+"), ")")
}

#adapt predictor matrix such that items are imputed by items own questionnaire and other TSs.
#target variable mi is in row and column variables are predictors
pred <- ini$predictorMatrix
pred[paste0("I", 1:10),] <- 0
pred[paste0("I", 1:10),paste0("I", 1:10)] <- 1
pred[paste0("I", 1:10),c("TS2","TS3","TS4","TS5")] <- 1
pred[paste0("I", 11:20),] <- 0
pred[paste0("I", 11:20),paste0("I", 11:20)] <- 1
pred[paste0("I", 11:20),c("TS1","TS3","TS4","TS5")] <- 1
pred[paste0("I", 21:30),] <- 0
pred[paste0("I", 21:30),paste0("I", 21:30)] <- 1
pred[paste0("I", 21:30),c("TS1","TS2","TS4","TS5")] <- 1
pred[paste0("I", 31:40),] <- 0
pred[paste0("I", 31:40),paste0("I", 31:40)] <- 1
pred[paste0("I", 31:40),c("TS1","TS2","TS3","TS5")] <- 1
pred[paste0("I", 41:50),] <- 0
pred[paste0("I", 41:50),paste0("I", 41:50)] <- 1
pred[paste0("I", 41:50),c("TS1","TS2","TS3","TS4")] <- 1
pred[,colnames(cov)] <- 1 #covariates as predictors for all items
pred <- pred*ini$predictorMatrix

#item items and total scores with passive MI
imp1 <- mice(data, m=15, meth=meth,pred=pred, maxit=10, seed=12354, print=FALSE)
plot(imp1)
#impute total score only from other ts (and covariates if excist)
tsdata <- data.frame(ts, cov)
imp2 <- mice(tsdata, 15,maxit=10,seed=61085,print=FALSE)


#combine imputations and select (per questionnaire) the ts from imp1 for persons with <70% of items missing;
#and the ts from imp2 for persons with >70% of items missing (in a questionnaire).

##make indicators for each q if missing <70%:
calculate_i <- function(x,nq,k){
  ind <- matrix(0,nrow=nrow(x), ncol=(nq))

  for (q in 1:(nq)){
    ind[,q] <- apply(x[,(((q*k[q])-k[q])+1):(q*k[q])],1,function(x) {sum(is.na(x))/length(x)})
  }
  colnames(ind) <- paste("TS",1:nq,sep="")
  ind <- ifelse(ind <0.7,1,0)
  ind
}
indicator <- calculate_i(x=data, nq=5, k=c(10,10,10,10,10))


implist <- lapply(1:15, function(x){
  x1 <- complete (imp1, x)
  x2 <- complete(imp2, x)
  tsimp <- lapply(1:nq, function(x){
    ifelse(indicator[,x]==1, x1[,paste0("TS",x)], x2[,paste0("TS",x)] )
  })
  tsimp <- data.frame(tsimp)
  colnames(tsimp) <- paste0("impTS",1:nq)
  data.frame(x1,tsimp)
})

## test of difference between T3 and T1 for each questionnaire
impdata <- imputationList (implist)
fit <- with (impdata, lm(impTS1~impTS3 )) ## regressie met verschil score als uitkomst. - zelfde als paired t-test (vergeleken)
summary(pool (fit))
