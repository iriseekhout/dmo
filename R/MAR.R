## written by Iris Eekhout mrt 2012 (www.iriseekhout.com)
## translated from JPL Brand et al (Brand, J. P. L., van Buuren, S., Groothuis-Oudshoorn, K., & Gelsema, E. S. (2003). A toolkit in SAS for the evaluation of multiple imputation methods. Statistica Neerlandica, 57(1), 36-45).

#' Generate MAR (missign at random) data
#'
#' @param x data frame where missing observations should be generated in.
#' @param alpha proportion of cases that will get a missing data pattern
#' @param pattern a matrix with ncol=ncol(data), nrow=number of missing data patterns; for each patter 0 indicates missing and 1 observed.
#' @param f frequency of each pattern
#' @param g the odds of the patterns to occur, the strength of the mechanism
#'
#' @return A data frame that contains missing observations
#' @export
#'
#' @examples
#' library(MASS)
#' x <- mvrnorm(n=100,mu=c(0,0,0), Sigma=matrix(c(5,1,1,1,5,1,1,1,5),3,3))
#' alpha <- 0.25
#' pattern <- matrix(c(1,1,0,1,0,1),2,3, byrow=T)
#' f <- c(0.5,0.5)
#' g <- c(4,4)
#' MAR(x,alpha,pattern,f,g)
MAR <- function(x, alpha, pattern, f,g=4, a=pattern)
{
  xobs <- testcand1 <- testpip <- tests <- testincompl <- testcand2 <- testresp <- fltest <- cltest<- bltest <- list()
  quant=data.matrix(c(rep(0.5,nrow(pattern))))
  g=data.matrix(c(rep(g,nrow(pattern))))
  x <- data.matrix(x)
  n <- NROW(x)
  m <- NCOL(x)
  sf <- sum (f)
  f <- data.matrix (f/sf)
  u <- runif(n)
  res <- outer(u, cumsum(f), ">")   # vervanging voor loop
  cand <- rowSums(res) + 1          # vervanging voor loop
  testcand1 <- cbind(u, cand)
  ss <- x %*% t(a*pattern)
  sortss <- apply(ss, 2, function(z) sort(z) )
  p <- data.matrix (rep(0,n))
  for( i in 1:nrow(pattern) ){
    gi <- cbind(1, g[i])
    quanti <- cbind(cbind(0, quant[i,]), 1)
    sumx <- 0
    for (j in 1:NCOL(gi)){
      sumx <- sumx + (quanti[j+1] - quanti[j]) %*% gi[j]
    }
    si <- matrix(sortss[, i], n, 1)
    ci <- si[ceiling(NROW(si) %*% t(quant[i,]))]
    boolcan <- cand == i
    boolcan <-ifelse(boolcan == T, 1, 0)
    si <- data.matrix(ss[which(boolcan !=0), i])
    cl <- rep(1, NROW(si))
    for(j in 1:NROW(ci)){
      fl <- rep(ci[j], NROW(si))
      fltest [[i]] <- fl
      bl <- ifelse(si > fl, 1, 0)
      cl <- cl + bl
    }
    pip <- data.matrix(rep(0, NROW(si)))
    for(j in 1:(NROW(ci) + 1)){
      boolcl <- cl == j
      boolcl <- ifelse(boolcl == T, 1, 0)
      if ( any(boolcl == 1) ){
        pip[which(boolcl !=0)] <- rep( ((alpha * gi[j]) / sumx) , sum(boolcl))
        testpip [[i]] <- pip
        tests [[i]] <- si
        cltest [[i]] <- cl
        bltest [[i]] <- boolcl
      }
    }
    p[which(boolcan !=0)] <- pip
  }
  u <- data.matrix(runif(n))
  incompl <- ifelse(u<= p, 1, 0)
  testincompl <- cbind (u, p, incompl)
  cand <- cand * incompl
  testcand2 <- (cand)
  resp <- diag(0, n, m)
  bool <- cand == 0
  bool <- ifelse(bool == T, 1, 0)
  resp[which(bool !=0), c(1:m)] <- matrix(1, sum(bool), m)
  bool <- cand > 0                # vervanging voor loop
  bool <- ifelse(bool == T, 1, 0) # vervanging voor loop
  if ( any(bool == 1) )	{resp[which(bool !=0), c(1:m)] <- matrix((pattern[cand,]), byrow = T)}  # vervanging voor loop
  testresp <- (apply (resp, 1, prod))
  xobs <- ifelse(resp==1, x , NA)
  xobs
 # list (xobs=xobs, testcand1=testcand1, testincompl=testincompl, testcand2=testcand2, testresp=testresp, testpip=testpip, tests=tests, fltest=fltest, cltest=cltest, bltest=bltest)
}


