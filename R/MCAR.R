## written by Iris Eekhout mrt 2012 (www.iriseekhout.com)
## translated from JPL Brand et al (Brand, J. P. L., van Buuren, S., Groothuis-Oudshoorn, K., & Gelsema, E. S. (2003). A toolkit in SAS for the evaluation of multiple imputation methods. Statistica Neerlandica, 57(1), 36-45).

#' Generate MCAR (missing completely at random) data
#'
#' @param x data frame where missing observations should be generated in.
#' @param alpha proportion of cases that will get a missing data pattern
#' @param pattern a matrix with ncol=ncol(data), nrow = number of missing data 
#' patterns; for each patter 0 indicates missing and 1 observed. 
#' `pattern = "random"` will generate `npattern` random patterns
#' @param npattern the number of patterns when patterns are randomly generated.
#' @param f frequency of each pattern
#'
#' @return A data frame that contains missing observations
#' @export
#' 
#' @importFrom stats runif
#'
#' @examples
#' x <- MASS::mvrnorm(n=100,mu=c(0,0,0), Sigma=matrix(c(5,1,1,1,5,1,1,1,5),3,3))
#' alpha <- 0.25
#' pattern <- matrix(c(1,1,0,1,0,1),2,3, byrow=TRUE)
#' f <- c(0.5,0.5)
#' MCAR(x,alpha,pattern,f)
MCAR <- function(x, 
                 alpha, 
                 pattern = "random",
                 npattern = 1,
                 f = rep(1/nrow(pattern)))
{
  xobs <- testcand1 <- testincompl <- testcand2 <- testresp <- list()
  
  
  if(is.character(pattern) & pattern[1] == "random"){
    pattern <- matrix(c(sample(c(0,1), size=ncol(x)*npattern,replace = TRUE)),nrow=npattern)
    
  }
  
  if(is.data.frame(pattern) | is.vector(pattern) | is.matrix(pattern)){
    if(ncol(pattern) == ncol(x)) {
      pattern <- as.matrix(pattern)}
    if(ncol(pattern) != ncol(x)) {
      stop("The length of the patterns is not equal to ncol(x).")}
  }
  
  if(!is.matrix(pattern)){
    warning("The defined pattern is not a matrix, vector or data.frame. A random pattern is generated")
    pattern <- "random"
  }
  
  
  orig <- x
  x <- data.matrix(x)
  n <- NROW(x)
  m <- NCOL(x)
  sf <- sum (f)
  f <- data.matrix (f/sf)
  u <- runif(n)
  res <- outer(u, cumsum(f), ">")   # vervanging voor loop
  cand <- rowSums(res) + 1          # vervanging voor loop
  testcand1 <- cbind(u, cand)
  u <- data.matrix(runif(n))
  incompl <- ifelse(u<=alpha, 1, 0)
  testincompl <- cbind (u, incompl)
  cand <- cand * incompl
  testcand2 <- (cand)
  resp <- diag(0, n, m)
  bool <- cand == 0
  bool <- ifelse(bool == T, 1, 0)
  resp[which(bool !=0), c(1:m)] <- matrix(1, sum(bool), m)
  bool <- cand > 0
  bool <- ifelse(bool == T, 1, 0)
  if ( any(bool == 1) )  {resp[which(bool !=0), c(1:m)] <- matrix((pattern[cand,]), byrow = TRUE)}
  testresp <- (apply (resp, 1, prod))
  xobs <- ifelse(resp==1, x , NA)
  if(is.data.frame(orig)) xobs <- data.frame(xobs)
  xobs
  #list (xobs=xobs, testcand1=testcand1, testincompl=testincompl, testcand2=testcand2, testresp=testresp)
}

