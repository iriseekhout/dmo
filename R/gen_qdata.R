#' Generate questionnaire data
#' A function to generate questionnaire data with normally distributed item score with a mean of 0.
#'
#' @param n sample size
#' @param nq number of questionnaires
#' @param k vector of length nq indicating number of items per questionnaire
#' @param likert continuous scores transformed to likert item scores
#' 
#' @return data.frame with `n` rows and `nq * k` columns
#' @export
gen_qdata <- function(n, nq ,k , likert=FALSE){
  if(length(k) != nq) stop("k is not the same length as number of questionnaires")
  sig <- matrix(1,nq*k,nq*k)
  diag(sig) <- 5
  x <- MASS::mvrnorm(n=100,mu=rep(0, (sum(k))), Sigma=sig)

  if(likert==TRUE){
    x <- ifelse(x > 0.841621234, 5, x)
    x <- ifelse(x > 0.253347103 & x <= 0.841621234 ,4,x)
    x <- ifelse(x > -0.253347103  & x<= 0.253347103,3,x)
    x <- ifelse(x > -0.841621234& x<=-0.253347103,2,x)
    x <- ifelse(x <= -0.841621234,1, x)
  }
  data.frame(x)
}
