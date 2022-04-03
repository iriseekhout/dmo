#' Generate questionnaire data
#' A function to generate questionnaire data with normally distributed item score with a mean of 0.
#'
#' @param n sample size
#' @param k vector number of items per questionnaire
#' @param likert continuous scores transformed to likert item scores
#' 
#' @return data.frame with `n` rows and `nq * k` columns
#' @export
gen_qdata <- function(n, k , likert=FALSE){

  quest <- list()
  for(q in seq_along(k)){
    sig <- matrix(1, nrow = k[q], ncol = k[q])
    diag(sig) <- 5
    x <- MASS::mvrnorm(n=n,mu=rep(0, k[q]), Sigma=sig)
    
    if(likert==TRUE){
      x <- ifelse(x > 0.841621234, 5, x)
      x <- ifelse(x > 0.253347103 & x <= 0.841621234 ,4,x)
      x <- ifelse(x > -0.253347103  & x<= 0.253347103,3,x)
      x <- ifelse(x > -0.841621234& x<=-0.253347103,2,x)
      x <- ifelse(x <= -0.841621234,1, x)
    }
    colnames(x) <- paste("Q", q, "_i", 1:k[q], sep = "")
    quest[[q]] <- x
  }
  
  
  data.frame(do.call('cbind', quest))
}
