#' Calculate total scores
#'
#' @param x data.frame with item scores that are to be summarized into a
#' total score. 
#' @param nq number of questionnaires
#' @param k number of items per questionnaire
#'
#' @return data.frame with `ncol = nq` and `nrow = nrow(x)`.
#' @export
#'
#' @examples
#' x <- gen_qdata(n=100, nq=5, k= c(10,10,10,10,10))
#' calculate_ts(x, 5, c(10,10,10,10,10), sum)
#' calculate_ts(x, 5, c(10,10,10,10,10), mean)
calculate_ts <- function(x, nq, k, .fun = sum){
  TStot <- matrix(0,nrow=nrow(x), ncol=(nq))

  for (q in 1:(nq)){
    TStot[,q] <- apply(x[,(((q*k[q])-k[q])+1):(q*k[q])],1, .fun)
  }
  colnames(TStot) <- paste("TS",1:nq,sep="")
  TStot
}

