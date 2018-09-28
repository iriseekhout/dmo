#' Calculate total scores
#'
#' @param x data.frame
#' @param nq number of questionnaires
#' @param k number of items per questionnaire
#'
#' @return
#' @export
#'
#' @examples
calculate_ts <- function(x,nq,k){
  TStot <- matrix(0,nrow=nrow(x), ncol=(nq))

  for (q in 1:(nq)){
    TStot[,q] <- apply(x[,(((q*k[q])-k[q])+1):(q*k[q])],1,sum)
  }
  colnames(TStot) <- paste("TS",1:nq,sep="")
  TStot
}
