#' Calculate total scores
#'
#' @param x data.frame with item scores that are to be summarized into a
#' total score. 
#' @param k number of items per questionnaire
#' @param .fun function used to summarise the item scores
#'
#' @return data.frame with `ncol = nq` and `nrow = nrow(x)`.
#' @export
#'
#' @examples
#' x <- gen_qdata(n=100, k= c(10,10,10,10,10))
#' calculate_ts(x, c(10,10,10,10,10), sum)
#' calculate_ts(x, c(10,10,10,10,10), mean)
calculate_ts <- function(x, k, .fun = sum){
  TStot <- matrix(0,nrow=nrow(x), ncol=length(k))

  csumk <- c(0,cumsum(k))
  
  for(q in seq_along(k)){
    TStot[,q] <- apply(x[, (csumk[q]+1):csumk[q+1]], 1, .fun)
    
  }
  colnames(TStot) <- paste("TSQ",1:length(k),sep="")
  TStot
}

