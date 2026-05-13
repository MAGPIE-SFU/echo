#' Extract point estimates from ECHO object
#' 
#' Takes ECHO output and returns a data.frame with the point estimates in an easily manipulable format.
#' 
#' @param x An object of class `echo`.
#' 
#' @returns A `data.frame` with the following columns: 
#' - `estimator`: Indicates which of ECHO-A, -B, or -C the row corresponds to
#' - `point`: The point estimate
#' - `LB`: The lower bound on the confidence interval
#' - `UB`: The upper bound on the confidence interval
#' 
#' @export
extract_df <- function(x) {
  if(x$n_trees == 1) {
    return(data.frame(estimator = c("A", "B", "C"),
                      point = c(x$A$point, x$B$point, x$C$point),
                      LB = c(x$A$CI[1], x$B$CI[1], x$C$CI[1]),
                      UB = c(x$A$CI[2], x$B$CI[2], x$C$CI[2])))
  } else{
    return(data.frame(estimator = rep(c("A", "B", "C"), each=x$n_trees),
                      point = c(x$A$point, x$B$point, x$C$point),
                      LB = c(x$A$CI[1,], x$B$CI[1,], x$C$CI[1,]),
                      UB = c(x$A$CI[2,], x$B$CI[2,], x$C$CI[2,])))
  }
}