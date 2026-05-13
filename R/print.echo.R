#' @rdname echo
#' @export
print.echo <- function(x, ...) {
  # cat("Run with:\n")
  # cat(" ", x$n_trees, "trees\n")
  # cat(" ", x$n_samples, "samples\n")
  
  if(x$n_trees == 1) {
    cat(paste0("ECHO-A: ", round(x$A$point, 2), " (", round(x$A$CI[1], 2),", ",round(x$A$CI[2]),") cases (observed and missing-in-tree)\n"))
    cat(paste0("ECHO-B: ", round(x$B$point, 2), " (", round(x$B$CI[1], 2),", ",round(x$B$CI[2]),") cases (observed and missing-in-tree)\n"))
    cat(paste0("ECHO-C: ", round(x$C$point, 2), " (", round(x$C$CI[1], 2),", ",round(x$C$CI[2]),") cases (observed, missing-in-tree, and missing-off-tree)\n"))
  } else {
    A_med <- round(quantile(x$A$point, probs=0.5), 2)
    B_med <- round(quantile(x$B$point, probs=0.5), 2)
    C_med <- round(quantile(x$C$point, probs=0.5), 2)
    
    A_LB <- round(quantile(x$A$CI[1,], probs=0.5), 2)
    B_LB <- round(quantile(x$B$CI[1,], probs=0.5), 2)
    C_LB <- round(quantile(x$C$CI[1,], probs=0.5), 2)
    
    A_UB <- round(quantile(x$A$CI[2,], probs=0.5), 2)
    B_UB <- round(quantile(x$B$CI[2,], probs=0.5), 2)
    C_UB <- round(quantile(x$C$CI[2,], probs=0.5), 2)
    
    cat(paste0("ECHO-A: ", A_med, " (", A_LB, ", ",A_UB, ") cases (median observed and missing-in-tree)\n"))
    cat(paste0("ECHO-B: ", B_med, " (", B_LB, ", ",B_UB, ") cases (median observed and missing-in-tree)\n"))
    cat(paste0("ECHO-C: ", C_med, " (", C_LB, ", ",C_UB, ") cases (median observed, missing-in-tree, and missing-off-tree)\n"))
  }
}