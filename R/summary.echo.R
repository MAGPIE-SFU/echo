#' @rdname echo
#' @param CI Level of the credible intervals. Only applicable if `n_trees>1`.
#' @export
summary.echo <- function(x, CI=0.95, ...) {
  if(x$n_trees == 1) {
    tab <- data.frame(
      Estimator = c("ECHO-A", "ECHO-B", "ECHO-C"),
      `Total cases` = c(sprintf("%.2f (%.2f, %.2f)", x$A$point, x$A$CI[1], x$A$CI[2]),
                        sprintf("%.2f (%.2f, %.2f)", x$B$point, x$B$CI[1], x$B$CI[2]),
                        sprintf("%.2f (%.2f, %.2f)", x$C$point, x$C$CI[1], x$C$CI[2])),
      `Missing-in-Tree cases`= c(sprintf("%.2f (%.2f, %.2f)", x$A$point-x$n_samples, x$A$CI[1]-x$n_samples, x$A$CI[2]-x$n_samples),
                                 sprintf("%.2f (%.2f, %.2f)", x$B$point-x$n_samples, x$B$CI[1]-x$n_samples, x$B$CI[2]-x$n_samples),
                                 "N/A"),
      `Cryptic cases` = c("N/A",
                          "N/A",
                          sprintf("%.2f (%.2f, %.2f)", x$B$point-x$n_samples, x$B$CI[1]-x$n_samples, x$B$CI[2]-x$n_samples)),
      check.names = F,
      stringsAsFactors=F
    )
    # tab[-1] <- lapply(tab[-1], function(x) sprintf("%.2f", x))
  } else {
    tab <- data.frame(
      Estimator = c("A", "B", "C"),
      `Point Estimate` = c(format_interval(x$A$point, alpha=1-CI), 
                           format_interval(x$B$point, alpha=1-CI), 
                           format_interval(x$C$point, alpha=1-CI)),
      `Lower CI` = c(format_interval(x$A$CI[1,], alpha=1-CI), 
                     format_interval(x$B$CI[1,], alpha=1-CI), 
                     format_interval(x$C$CI[1,], alpha=1-CI)),
      `Upper CI` = c(format_interval(x$A$CI[2,], alpha=1-CI), 
                     format_interval(x$B$CI[2,], alpha=1-CI), 
                     format_interval(x$C$CI[2,], alpha=1-CI)),
      check.names = F,
      stringsAsFactors=F
    )
  }
  
  
  # PRINT SUMMARY
  widths <- sapply(seq_along(tab), function(i) {
    max(nchar(c(names(tab)[i], as.character(tab[[i]])))) + 2
  })
  cat("\nECHO Summary\n\n")
  
  cat("Run with:\n ", x$n_trees, "trees\n ", x$n_samples, "samples.\n\n")
  
  cat(
    format(names(tab)[1], width = widths[1], justify = "left"),
    format(names(tab)[2], width = widths[2], justify = "centre"),
    format(names(tab)[3], width = widths[3], justify = "centre"),
    format(names(tab)[4], width = widths[4], justify = "centre"),
    "\n",
    sep = ""
  )
  
  cat(strrep("-", sum(widths)), "\n", sep = "")
  
  # Rows
  for (i in seq_len(nrow(tab))) {
    cat(
      format(tab[i, 1], width = widths[1], justify = "centre"),
      format(tab[i, 2], width = widths[2], justify = "right"),
      format(tab[i, 3], width = widths[3], justify = "right"),
      format(tab[i, 4], width = widths[4], justify = "right"),
      "\n",
      sep = ""
    )
  }
  
  cat(strrep("-", sum(widths)), "\n", sep = "")
  
  invisible(tab)
  
  # class(tab) <- c("summary.echo", class(tab))
  # return(tab)
}


# print.summary.echo <- function(x, ...) {
#   widths <- sapply(seq_along(x), function(i) {
#     max(nchar(c(names(x)[i], as.character(x[[i]])))) + 2
#   })
#   cat("\nECHO Summary\n\n")
# 
#   cat("Run with:\n ", x)
# 
#   cat(
#     format(names(x)[1], width = widths[1], justify = "left"),
#     format(names(x)[2], width = widths[2], justify = "centre"),
#     format(names(x)[3], width = widths[3], justify = "centre"),
#     format(names(x)[4], width = widths[4], justify = "centre"),
#     "\n",
#     sep = ""
#   )
# 
#   cat(strrep("-", sum(widths)), "\n", sep = "")
# 
#   # Rows
#   for (i in seq_len(nrow(x))) {
#     cat(
#       format(x[i, 1], width = widths[1], justify = "centre"),
#       format(x[i, 2], width = widths[2], justify = "right"),
#       format(x[i, 3], width = widths[3], justify = "right"),
#       format(x[i, 4], width = widths[4], justify = "right"),
#       "\n",
#       sep = ""
#     )
#   }
# 
#   cat(strrep("-", sum(widths)), "\n", sep = "")
# 
#   invisible(x)
# }

format_interval <- function(x, alpha) {
  med <- quantile(x, probs=0.5, na.rm=T)
  L <- quantile(x, probs=alpha/2, na.rm=T)
  U <- quantile(x, probs=1-alpha/2, na.rm=T)
  sprintf("%.2f (%.2f, %.2f)", med, L, U)
}

# format_confidence_interval <- function(x, l, u) {
#   sprintf
# }