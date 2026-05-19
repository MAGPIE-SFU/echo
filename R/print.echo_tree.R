#' @export
print.echo_tree <- function(x, ...) {
  cat("<echo_tree>\n")
  cat("Edges:", nrow(x$edge), "\n")
  cat("Root:", x$root, "\n")
  invisible(x)
}