#' @export
echo_tree <- function(edge, node_meta = NULL, root = NULL) {
  stopifnot(is.data.frame(edge))
  stopifnot(all(c("parent", "child") %in% names(edge)))
  
  structure(
    list(
      edge = edge,
      node_meta = node_meta,
      root = root
    ),
    class = "echo_tree"
  )
}