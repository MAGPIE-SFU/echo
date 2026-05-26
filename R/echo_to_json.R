#' @export
echo_to_json <- function(x, indent = 0) {
  pad <- function(n) paste(rep(" ", n), collapse = "")
  
  # atomic NULL
  if (is.null(x)) return("null")
  
  # logical
  if (is.logical(x)) {
    if (length(x) == 1) return(if (is.na(x)) "null" else tolower(as.character(x)))
    vals <- vapply(x, echo_to_json, character(1))
    return(paste0("[", paste(vals, collapse = ", "), "]"))
  }
  
  # numeric / integer
  if (is.numeric(x) || is.integer(x)) {
    if (length(x) == 1) return(if (is.na(x)) "null" else as.character(x))
    vals <- vapply(x, function(v) if (is.na(v)) "null" else as.character(v), character(1))
    return(paste0("[", paste(vals, collapse = ", "), "]"))
  }
  
  # character
  if (is.character(x)) {
    esc <- function(s) {
      s <- gsub("\\\\", "\\\\\\\\", s)
      s <- gsub('"', '\\"', s)
      paste0('"', s, '"')
    }
    if (length(x) == 1) return(if (is.na(x)) "null" else esc(x))
    vals <- vapply(x, esc, character(1))
    return(paste0("[", paste(vals, collapse = ", "), "]"))
  }
  
  # named or unnamed list
  if (is.list(x)) {
    is_named <- !is.null(names(x)) && any(names(x) != "")
    
    if (!is_named) {
      vals <- vapply(x, function(v) echo_to_json(v, indent + 2), character(1))
      return(paste0("[", paste(vals, collapse = ", "), "]"))
    }
    
    items <- vapply(seq_along(x), function(i) {
      key <- names(x)[i]
      if (is.null(key) || key == "") key <- paste0("V", i)
      val <- echo_to_json(x[[i]], indent + 2)
      paste0('"', key, '": ', val)
    }, character(1))
    
    if (indent == 0) {
      return(paste0("{", paste(items, collapse = ", "), "}"))
    } else {
      return(paste0("{", paste(items, collapse = ", "), "}"))
    }
  }
  
  # fallback
  paste0('"', as.character(x), '"')
}