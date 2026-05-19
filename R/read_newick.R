#' @export
read_newick <- function(filename) {
  
  if (file.exists(filename)) {
    text <- paste(readLines(filename, warn = FALSE), collapse = "")
  }
  
  text <- gsub("\r", "", text)
  text <- gsub("\\s+", "", text)
  text <- sub(";$", "", text)
  
  tokenize_newick <- function(text) {
    pattern <- "(\\()|(\\))|(,)|(:)|([0-9]+\\.?[0-9eE\\-]*)|([^\\(\\),:]+)"
    m <- gregexpr(pattern, text, perl = TRUE)
    regmatches(text, m)[[1]]
  }
  
  tokens <- tokenize_newick(text)
  
  new_internal <- local({
    i <- 0
    function() {
      i <<- i + 1
      list(
        name = paste0("N", i),
        children = list(),
        length = NA_real_
      )
    }
  })
  
  stack <- list()
  last_node <- NULL
  root <- NULL
  i <- 1
  
  while (i <= length(tokens)) {
    
    tok <- tokens[i]
    
    if (tok == "(") {
      
      stack[[length(stack) + 1]] <- new_internal()
      
    } else if (tok == ")") {
      
      node <- stack[[length(stack)]]
      stack[[length(stack)]] <- NULL
      
      last_node <- node
      
      if (length(stack) > 0) {
        stack[[length(stack)]]$children <-
          c(stack[[length(stack)]]$children, list(node))
      } else {
        root <- node
      }
      
    } else if (tok == ",") {
      
      # separator only
      
    } else if (tok == ":") {
      
      i <- i + 1
      len <- as.numeric(tokens[i])
      
      if (length(stack) > 0) {
        children <- stack[[length(stack)]]$children
        children[[length(children)]]$length <- len
        stack[[length(stack)]]$children <- children
      } else if (!is.null(last_node)) {
        last_node$length <- len
      }
      
    } else {
      
      # token is a label
      node <- list(
        name = tok,
        children = list(),
        length = NA_real_
      )
      
      last_node <- node
      
      if (length(stack) > 0) {
        stack[[length(stack)]]$children <-
          c(stack[[length(stack)]]$children, list(node))
      } else {
        root <- node
      }
    }
    
    i <- i + 1
  }
  
  edges <- data.frame(
    parent = character(),
    child = character(),
    length = numeric(),
    stringsAsFactors = FALSE
  )
  
  flatten <- function(node, parent = NULL) {
    
    if (!is.null(parent)) {
      edges <<- rbind(
        edges,
        data.frame(
          parent = parent,
          child = node$name,
          length = node$length,
          stringsAsFactors = FALSE
        )
      )
    }
    
    for (child in node$children) {
      flatten(child, node$name)
    }
  }
  
  flatten(root)
  
  echo_tree(
    edge = edges,
    node_meta = NULL,
    root = root$name
  )
}