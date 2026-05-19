#' Save ECHO output to a CSV file
#' 
#' @param obj An object of class `echo`.
#' @param filename String indicating the name of the file to be saved.
#' If only a file name is specified it will save to the current directory.
#' 
#' @export
export_echo <- function(obj, filename){
  if(inherits(obj, "echo"))
    stop("Object must be of class echo.")
  
  if(!is.character(filename))
    stop("filename must be a string.")
  
  obj_df <- extract_df(obj)
  
  utils::write.csv(obj, file=filename)
}