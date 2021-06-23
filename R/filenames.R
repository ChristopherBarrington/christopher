#' Get the extension of a file
#' 
#' @param path At least a file name, potentially the full path
#' 
#' @importFrom stringr str_remove
#' 
#' @export
#' 
get_file_extension <- function(path)
  str_remove(string=path, pattern='.*\\.')
