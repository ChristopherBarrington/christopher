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

#' Create and/or clear out a directory
#' 
#' @param path Path to directory to create and/or clean
#' @param clean Should the contents of `path` be deleted? Defaults to `FALSE` for safety.
#' 
#' @return
#' Invisibly returns the `path` that may have been created and/or cleaned.
#' 
#' @export
#' 
make_clean_directory <- function(path, clean=FALSE) {
  # check that there is something to do
  if(missing(path))
    stop('no path was specified!', call.=FALSE)

  if(!is.logical(clean))
    stop('clean must be either TRUE or FALSE')

  # create the directory, even if it exists don't moan about it
  dir.create(path=path, recursive=TRUE, showWarnings=FALSE)

  # if cleaning is needed, do it
  if(clean)
    sprintf('rm -r %s/*', path) %>% system()

  invisible(path)
}
