#' Make a ranked choice
#' 
#' Select a vector element using a ranked set of preferences.
#' 
#' @param x Named vector of possible choices
#' @param preferences Ranked vector of names of \code{x}
#' @param default Element of \code{x} if none of \code{preferences} are names of \code{x}
#' @param missing Value returned instead of \code{x[default]}, when `default=0` or none of \code{preferences} or \code{default} exist in \code{names(x)}
#' 
#' @export
#' 
preferred_choice <- function(x, preferences, default=1, missing=NA) {
  if(length(x)==0)
    stop('no elements from which to choose!')
  
  # if default is a number and could be in x or default is a character with at least one match with x's names
  if((is.numeric(default) && between(default, 1, length(x))) | (is.character(default) && {is.element(default, names(x)) %>% any()})) {
    # if no `preferences` are in `x`, use the [default] element of `x`
    preferences %<>% append(pluck(x, default))
    
    # return the first match in ranked `preferences` that is in `x`
    x %>%
      str_c(collapse='|') %>%
      sprintf(fmt='^(%s)$') %>%
      str_subset(string=preferences) %>%
      head(n=1) %>%
      return()
  } else {
    return(missing)
  }
}
