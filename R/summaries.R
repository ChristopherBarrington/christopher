#' Get the top and bottom of a \code{data.frame}
#' 
#' Uses \code{rbind} to return the \code{head} and \code{tail} of a sufficiently large \code{data.frame}.
#' 
#' @param x A \code{data.frame}
#' @param n,n.head,n.tail Number of rows to head and tail
#' 
#' @return
#' A \code{data.frame} of \code{n.head} and \code{n.tail} rows. If `nrow(x)` would return overlapped rows, `x` is returned.
#' 
#' @export
#' 
headtail <- function(x, n=10, n.head=n, n.tail=n) {
  if({x %>% when(is.vector(.)~length(.), is.data.frame(.)~nrow(.), TRUE~nrow(.)) %>% is_weakly_less_than(n.head+n.tail)}) {
    x
  } else {
    rbind(head(x=x, n=n.head), tail(x=x, n=n.tail))
  }
}

#' Get the size of an object
#' 
#' Prints the size in \code{units} of an object(s).
#' 
#' @param x Object to check
#' @param unit Units to print
#' 
#' @seealso utils:::format.object_size
#' 
#' @export
#' 
print_object_size <- function(x, unit='GB')
  utils::object.size(x=x) %>% format(unit=unit[1])
