#' Set default options for a kable
#' 
#' Uses options that I always have to type manually.
#' 
#' @param ... Options passed to \code{knitr::kable}
#' 
#' @importFrom knitr kable
#' 
#' @export
#' 
kable <- function(...) {
  opts <- list(...)
  defaults <- list(format=ifelse(interactive(), 'pipe', 'html'), format.args=list(big.mark=','))

  modifyList(x=defaults, val=opts) %>%
    do.call(what=knitr::kable)
}
