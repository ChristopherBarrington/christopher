#' Set default options for a kable
#' 
#' Uses options that I always have to type manually.
#' 
#' @param ... Options passed to \code{kableExtra::kable}
#' 
#' @importFrom kableExtra kable
#' 
#' @export
#' 
kable <- function(...) {
  opts <- list(...)
  defaults <- list(format=ifelse(interactive(), 'markdown', 'html'), format.args=list(big.mark=','))

  modifyList(x=opts, val=defaults) %>%
    do.call(what=kableExtra::kable)
}
