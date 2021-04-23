#' Get breaks for a log scale
#' 
#' Provides breaks for major and minor log-transformed scales.
#' 
#' @param base Log base
#' @param max.exp,min.exp range of exponents to return
#' 
#' @describeIn log_breaks Log breaks
#' @family log breaks
#' 
#' @examples
#' log_breaks(base=10, max.exp=2)
#' 
#' @export
#' 
log_breaks <- function(base=10, max.exp=7, min.exp=0)
  major_log_breaks(base=base, max.exp=max.exp, min.exp=min.exp) %>%
    c(minor_log_breaks(base=base, max.exp=max.exp, min.exp=min.exp)) %>%
    sort()

#'
#' @describeIn log_breaks Major log breaks
#' @family log breaks
#' 
#' @examples
#' major_log_breaks(base=10, max.exp=2)
#' 
#' @export
#' 
major_log_breaks <- function(base=10, max.exp=7, min.exp=0)
  base^c(min.exp:max.exp)

#' 
#' @describeIn log_breaks Minor log breaks
#' @family log breaks
#' 
#' @examples
#' minor_log_breaks(base=10, max.exp=2)
#' 
#' @export
#' 
minor_log_breaks <- function(base=10, max.exp=7, min.exp=0) {
  majors <- major_log_breaks(base=base, max.exp=max.exp, min.exp=min.exp) %>% head(n=-1)
  majors %>%
    sapply(function(x) seq(from=x, length.out=base-1, by=x)) %>%
    as.vector() %>%
    (function(x) x[!x %in% majors])
}

