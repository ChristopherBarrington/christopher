#' Add metric suffixes to numbers
#' 
#' Convert numeric values into characters with metric suffixes.
#' 
#' @param i Number to convert (passed through \code{as.numeric})
#' @param sep Separator between number and suffix
#' 
#' @return
#' Character vector of equal size to \code{i}.
#' 
#' @export
#' 
numeric_to_si <- function(i, sep='') {
  sifactor <- c(1e-24, 1e-21, 1e-18, 1e-15, 1e-12, 1e-9, 1e-6, 1e-3, 1, 1e3, 1e6, 1e9, 1e12, 1e15, 1e18, 1e21, 1e24)
  pre <- c('y', 'z', 'a', 'f', 'p', 'n', 'u', 'm', '', 'k', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y')

  i <- as.numeric(i)
  absolutenumber <- i * sign(i)
  ix <- findInterval(absolutenumber, sifactor)
  ix[ix==0] <- which(pre=='')
  if(length(ix) > 0) {
    sistring <- sprintf('%s%s%s', i/sifactor[ix], sep, pre[ix])
    sistring <- str_remove(sistring, pattern=sprintf('%s$', sep))
  }
  else {
      sistring <- as.character(i)
  }
  return(sistring)
}

#' Split a data frame into a nested list
#' 
#' Convert a set of data frame variables into keys of a nested list
#' 
#' @param x `data.frame` to split
#' @param variables An ordered character vector of variables in `x` that will be nested levels of the output list
#' @param f A function applied to the data frame, once all variables have been used
#' @param ... Arguments passed to `f`
#' 
#' @details
#' _May_ assume that every combination of `variables` produces a data frame to which `f` can be applied.
#' 
#' @return
#' A nested list
#' 
#' @importFrom magrittr %<>%
#' @importFrom plyr dlply
#' @importFrom purrr map_depth
#' 
#' @export
#' 
iteratively_split_df <- function(x, variables={colnames(x) |> head(n=-1)}, f, ...) {
  i <- 1
  x %<>% dlply(variables[i])
  
  if(length(variables) > 1)
    for(i in seq_along(variables)[-1])
      x %<>% map_depth(.depth=i-1, dlply, variables[i])

  if(!missing(f))
    x %<>% map_depth(.depth=i, f, ...)

  invisible(x)
}
