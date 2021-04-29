#' Lazy load all chunks in a cache
#' 
#' @param cache Path to the cache from which all chunks are lazily loaded
#' 
#' @export
#' 
load_all_chunks <- function(cache='cache')
  list.files(cache, pattern='*RData') %>%
    str_remove('.RData') %>%
    plyr::l_ply(lazyload_cached_chunk, path=cache)

#' Lazy load a knitr chunk
#'
#' @param chunk Name of cached chunk (from Rmd). Can be a path to a chunk (partial), from which `path` will be taken,
#' @param filter Character vector of object names to retain
#' @param path Root path within which caches (`*rdx`) are searched
#' @param env Environment into which objects are loaded
#'
#' @export
#' 
lazyload_cached_chunk <- function(chunk='unnamed-chunk', filter, path=dirname(chunk), env=globalenv()) {
    chunk <- basename(chunk)
    chunk_file <- list.files(path=path, pattern=sprintf('%s.*.rdx', chunk), full.names=TRUE, recursive=TRUE)
    chunk_file <- stringr::str_remove(chunk_file, pattern='.rdx')

   if (length(chunk_file) > 1)
      stop('Cannot load chunk from:\n', paste(chunk_file, collapse='\n'))
    if (length(chunk_file) == 0)
      stop('No chunks called `', chunk, '` found!')

    filter_func <- function(x) rep(TRUE, length(x))
    if (!missing(filter))
      filter_func <- function(x) x %in% filter

    lazyLoad(chunk_file, envir=env, filter=filter_func)
    invisible(chunk_file)
}
