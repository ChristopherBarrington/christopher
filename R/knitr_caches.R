#' Load a cached knitr chunk
#' 
#' Loads the cached chunk from cache using lazy loading where possible.
#'
#' @param chunk Name of cached chunk (from Rmd). Can be a path to a chunk (partial), from which `path` will be taken.
#' @param cache Root path to cache(s) that will be searched for `chunk`
#' @param filter Character vector of object names to retain
#' @param env Environment into which objects are loaded
#'
#' @describeIn load_cached_chunk Load a cached chunk
#' 
#' @importFrom magrittr %T>%
#' @importFrom purrr when
#' @importFrom stringr str_detect str_remove
#' 
#' @export
#' 
load_cached_chunk <- function(chunk='unnamed-chunk', cache=c('knitr_cache', 'cache'), filter, env=globalenv()) {
  # get the potential chunks from the cache
  list.files(path=cache, pattern=chunk, full.names=TRUE) -> chunks_list

  # check for the number of files for the chunk
  chunks_list %>%
    when(length(.)==0~stop('[load_cached_chunk] No cached chunks found for: ', chunk, call.=FALSE),
         length(.)==1 && str_detect(., '\\.(RData)$')~'load',
         length(.)==3 && {str_detect(., '\\.(RData|rdb|rdx)$') %>% all()}~'lazy', # this may not work as expected
         length(.)>3~stop('[load_cached_chunk] Too many potential chunks for: ', chunk, call.=FALSE),
         TRUE~stop('[load_cached_chunk] Cannot load (may be incomplete?) chunk: ', chunk, call.=FALSE)) -> cache_type

  # load the chunk
  if(cache_type=='load')
    chunks_list %>%
      head(n=1) %T>%
      load(envir=env) %>%
      remove_extension(filename_only=FALSE) -> chunk_file

  # lazily load the chunk
  if(cache_type=='lazy') {
    chunks_list %>%
      head(n=1) %>%
      remove_extension(filename_only=FALSE) -> chunk_file

    filter_func <- function(x) rep(TRUE, length(x))
    if(!missing(filter))
      filter_func <- function(x) x %in% filter

    lazyLoad(chunk_file, envir=env, filter=filter_func)
  }

  # return the path of the loaded chunk
  invisible(chunk_file)
}

#' Lazy load all chunks in a cache
#' 
#' @param cache Path to the cache from which all chunks are loaded
#' 
#' @details
#' Any existing objects in the environment will be overwritten when chunk(s) are loaded. 
#' 
#' @describeIn load_cached_chunk Load all cached chunks (alphabetically)
#' 
#' importFrom plyr l_ply
#' 
#' @export
#' 
load_all_chunks <- function(cache=c('knitr_cache', 'cache'))
  list.files(cache, pattern='*RData') %>%
    str_remove('.RData') %>%
    l_ply(load_cached_chunk, cache=cache)

