#' Make tabs shortcode
#' 
#' Write a shortcode to create tabs from a (nested) list.
#' 
#' @param content List (of lists) containing content to include in a tab
#' @param tabsets List (of lists) containing keys of the `content` list (not of lists)
#' @param chunkname Character name of the output tabset
#' @param output_function Function used to print/plot/etc
#' @param target_classes Set of classes that cause `output_function` to run
#' @param depth Number of elements to traverse before `output_function` is run
#' 
#' @details
#' It would be better to not use `tabsets` and convert a list's indexes into a nested list structure.
#' 
#' @return
#' A character vector for the shortcode to include in the markdown.
#' 
#' @export
#'
make_tabs_shortcode <- function(content, tabsets=content, chunkname, output_function=show, target_classes=c('ggplot', 'kable'), depth) {
  if(missing(chunkname))
    chunkname <- opts_current$get('label')

  if(missing(depth))
    depth <- -1

  make_tab <- function(tabsets, tabset_name, tab_name, to_target_depth) {
    sprintf(fmt='{{< tab "%s" >}}', tab_name) |> cat(sep='\n')
    if(to_target_depth==0) {
      output_function(tabsets)
    } else if(target_classes %in% class(tabsets) |> any()) {
      output_function(tabsets)
    } else if(is.character(tabsets)) {
      content |> pluck(tabsets) |> output_function()
    } else {
      make_tabset(tabsets=tabsets, tabset_name=str_c(tabset_name, tab_name, sep=':'), to_target_depth=to_target_depth)
    }
    cat('{{< /tab >}}', sep='\n')
  }
  
  make_tabset <- function(tabsets, tabset_name, to_target_depth) {
    sprintf(fmt='{{< tabs "%s" >}}', tabset_name) |> cat(sep='\n')
    to_target_depth %<>% subtract(1)
    Map(tabsets=tabsets, tabset_name=tabset_name, tab_name=names(tabsets), to_target_depth=to_target_depth, make_tab)
    cat('{{< /tabs >}}', sep='\n')
  }
  
  make_tabset(tabsets=tabsets, tabset_name=chunkname, to_target_depth=depth)
}
