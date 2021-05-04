#' Make tabs shortcode
#' 
#' Write a shortcode to create tabs from a (nested) list.
#' 
#' @param x List (of lists) from which tabs will be created
#' 
#' @return
#' A character vector for the shortcode to include in the markdown.
#' 
make_tabs_shortcode <- function(x, tabset_name='chunk_name') {
  # {{< tabs "uniqueid" >}}
  # {{< tab "MacOS" >}} # MacOS Content {{< /tab >}}
  # {{< tab "Linux" >}} # Linux Content {{< /tab >}}
  # {{< tab "Windows" >}} # Windows Content {{< /tab >}}
  # {{< /tabs >}}
  stop()
  chunk_name %>% sprintf(fmt='{{< tabs "%s" >}}') -> lines

  lines %>% c('{{< /tabs >}}') %>% str_c(collapse='\n')
}
