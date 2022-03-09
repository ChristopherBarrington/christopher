
#' Select `DESeqDataSet` objects from a list
#' 
#' Filter a list of `DESeqDataSet` objects by the type of test that was run. Test type is determined by the content of the metadata of the object; existence of a `reduced` element indicates the test was `LRT`.
#'
#' @param x A list of `DESeqDataSet` objects
#' @param type Either 'Wald' or 'LRT' to indicate which should be kept
#' 
#' @return
#' A list of `DESeqDataSet` objects; if x is a `DESeqDataSet` object (not a list) then a list is still returned of the one object, named `x`.
#' 
#' @import DESeq2
#' @importFrom magrittr extract
#' @importFrom purrr discard keep
#' @importFrom S4Vectors metadata
#' @importFrom stringr str_to_lower
#' 
#' @export
#' 
filter_dds_by_test_type <- function(x, type='Wald') {
  # check that DESeq2 is installed
  if(!requireNamespace(package='DESeq2', quietly=TRUE))
    stop('[filter_dds_by_type] required DESeq2!', call.=FALSE)

  # convert case of type so as not to worry about it
  type %<>% str_to_lower()

  # make sure type is only Wald or LRT
  if(type %>% is_in(c('wald', 'lrt')) %>% not())
    stop('type must be "Wald" or "LRT"!', call.=FALSE)

  # make sure that x is a list
  x %<>%
    class() %>%
    when(.!='list'~list(x=x),
         TRUE~x)

  # make sure that x contains only DESeqDataset objects
  x %>%
    sapply(class) %>%
    equals('DESeqDataSet') %>%
    all() %>%
    when(.==FALSE~stop('not all of x were DESeqDataSet objects!', call.=FALSE))

  # decide whether to keep or discards objects with a `reduced` element in the metadata
  if(type=='wald')
    select_function <- discard
  else if(type=='lrt')
    select_function <- keep

  x %>%
    select_function(function(x) metadata(x) %>% names() %>% is_in(x='reduced')) %>%
    names() %>%
    extract(x, .)
}
