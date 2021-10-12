#' Read and parse a .babs file
#' 
#' @importFrom datarepository system.file
#' @importFrom purrr pluck
#' @importFrom purrr map_at
#' @importFrom purrr when
#' @importFrom readr read_lines
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_split
#' @importFrom yaml read_yaml
#' 
#' @export
#' 
read_dotbabs <- function()
  get_project_root() %>%
    file.path('.babs') %>%
    when(file.exists(.)~.,
         TRUE~{sprintf(fmt='no .babs file found at: %s', .) %>% stop(call.=FALSE)}) %>%
    read_lines() %>%
    str_replace_all(pattern='\\{.+\\}', replacement='unspecified') %>%
    read_yaml(text=.) %>%
    pluck(1) %>%
    map_at('Scientist', str_remove, pattern='@crick.ac.uk')

get_project_root <- function()
  getwd() %>%
    str_split('/') %>%
    unlist() %>%
    head(n=10) %>%
    str_c(collapse='/')

#' Read files from my MSigDB cache
#' 
#' Reads an MSigDB `gmt` file and returns a list of genes in pathways.
#' 
#' @param collection Name of the collection
#' @param pathways Character vector of pathways in `collection` to keep
#' @param version Release version
#' @param identifier Identifier type: `symbols` or `entrez` (`entrez` may not exist!!)
#' @param dbroot Path to the root of the MSigDB files
#' 
#' @return
#' A list of genes in pathways, with pathway name as the key and a character vector of gene identifiers (`identifer`).
#' 
#' @importFrom fs path
#' @importFrom dplyr select
#' @importFrom plyr dlply
#' @importFrom purrr when
#' @importFrom stringr str_c str_subset
#' @importFrom utils count.fields

#' @export
#' 
read_msigdb <- function(collection='h.all', pathways=NULL, version='7.4', identifier='symbols', dbroot='/camp/stp/babs/working/barrinc/db/msigdb') {

  sprintf(fmt='%s/%s.v%s.%s.gmt',dbroot,  collection, version, identifier) %>%
    when(!file.exists(.)~stop('MSigDB gmt file does not exist!', call.=FALSE),
         TRUE~.) %>%
    {list(path=., ncol={count.fields(., sep='\t') %>% max()})} %>%
    {read.table(file=.$path, sep='\t', fill=TRUE, header=FALSE, col.names=c('pathway', 'url', str_c('id.', 1:.$ncol)))} %>%
    dlply(~pathway, function(x)
      select(x, starts_with('id.')) %>%
        unlist(use.names=FALSE) %>%
        na.omit() %>%
        str_subset('^$', negate=TRUE)) %>%
    when(!is.null(pathways)~.[pathways],
         TRUE~.[names(.)])
}
