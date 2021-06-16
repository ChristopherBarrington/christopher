#' Retrieve Ensembl archive URL
#' 
#' Get a URL to Ensembl archive server for a specific Ensemble release.
#' 
#' @param release Ensembl release number, `character` or `integer`
#' 
#' @import magrittr
#' @importFrom biomaRt listEnsemblArchives
#' @importFrom dplyr select
#' @importFrom tibble deframe
#' @importFrom purrr pluck
#' 
ensembl_release_to_archive <- function(release) {
  if(missing(release))
    stop('[ensembl_release_to_archive] requires the database release (eg 98) to be specified!')
  release %<>% as.character()
  listEnsemblArchives() %>% dplyr::select(version, url) %>% deframe() %>% pluck(release)
}

#' Create a biomaRt connection
#' 
#' Connect to an Ensembl release server, providing species and release to get a `mart` object.
#' 
#' @param species The organism name eg 'mmusulus'
#' @param release Ensembl release version
#' @param dataset Ensembl dataset to connect
#' 
#' @importFrom stringr str_c
#' @importFrom biomaRt useMart useDataset
#' 
#' @export
#' 
get_mart <- function(species='mmusculus', release=95, dataset='gene_ensembl', ...) {
  # project <- system.file('extdata/project.yml', package='projectData') %>% yaml::read_yaml()
  ensembl_archive <- ensembl_release_to_archive(release=release)

  ensembl_dataset <- str_c(species, dataset, sep='_')
  sprintf(fmt='[get_mart] making mart for %s using release-%s %s', species, release, ensembl_dataset) %>% message()
  mart <- useMart(biomart='ensembl', host=ensembl_archive)
  mart <- useDataset(dataset=ensembl_dataset, mart=mart)

  mart
}

#' Format a URL to Ensembl
#' 
#' Uses `mart` object to produce a URL to a specific feature in Ensembl.
#' 
#' @param ensembl_gene_id Ensembl gene identifier for which a URL is created
#' @param mart The (optional) `biomaRt` connection object. If omitted a URL to search the current Ensembl release is produced.
#' 
#' @import magrittr
#' @importFrom dplyr mutate select
#' @importFrom stringr str_remove
#' @importFrom urltools url_compose url_parse
#' 
#' @export
#' 
get_ensembl_url <- function(ensembl_gene_id, mart) {

  # https://www.ensembl.org/Multi/Search/Results?q=ENSMUSG00000004591
  # https://www.ensembl.org/Mus_musculus/Gene/Summary?g=ENSMUSG00000004591

  if(missing(ensembl_gene_id))
    stop('!!! get_ensembl_url must have an ensembl_gene_id')

  if(missing(mart)) {
    url_parse(url='https://www.ensembl.org/') %>%
      mutate(port=NA,
             path='Multi/Search/Results',
             parameter=sprintf(fmt='q=%s', ensembl_gene_id),
             fragment=NA) -> parsed_url
  } else if(class(mart)=='Mart') {
    str_remove(mart@dataset, '_.*$') %>%
      switch(hsapiens='Homo_sapiens',
             mdomestica='Monodelphis_domestica',
             mmusculus='Mus_musculus',
             scerevisiae='Saccharomyces_cerevisiae') -> species
    
    url_parse(url=mart@host) %>%
      mutate(port=NA,
             path=sprintf(fmt='%s/Gene/Summary', species),
             parameter=sprintf(fmt='g=%s', ensembl_gene_id),
             fragment=NA) -> parsed_url
  } else {
    stop('!!! get_ensembl_url does not know what to do with your mart')
  }

  url_compose(parsed_urls=parsed_url)
}
