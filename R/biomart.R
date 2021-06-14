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
