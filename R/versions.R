
#' Collect software versions
#' 
#' Collect the version of software and packages loaded for the R session
#' 
#' @param conda_envs Names of `conda` environments to query as a named list. The assumption is that the `conda` environment(s) actually exist!
#' @param packages Should versions of the currently loaded packages be included?
#' 
#' @importFrom devtools session_info
#' @importFrom dplyr select
#' @importFrom plyr llply
#' @importFrom purrr pluck set_names when
#' @importFrom rjson fromJSON
#' @importFrom stringr str_c
#' @importFrom tibble deframe
#' 
#' @examples
#' list(r=Sys.getenv('CONDA_DEFAULT_ENV'),
#'      project=read_dotbabs()$Lims) %>%
#' collect_software_versions(packages=FALSE)
#' 
#' @export
#' 
collect_software_versions <- function(conda_envs, packages=TRUE) {
  versions_list <- list()

  # get package versions
  if(packages)
    session_info() %>%
      pluck('packages') %>%
      as.data.frame() %>%
      select(package, loadedversion) %>%
      deframe() -> versions_list$packages

  # get conda-installed software versions
  if(!missing(conda_envs))
    conda_envs %>%
      when(is.list(.)~conda_envs,
           TRUE~stop('conda_envs is not a list!', call.=FALSE)) %>%
      llply(.parallel=TRUE, function(env)
        sprintf(fmt='conda list --json --name %s', env) %>%
          system(intern=TRUE) %>%
          str_c(collapse='') %>%
          fromJSON() %>%
          (function(x) sapply(x, pluck, 'name') %>% purrr::set_names(x=x)) %>%
          llply(pluck, 'version')) %>%
      append(versions_list) -> versions_list

  versions_list
}
