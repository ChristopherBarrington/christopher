#' Read and parse a .babs file
#' 
#' @importFrom datarepository system.file
#' @importFrom yaml read_yaml
#' @importFrom purrr pluck
#' @importFrom purrr map_at
#' @importFrom purrr when
#' @importFrom stringr str_replace_all
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
