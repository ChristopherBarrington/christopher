
#' Write parameters file
#' 
#' Writes a `data.frame` as a `csv` ready to be ready by an `sbatch` job array.
#' 
#' @param x `data.frame` of parameters
#' @param filename Path to output file
#' @param add_id Should an `id` variable be included, which is just a zero-padded row number
#' 
#' @importFrom dplyr mutate n
#' @importFrom readr write_csv
#' @importFrom stringr str_flatten str_pad str_to_upper
#' 
#' @export
#' 
write_parameters_file <- function(x, filename='parameters.csv', add_id=TRUE) {
  # get the function with with a file will be written
  fmt <- 'csv' # maybe make this an argument?
  write_fmt <- get('write_csv')
  # if(missing(fmt) %>% not())
  #   filename %<>% fs::path(ext=fmt)

  # add a row number as a column
  if(add_id)
    x %<>%
      mutate(id={n() %>% seq() %>% str_pad(width={nchar(.) %>% max()}, pad='0', side='left')},
             .before=1)

  # print sbatch info
  sprintf(fmt='#SBATCH --array 1-%d:1', nrow(x)) %>% message()
  sprintf(fmt="IFS=',' read %s <<< $(awk \"NR==${SLURM_ARRAY_TASK_ID}{print}\" %s)", {colnames(x) %>% str_flatten(collapse=' ') %>% str_to_upper()}, basename(filename)) %>% message()

  # write the contents of x to file
  write_fmt(x=x, file=filename, col_names=FALSE)

  invisible(filename)
}
