
#' Write parameters file
#' 
#' Writes an object of parameters to file ready for use in an `sbatch` job array.
#' 
#' @param x `data.frame` or `list` of parameters to be written as a `format` file.
#' @param filename Path to output file.
#' @param format Output file format using `readr` `write_*` functions. Currently only `csv` or `rds`!
#' @param add_id Should an `id` variable be included, which is just a zero-padded row number. Defaults to `true` when `x` is some type of `data.frame`.
#' 
#' @importFrom dplyr mutate n
#' @importFrom purrr pluck when
#' @importFrom readr write_csv write_tsv write_rds
#' @importFrom stringr str_detect str_flatten str_pad str_to_upper
#' 
#' @return
#' Invisibly returns the filename that was written.
#' 
#' @export
#' 
write_parameters_file <- function(x, filename='parameters.csv', format=get_file_extension(filename), add_id=is_in('data.frame', class(x))) {
  # get the function with with a file will be written
  str_c('write_', format) %>%
    get() -> writer

  # add a row number as a column
  if(add_id & !is_in('id', colnames(x)))
    x %<>%
      mutate(id={n() %>% seq() %>% str_pad(width={nchar(.) %>% max()}, pad='0', side='left')},
             .before=1)

  # print sbatch info
  sprintf(fmt='#SBATCH --array 1-%d:1', nrow(x)) %>% message()
  when(format,
       str_detect(., '^csv$') ~ sprintf(fmt="IFS=',' read %s <<< $(awk \"NR==(${SLURM_ARRAY_TASK_ID}+1) {print}\" %s)", {colnames(x) %>% str_flatten(collapse=' ') %>% str_to_upper()}, basename(filename)),
       str_detect(., '^rds$') ~ 'export PARAMETER_SET=${SLURM_ARRAY_TASK_ID}',
       TRUE ~ stop('`format` must be `csv` or `rds`!, call.=FALSE')) %>%
    message()

  # write the contents of x to file
  when(format,
       str_detect(., '^rds$') ~ list(),
       TRUE ~ list(col_names=TRUE)) %>%
    append(values=list(file=filename, x=x)) %T>%
    do.call(what=writer) %>%
    pluck('filename') %>%
    invisible()
}
