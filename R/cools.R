#' Read data from a cool (or mcool) file
#' 
#' Reads the cool h5 file to provide the whole interaction matrix or a subset of coordinates.
#' 
#' @param cool,mcool Path to the h5 file
#' @param resolution Bin size in the \code{mcool} to extract
#' 
#' @details
#' The \code{cool} path takes priority and if \code{mcool} is provided so too must \code{resolution}.
#' 
#' @return
#' A \code{data.frame} of 2D genomic coordinates with their weights and balanced score.
#' 
#' @describeIn get_cool_interaction_matrix Get a genome-wide interaction matrix
#' 
#' @import rhdf5
#' 
#' @export
#' 
get_cool_interaction_matrix <- function(cool, mcool, resolution) {

  if(!missing(mcool) & !missing(resolution))
    uri <- sprintf(fmt='%s::resolutions/%s', mcool, resolution)
  else
    uri <- cool

  h5read(file=uri, name='bins') %>%
    as.data.frame() %>%
    mutate(bin=seq(n())-1) -> bins

  h5read(file=uri, name='pixels') %>%
    as.data.frame() %>%
    left_join(y=bins, by=c(bin1_id='bin')) %>%
    left_join(y=bins, by=c(bin2_id='bin'), suffix=c('1','2')) %>%
    mutate(balanced=count*weight1*weight2) %>%
    drop_na() %>%
    mutate_if(is.factor, as.character) %>%
    select(chrom1, start1, end1, chrom2, start2, end2, everything())
}

#' 
#' @param chrom1,chrom2 Chromosomes for intervals
#' @param pos1,pos2 Start coordinate of intervals
#' @param range1,range2 Size of intervals (base pairs)
#' @param mcool,resolution Path to h5 file and resolution to extract
#' 
#' @details
#' Coordinates are filtered from the whole matrix via \code{get_cool_interaction_matrix()}.
#' 
#' @describeIn get_cool_interaction_matrix Get an interaction matrix for a pair of genomic regions
#' 
#' @export
#' 
get_cool_interaction_submatrix <- function(chrom1, pos1, range1, chrom2=chrom1, pos2=pos1, range2=range1, mcool, resolution) {
  if(any(missing(chrom1), missing(pos1), missing(range1), missing(mcool), missing(resolution)))
    stop('!!! missing input parameter(s)')

  filter_chrom1 <- chrom1 %>% as.character()
  filter_chrom2 <- chrom2 %>% as.character()
  floor_pos1 <- pos1 %>% divide_by(resolution) %>% floor() %>% multiply_by(resolution)
  floor_pos2 <- pos2 %>% divide_by(resolution) %>% floor() %>% multiply_by(resolution)
  filter_start1 <- floor_pos1 %>% subtract(range1)
  filter_start2 <- floor_pos2 %>% subtract(range2)
  filter_end1 <- floor_pos1 %>% add(range1) %>% add(resolution)
  filter_end2 <- floor_pos2 %>% add(range2) %>% add(resolution)

  get_matrix(mcool=mcool, resolution=resolution) %>%
    filter(chrom==filter_chrom1 & start>=filter_start1 & end<=filter_end1) %>%
    filter(chrom2==filter_chrom2 & start2>=filter_start2 & end2<=filter_end2)
}

#' Get the available resolutions of an mcool
#' 
#' @param mcool Path to the mcool
#' 
#' @export
#' 
list_mcool_resolutions <- function(mcool) {
  rhdf5::h5ls(file=mcool, recursive=2) %>%
    filter(group=='/resolutions') %>%
    pluck('name') %>%
    gtools::mixedsort()
}
