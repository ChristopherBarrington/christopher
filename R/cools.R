#' @export
#' 
get_submatrix <- function(chrom1, pos1, range1, chrom2=chrom1, pos2=pos1, range2=range1, mcool, resolution, ID) {
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

  get_matrix() %>%
    filter(chrom==filter_chrom1 & start>=filter_start1 & end<=filter_end1) %>%
    filter(chrom2==filter_chrom2 & start2>=filter_start2 & end2<=filter_end2)
}

#' @imports rhdf5
#' 
#' @export
#' 
get_matrix <- function(mcool, resolution) {
  h5read(file=mcool, name=sprintf(fmt='resolutions/%s/bins', resolution)) %>%
    as.data.frame() %>%
    mutate(bin=seq(n())-1) -> bins

  h5read(file=mcool, name=sprintf(fmt='resolutions/%s/pixels', resolution)) %>%
    as.data.frame() %>%
    left_join(y=bins, by=c(bin1_id='bin')) %>%
    left_join(y=bins, by=c(bin2_id='bin'), suffix=c('1','2')) %>%
    mutate(balanced=count*weight1*weight2) %>%
    drop_na() %>%
    mutate_if(is.factor, as.character) %>%
    select(chrom1, start1, end1, chrom2, start2, end2, everything())
}
