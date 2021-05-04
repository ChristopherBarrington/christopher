#' @export
set_panel_dims <- function(ggplot=NULL, ggplot_gtable=ggplotGrob(ggplot), height, width) {
  if(gtable::is.gtable(ggplot))
      ggplot_gtable <- ggplot

  panels <- grep('panel', ggplot_gtable$layout$name)
  panel_index_w <- unique(ggplot_gtable$layout$l[panels])
  panel_index_h <- unique(ggplot_gtable$layout$t[panels])

  width_ratios <- ggplot_gtable$widths[panel_index_w] %>% as.numeric() %>% (function(x) x/max(x))
  height_ratios <- ggplot_gtable$heights[panel_index_h] %>% as.numeric() %>% (function(x) x/max(x))

  if (getRversion() < '3.3.0')
      stop('[set_panel_dims] R 3.3.0 is required!')

  if (!is.null(width))
      ggplot_gtable$widths[panel_index_w] <- width_ratios * width

  if (!is.null(height))
      ggplot_gtable$heights[panel_index_h] <- height_ratios * height

  invisible(ggplot_gtable)
}

#' @export
show_resized_plot <- function(x) {
  grid::grid.newpage()
  grid::grid.draw(x=x)
}

#' @export
remove_clipping <- function(x) {
  x$layout$clip <- 'off'
  for(i in seq(x$grobs))
    if(!is.null(x$layout$clip))
      x$grobs[[i]]$layout$clip <- 'off'
  x
}
