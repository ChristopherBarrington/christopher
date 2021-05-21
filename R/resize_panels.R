#' Resize and display a `ggplot` object
#' 
#' Accepts a `ggplot` object and converts it to a `gtable` before setting panel dimensions and returning (and showing) the modified `gtable`.
#' 
#' @param x Either a `ggplot` or `gtable`/`pheatmap`
#' @param size Value of both `width` and `height`, if defined
#' @param height,width Numeric values for size of the dimensions. Set to `NULL` to avoid resizing.
#' @param unit Character for units of `width` and `height`, passed to [grid::unit()]
#' @param orientation Character either 'landscape'/'l' or 'portrait'/'p'
#' @param aspect Numeric for aspect ratio to apply
#' @param clip Should clipping be on or off? Set to TRUE or FALSE. Default is FALSE: turn clipping off.
#' 
#' @describeIn resize_and_show Display and resize a `grid` object on new page
#' @seealso [grid::unit()]
#' 
#' @imports grid
#' 
#' @export
#'
resize_and_show <- function(x, size, width, height, unit='in', orientation=c('landscape','portrait'), aspect=1.6, clip=TRUE) {
  # wrangle dimensions
  orientation %<>% head(n=1) %>% str_extract('^.')
  if(!is_in(orientation, c('l','p')))
    stop('(resize_and_show) orientation must be one of "c" or "p"')

  if(missing(size) && missing(width) && missing(height))
    stop('(resize_and_show) at least one of size, height or width must be defined!', call.=FALSE)

  if(!missing(width) & !missing(height) && is.null(width) && is.null(height))
    stop('(resize_and_show) both width and height are NULL; no need to resize?', call.=FALSE)

  if(!missing(size))
    width <- height <- as.numeric(size)
  else
    if(!missing(width) && !is.null(width) && missing(height))
      orientation %>%
        when(.=='l' ~ width/aspect,
             .=='p' ~ width*aspect) -> height
    else if(!missing(height) && !is.null(height) && missing(width))
      orientation %>%
        when(.=='l' ~ height*aspect,
             .=='p' ~ height/aspect) -> width

  if(!is.null(width))
    width %<>% as.numeric() %>% unit(units=unit)
  if(!is.null(height))
    height %<>% as.numeric() %>% unit(units=unit)

  # wrangle x into a gtable
  x %<>%
    when(class(.) %>% is.element(el='ggplot') ~ ggplotGrob(.),
         class(.) %>% is.element(el='pheatmap') ~ pluck(., 'gtable') %>% (function(x) {x$layout$name[x$layout$name=='matrix'] <- 'panel' ; x}),
         class(.) %>% is.element(el='gtable') ~ .,
         TRUE~class(.) %>% str_c(collapse='/') %>% sprintf(fmt='(resize_and_show) do not know what to do when x is a %s') %>% stop(call.=FALSE))

  # resize the panel(s)
  set_panel_dims(ggplot_gtable=x, height=height, width=width) %>%
    set_plot_clipping(clip=clip) %>%
    show_resized_plot()
}

#' Define the dimensions of a `gtable` panel
#' 
#' @param ggplot Input `ggplot` object
#' @param ggplot_gtable Input `gtable` object
#' @param height,width For set_panel_dims: Dimensions in [grid::unit()]
#' 
#' @describeIn resize_and_show Alter dimensions of plot area
#' 
set_panel_dims <- function(ggplot=NULL, ggplot_gtable=ggplotGrob(ggplot), height=width, width=height) {
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

#' Prevent clipping in `grid` objects
#' 
#' @param x `grid` object
#' @param clip Should clipping be on or off? Set to TRUE or FALSE. Default is FALSE: turn clipping off.
#' 
#' @describeIn resize_and_show Turn clipping off for all grobs in a `grid` object
#' 
set_plot_clipping <- function(x, clip=TRUE) {
  clip %<>% if_else('on', 'off')

  x$layout$clip <- clip
  for(i in seq(x$grobs))
    if(!is.null(x$layout$clip))
      x$grobs[[i]]$layout$clip <- clip
  x
}

#' Display a `grid` object
#' 
#' @param x `grid` object
#' 
#' @describeIn resize_and_show Display `grid` object on new page
#' 
show_resized_plot <- function(x) {
  grid::grid.newpage()
  grid::grid.draw(x=x)
}

