#' Set width of console
#' 
#' Uses the width of the console to define the number of characters to print on lines.
#' 
#' @param how_wide Number of characters
#' @param verbose Include message of new console width
#' 
#' @describeIn wideScreen Set width of the console output
#' 
#' @export
#' 
wideScreen <- function(how_wide=get_screen_width()-1, verbose=FALSE) {
  if(verbose) message(sprintf('setting width=%d', how_wide))
  options(width=as.integer(how_wide))
}

#' 
#' @describeIn wideScreen Get width of console
#' 
get_screen_width <- function()
  as.numeric(strsplit(system('stty size', intern=TRUE), ' ')[[1]])[2]

#' Clear the screen
#' 
#' @export
#' 
clear <- function()
  system('clear')

#' Stream output less-like
#' 
#' Scroll and search through long data streams like with \code{bash} \code{less}.
#' 
#' @export
#' 
less <- function(x, ...) {
  when(is_tibble(x)~as.data.frame(x), TRUE~x) %>%
    utils::page(method='p')
}

#' Open X11 on a display
#' 
#' Iterate through displays to find an X11 display to use.
#' 
#' @param display_id Starting display ID
#' @param max_attempts Number of displays to try
#' 
#' @details
#' When reconnecting a \code{tmux} session, the R session may no longer be able to open an X11 window. In such cases, the display ID may have changed between the R session opening and the \code{tmux} session reconnecting. This function iterates over the displays until one can be used or \code{max_attempts} is reached.
#' 
#' @return
#' Invisibly returns the opened display ID.
#' 
#' @export
#' 
open_x11 <- function(display_id=10, max_attempts=10) {
  max_display_id <- display_id + max_attempts
  if(!is.null(dev.list())) {
    message(sprintf('/// x11 device already open!'))
  } else {
    while(is.null(dev.list()) && display_id<max_display_id) {
      suppressWarnings(try({x11(display=sprintf('localhost:%s.0', display_id))}, silent=TRUE))

      device_msg <- sprintf('x11 device: localhost:%s.0', display_id)
      if(!is.null(dev.list())) {
        message(sprintf(fmt='/// %s opened', device_msg))
      } else {
        message(sprintf(fmt='!!! %s failed', device_msg))
      }

      display_id <- display_id+1
    }
  }
  invisible(display_id)
}
