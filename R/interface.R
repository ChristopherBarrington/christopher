wideScreen <- function(howWide=get_screen_width()-1, verbose=FALSE)
{
    if (verbose)
        message(sprintf("! setting width=%d", howWide))
    options(width = as.integer(howWide))
}

get_screen_width <- function()
  as.numeric(strsplit(system('stty size', intern=TRUE), ' ')[[1]])[2]

clear <- function()
  system('clear')

less <- page <- function(x, ...) {
  utils::page(x=x, method='p')
}

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
