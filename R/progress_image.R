#' Display a custom 80 by 80 pixel image as a progress bar.
#'
#' The \code{progress_image} widget displays an image as a progressBar.
#'
#' @section Usage:
#'    \preformatted{progress_image(eavesdrop, maximum = 100, size = 1,
#'    placeOnGrid = c(1, 1), updates = 100, delay = 0, honest = FALSE,
#'    closeAtMaximum = FALSE, fill = "horizontal", file = "")}
#'
#' @param eavesdrop The variable to track with a progress bar.
#' @param maximum The maximum value of \code{eavesdrop} that marks the end of
#'    what to progress track.
#' @param size A number used to designate the size (magnification) of the
#'    widget. The default is set to \code{1} which is 80 by 80 pixels. For
#'    example, setting to \code{3} will results in a widget 3-times the default
#'    size (240 by 240 pixels) and will occupy a grid area of 3 by 3.
#' @param placeOnGrid A row by column coordinate (e.g., \code{c(row-number, column-number)})
#'    of a grid that designates the position to draw the widget on the
#'    \code{switchboard}. Use \code{showGrid()} to help organize widget placement
#'    on dashboard. The default places the first widget in pipe chain to
#'    the \code{c(1, 1)} position, and all following on the same row stacked to
#'    the right.
#' @param updates The number of times the widget is to be updated (e.g., when
#'     it be modified/changed). The default updates the widget 100 times. Increase
#'     number for smoother progress bar animation.
#' @param delay Pause each update of the switchboard. Default has no delay,
#'    values are in seconds (e.g., \code{delay = 0.01} results in 0.01 second
#'    delay with each iteration).
#' @param honest When \code{TRUE}, it updates the widget by the true progression value.
#'     The default (\code{FALSE}) has a cosmetic modification to the progression
#'    value that helps update it in a prettier way.
#' @param closeAtMaximum Functions like \code{switchboard_close()} by closing
#'      the switchboard window when the eavesdropped value equals maximum. NOTE:
#'      if a widget has \code{closeAtMaximum = TRUE}, then this widget MUST
#'      be placed at the end (i.e., last widget) of the pipe chain.
#' @param fill The direction of how things are animated when displaying
#'      progression. The default is \code{horizontal}, which tracks progression
#'      from left to right (maximum), but \code{vertical} can also be used for progress to
#'      occur in a bottom to up (maximum) animation.
#' @param file A *.png filename with a transparent background that designates an
#'      image to be used as a progress-bar. The total image size should be
#'      80 by 80 pixels, but for best integration into switchboard layout, the
#'      actual image must be 75 by 75 pixels placed at the bottom left of the
#'      80 by 80 image. This will leave a 5 pixel whitespace at both the top
#'      and right side of the image.
#'
#' @return Nothing.
#'
#' @examples \dontrun{
#'      
#'      for (i in 1:250) {
#'        switchboard(delay = 0.01) %>%
#'          progress_image(i, maximum = 250, 
#'                         file = system.file("images", "topography.png", package = "switchboard"))
#'      }
#'      switchboard_close()
#'
#' }
#'
#' @family progress bars
#' @name progress_image
NULL


#' @inheritParams progress_image
#' @import tcltk magrittr
#' @export
progress_image <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = progress_image_construct(.switchboard, ...),
                     updater = progress_image_update(.switchboard, ...), ...)
}

#' helper function that constructs canvas items of a progress_image widget
#' @keywords internal
progress_image_construct <- function(.switchboard,
                                     size = 1,
                                     file = system.file("images", "topography.png", package = "switchboard"),
                                     ...) {

  size <- ceiling(size)
  aCanvas <- tcltk::tkcanvas(.switchboard, width = 80 * size, height = 80 * size, background = switchboard.env$mainColors[2], borderwidth = 0, highlightthickness = 0)
  theImage <- tcltk::tcl("image", "create", "photo", file = file)
  zoomFigure <- tcltk::tcl("image", "create", "photo")
  tcltk::tcl(zoomFigure, "copy", theImage, "-zoom", size)
  tcltk::tkcreate(aCanvas, "rectangle", 0, 5, 75 * size, 80 * size, width = 0, fill = switchboard.env$mainColors[4])
  tcltk::tkcreate(aCanvas, "rectangle", 0, 0, 0, 0, width = 0, fill = switchboard.env$mainColors[1], tags = "progressImage")
  tcltk::tkcreate(aCanvas, "image", 40 * size, 40 * size, image = zoomFigure, anchor = "center")

  return(aCanvas)
}


#' helper function that updates the canvas items of a progress_image widget
#' @keywords internal
progress_image_update <- function(.switchboard,
                                  eavesdrop = NULL,
                                  maximum = 100,
                                  size = 1,
                                  updates = maximum,
                                  honest = TRUE,
                                  closeAtMaximum = FALSE,
                                  fill = "horizontal",
                                  ...) {

  if(eavesdrop %% ceiling(maximum / updates) == 0) {
    size <- ceiling(size) #only necessary for image-based progress bars
    aCanvas <- paste0(.switchboard$theSwitchboard, ".", .switchboard$theWidget)
    if(honest == FALSE) percentDisplayed <- animate(eavesdrop/maximum, 1)
    else percentDisplayed <- eavesdrop/maximum
    if(fill == "horizontal") tcltk::.Tcl(paste(aCanvas, "coords", "progressImage", 0, 5, percentDisplayed * 75 * size, 80 * size))
    else tcltk::.Tcl(paste(aCanvas, "coords", "progressImage", 0, 80 * size, 75 * size, 80 * size - percentDisplayed * 75 * size))
  }

  if((closeAtMaximum == TRUE) && (eavesdrop == maximum)) switchboard_close()
}
