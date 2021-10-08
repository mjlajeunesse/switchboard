#' Display an organic-like geometric spiral pattern as a progress bar.
#'
#' The \code{progress_phyllotaxis} widget displays the growth of geometric
#' spiral pattern as a progress bar. You know, like the way aloe plants grow.
#'
#' @section Usage:
#'    \preformatted{progress_phyllotaxis(eavesdrop, maximum = 100, caption = "",
#'    size = 1, placeOnGrid = c(1, 1), updates = 100, delay = 0, honest = FALSE,
#'    closeAtMaximum = FALSE)}
#'
#' @param eavesdrop The variable to track with a progress bar.
#' @param maximum The maximum value of \code{eavesdrop} that marks the end of
#'    what to progress/track.
#' @param label A small caption/label for the widget.
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
#'
#' @return Nothing.
#'
#' @examples \dontrun{
#'      
#'      for (i in 1:250) {
#'        switchboard(delay = 0.01) %>%
#'          progress_phyllotaxis(i, maximum = 250)
#'      }
#'      switchboard_close()
#'
#' }
#'
#' @family progress bars
#' @name progress_phyllotaxis
NULL


#' @inheritParams progress_phyllotaxis
#' @import tcltk magrittr
#' @export
progress_phyllotaxis <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = progress_phyllotaxis_construct(.switchboard, ...),
                     updater = progress_phyllotaxis_update(.switchboard, ...), ...)
}


#' helper function that constructs canvas items of a progress_phyllotaxis widget
#' @keywords internal
progress_phyllotaxis_construct <- function(.switchboard,
                                           label = " ",
                                           size = 1,
                                           ...) {

  aCanvas <- tcltk::tkcanvas(.switchboard, width = 80 * size, height = 80 * size, background = switchboard.env$mainColors[2], borderwidth = 0, highlightthickness = 0)
  tcltk::tkcreate(aCanvas, "text", 80 * size - 6, 80 * size, text = label, anchor = "se", font = paste(switchboard.env$font, floor(10 * size / 1.2), "bold"), fill = switchboard.env$mainColors[2])
  tcltk::tkcreate(aCanvas, "text", 80 * size - 5, 80 * size, text = label, anchor = "se", font = paste(switchboard.env$font, floor(10 * size / 1.2), "bold"), fill = switchboard.env$mainColors[1])

  return (aCanvas)
}



#' helper function that updates the canvas items of a progress_phyllotaxis widget
#' @keywords internal
progress_phyllotaxis_update <- function(.switchboard,
                                        eavesdrop,
                                        maximum = 100,
                                        size = 1,
                                        updates = 100,
                                        closeAtMaximum = FALSE,
                                        ...) {

  if(eavesdrop %% ceiling(maximum / updates) == 0) {

    aCanvas <- paste0(.switchboard$theSwitchboard, ".", .switchboard$theWidget)
    goldenRatio <- (sqrt(5) + 1)/2
    fibonacciAngle <- 360 / (goldenRatio ^ 2)
    r <- sqrt(animate(eavesdrop/maximum, 1.0)) * 34 * size
    theAngle <- (eavesdrop/maximum) * fibonacciAngle
    theX <- floor(38 * size + cos(theAngle) * r); theY <- floor(43 * size + sin(theAngle) * r);
    aCircle <- tcltk::tkcreate(aCanvas, "oval", theX - 2 * size, theY - 2 * size, theX + 2 * size, theY + 2 * size, width = 0, fill = switchboard.env$rangeColors[80 - ceiling(eavesdrop/maximum * 79)/1.5], tags = "smallCircle")
    tcltk::tkitemlower(aCanvas, aCircle)

  }

  if((closeAtMaximum == TRUE) && (eavesdrop == maximum)) switchboard_close()

}


