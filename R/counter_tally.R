#' A tally counter to keep track of the number of simulation iterations.
#'
#' The \code{counter_tally} widget displays a tally of simulation iterations.
#'
#' @section Usage:
#'    \preformatted{counter_tally(label = "", size = 1, placeOnGrid = c(1, 1))}
#'
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
#'
#' @return Nothing.
#'
#' @examples \dontrun{
#'
#'      for (i in 1:250) {
#'        switchboard(delay = 0.01) %>%
#'          counter_tally(label = "tallying 250")
#'      }
#'      switchboard_close()
#'
#' }
#'
#' @family eavesdroppers
#' @name counter_tally
NULL


#' @inheritParams counter_tally
#' @import tcltk magrittr
#' @export
counter_tally <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = counter_tally_construct(.switchboard, ...),
                     updater = counter_tally_update(.switchboard, ...), ...)
}


#' helper function that constructs canvas items of a counter_tally widget
#' @keywords internal
counter_tally_construct <- function(.switchboard,
                                    label = " ",
                                    size = 1,
                                    ...) {

  aCanvas <- tcltk::tkcanvas(.switchboard, width = 80 * size, height = 80 * size, background = switchboard.env$mainColors[2], borderwidth = 0, highlightthickness = 0)
  tcltk::tkcreate(aCanvas, "text", 40 * size, 37 * size, anchor = "center", text = switchboard.env$totalIterations, font = paste(switchboard.env$font, floor(15 * size / 1.2)), fill = switchboard.env$mainColors[1], tags = "counter")
  tcltk::tkcreate(aCanvas, "text", 40 * size, 48 * size, anchor = "center", text = label, font = paste(switchboard.env$font, floor(10 * size / 1.2)), fill = switchboard.env$mainColors[4])

  return (aCanvas)

}


#' helper function that updates canvas items of a counter_tally widget
#' @keywords internal
counter_tally_update <- function(.switchboard,
                                 ...) {

  aCanvas <- paste0(.switchboard$theSwitchboard, ".", .switchboard$theWidget)
  tcltk::.Tcl(paste(aCanvas, "itemconfigure", "counter", "-text", formatC(switchboard.env$totalIterations, width = 8, format = "d", flag = "0")))
}
