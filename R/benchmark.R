#' Benchmark or signal an event or achievement with a giant checkmark.
#'
#' The \code{benchmark} widget displays the number of simulation iterations.
#'
#' @section Usage:
#'    \preformatted{benchmark(eavesdrop = NULL, benchmark = NA, label = "",
#'    size = 1, placeOnGrid = c(1, 1))}
#'
#' @param eavesdrop The variable to track.
#' @param benchmark The numerical value associated with \code{eavesdrop} that
#'    will trigger a visual benchmark on a widget. For example, if \code{eavesdrop}
#'    is a variable that ranges from 1 to 100, then \code{benchmark = 50} will flag
#'    completion at the 50 value.
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
#'    for (i in 1:250) {
#'      switchboard(delay = 0.01) %>%
#'        benchmark(i, benchmark = 125, label = ">125")
#'    }
#'    switchboard_close()
#'
#' }
#'
#' @family eavesdroppers
#' @name benchmark
NULL


#' @inheritParams benchmark
#' @import tcltk magrittr
#' @export
benchmark <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = benchmark_construct(.switchboard, ...),
                     updater = benchmark_update(.switchboard, ...), ...)
}

#' helper function that constructs canvas items of a benchmark widget
#' @keywords internal
benchmark_construct <- function(.switchboard,
                                label = " ",
                                size = 1,
                                ...) {

  aCanvas <- tcltk::tkcanvas(.switchboard, width = 80 * size, height = 80 * size, background = switchboard.env$mainColors[2], borderwidth = 0, highlightthickness = 0)
  tcltk::tkcreate(aCanvas, "text", -11, 96 * size, text = "\U2713", anchor = "sw", font = paste(switchboard.env$font, floor(84 * size / 1.2)), fill = switchboard.env$mainColors[4], tags = "benchmark")
  tcltk::tkcreate(aCanvas, "text", 5 * size, 5 * size, text = label, anchor = "nw", font = paste(switchboard.env$font, floor(12 * size / 1.2)), fill = switchboard.env$mainColors[3])

  return (aCanvas)
}


#' helper function that updates canvas items of a benchmark widget
#' @keywords internal
benchmark_update <- function(.switchboard,
                             eavesdrop,
                             benchmark = NA,
                             ...) {

  aCanvas <- paste0(.switchboard$theSwitchboard, ".", .switchboard$theWidget)
  if(eavesdrop == benchmark) tcltk::tkitemconfigure(aCanvas, "benchmark", "-fill", switchboard.env$mainColors[1])

}
