#' Display percent-complete along with an event benchmark.
#'
#' The \code{progress_benchmark} widget displays a progress bar with a percent
#' complete and a triggerable benchmark as a giant checkmark.
#'
#' @section Usage:
#'    \preformatted{progress_benchmark(eavesdrop, maximum = 100, benchmark = NA,
#'    caption = "", size = 1, placeOnGrid = c(1, 1), updates = 100, delay = 0,
#'    honest = FALSE, closeAtMaximum = FALSE)}
#'
#' @param eavesdrop The variable to track with a progress bar.
#' @param maximum The maximum value of \code{eavesdrop} that marks the end of
#'    what to progress track.
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
#'          progress_benchmark(i, maximum = 250, benchmark = 125)
#'      }
#'      switchboard_close()
#'
#' }
#'
#' @family progress bars
#' @family eavesdroppers
#' @name progress_benchmark
NULL


#' @inheritParams progress_benchmark
#' @import tcltk magrittr
#' @export
progress_benchmark <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = progress_benchmark_construct(.switchboard, ...),
                     updater = progress_benchmark_update(.switchboard, ...), ...)
}


#' helper function that constructs canvas items of a progress_benchmark widget
#' @keywords internal
progress_benchmark_construct <- function(.switchboard,
                                         size = 1,
                                         ...) {

  aCanvas <- tcltk::tkcanvas(.switchboard, width = 80 * size, height = 80 * size, background = switchboard.env$mainColors[2], borderwidth = 0, highlightthickness = 0)
  tcltk::tkcreate(aCanvas, "text", -11, 96 * size, text = "\U2713", anchor = "sw", font = paste(switchboard.env$font, floor(84 * size / 1.2)), fill = switchboard.env$mainColors[4], tags = "benchmark")
  tcltk::tkcreate(aCanvas, "text", 35 * size, 3 * size, text = 0, anchor = "ne", font = paste(switchboard.env$font, floor(20 * size / 1.2)), fill = switchboard.env$mainColors[3], tags = "percent")
  tcltk::tkcreate(aCanvas, "text", 45 * size, 10 * size + 1 * size, text = "%", anchor = "ne", font = paste(switchboard.env$font, floor(10 * size / 1.2)), fill = switchboard.env$mainColors[3])

  return (aCanvas)
}

#' helper function that updates the canvas items of a progress_benchmark widget
#' @keywords internal
progress_benchmark_update <- function(.switchboard,
                                      eavesdrop = c(NULL, NULL),
                                      maximum = c(" ", " "),
                                      updates = 100,
                                      honest = FALSE,
                                      closeAtMaximum = FALSE,
                                      benchmark = NA,
                                      ...) {

  aCanvas <- paste0(.switchboard$theSwitchboard, ".", .switchboard$theWidget)
  if(eavesdrop %% ceiling(maximum / updates) == 0) {
    if(honest == FALSE) percentDisplayed <- animate(eavesdrop/maximum, 1.0)
    else percentDisplayed <- eavesdrop/maximum
    tcltk::tkitemconfigure(aCanvas, "percent", "-text", ceiling(percentDisplayed * 100))
  }
  if(eavesdrop == benchmark) tcltk::tkitemconfigure(aCanvas, "benchmark", "-fill", switchboard.env$mainColors[1])
  if((closeAtMaximum == TRUE) && (eavesdrop == maximum)) switchboard_close()

}


