#' Display the value of four continuous variables in a simulation.
#'
#' The \code{number_quartet} widget displays the numerical value of four continuous
#' variables.
#'
#' @section Usage:
#'    \preformatted{number_quartet(eavesdrop = c(NULL, NULL, NULL, NULL),
#'    digits = 5, updates = 1, label = c("", "", "", ""), size = 1,
#'    placeOnGrid = c(1, 1))}
#'
#' @param eavesdrop A vector of four variables to track.
#' @param digits The number of digits to display in a widget.
#' @param label A vector of four small captions/labels for the widget.
#' @param updates The number of times the widget is to be updated (e.g., when
#'     it be modified/changed). The default updates with each iteration.
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
#'          number_quartet(c(i, i/2, i/3, i /4), label = c("i", "i/2", "i/3", "i/4"))
#'      }
#'      switchboard_close()
#'
#' }
#'
#' @family eavesdroppers
#' @name number_quartet
NULL


#' @inheritParams number_quartet
#' @import tcltk magrittr
#' @export
number_quartet <- function(.switchboard, ...) {
  switchboard_engine(.switchboard,
                     constructor = number_quartet_construct(.switchboard, ...),
                     updater = number_quartet_update(.switchboard, ...), ...)
}

#' helper function that constructs canvas items of a number_quartet widget
#' @keywords internal
number_quartet_construct <- function(.switchboard,
                                     eavesdrop = c(NULL, NULL, NULL, NULL),
                                     size = 1,
                                     digits = 5,
                                     label = c(" ", " ", " ", " "),
                                     ...) {

  aCanvas <- tcltk::tkcanvas(.switchboard, width = 80 * size, height = 80 * size, background = switchboard.env$mainColors[2], borderwidth = 0, highlightthickness = 0)
  tcltk::tkcreate(aCanvas, "text", 0, -2, text = signif(eavesdrop[1], digits), anchor = "nw", font = paste("courier", floor(14 * size / 1.2), "bold"), fill = switchboard.env$mainColors[4], tags = "number1")
  tcltk::tkcreate(aCanvas, "text", 0, 9 * size, text = label[1], anchor = "nw",  font = paste(switchboard.env$font, floor(10 * size / 1.2)), fill = switchboard.env$mainColors[4])
  tcltk::tkcreate(aCanvas, "text", 80 * size, 42 * size -  6 * size, text = signif(eavesdrop[2], digits), anchor = "se", font = paste("courier", floor(14 * size / 1.2), "bold"), fill = switchboard.env$mainColors[3], tags = "number2")
  tcltk::tkcreate(aCanvas, "text", 80 * size, 43 * size, text = label[2], anchor = "se",  font = paste(switchboard.env$font, floor(10 * size / 1.2)), fill = switchboard.env$mainColors[3])
  tcltk::tkcreate(aCanvas, "text", 0, 38 * size, text = signif(eavesdrop[3], digits), anchor = "nw", font = paste("courier", floor(14 * size / 1.2), "bold"), fill = switchboard.env$mainColors[4], tags = "number3")
  tcltk::tkcreate(aCanvas, "text", 0, 42 * size + 7 * size, text = label[3], anchor = "nw",  font = paste(switchboard.env$font, floor(10 * size / 1.2)), fill = switchboard.env$mainColors[4])
  tcltk::tkcreate(aCanvas, "text", 80 * size, 82 * size -  6 * size, text = signif(eavesdrop[4], digits), anchor = "se", font = paste("courier", floor(14 * size / 1.2), "bold"), fill = switchboard.env$mainColors[3], tags = "number4")
  tcltk::tkcreate(aCanvas, "text", 80 * size, 82 * size, text = label[4], anchor = "se",  font = paste(switchboard.env$font, floor(10 * size / 1.2)), fill = switchboard.env$mainColors[3])

  return (aCanvas)
}


#' helper function that updates canvas items of a number_quartet widget
#' @keywords internal
number_quartet_update <- function(.switchboard,
                                  eavesdrop = c(NULL, NULL, NULL),
                                  digits = 5,
                                  updates = 1,
                                  ...) {

  if(switchboard.env$totalIterations %% updates == 0) {
    aCanvas <- paste0(.switchboard$theSwitchboard, ".", .switchboard$theWidget)
    tcltk::.Tcl(paste(aCanvas, "itemconfigure", "number1", "-text", signif(eavesdrop[1], digits)))
    tcltk::.Tcl(paste(aCanvas, "itemconfigure", "number2", "-text", signif(eavesdrop[2], digits)))
    tcltk::.Tcl(paste(aCanvas, "itemconfigure", "number3", "-text", signif(eavesdrop[3], digits)))
    tcltk::.Tcl(paste(aCanvas, "itemconfigure", "number4", "-text", signif(eavesdrop[4], digits)))
  }

}
